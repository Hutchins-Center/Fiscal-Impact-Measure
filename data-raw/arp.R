## code to prepare `arp` dataset goes here
arp_annual <- read_xlsx('data/pandemic-legislation/american_rescue_plan.xlsx',
                        range = 'A3:AV14') %>% 
  janitor::clean_names()

arp_timing <- read_xlsx('data/pandemic-legislation/american_rescue_plan.xlsx', sheet = 'Timing Assumptions',
                        range = 'A2:I22') %>% 
  mutate(date = yearquarter(date)) %>% 
  as_tsibble(index = date)

arp <-
  arp_annual %>% 
  as_tsibble(index = date) %>% 
  annual_to_quarter() %>% 
  summarize(rebate_checks,
            other_direct_aid = child_tax_credit + eitc + child_care_for_workers + dependent_care_for_families,
            health_grants = medicaid + medicare + chip,
            non_health_grants = coronavirus_relief_fund +
              human_services_and_community_supports_minus_childcare_policies + provider_relief + title_9_other + education + covid_containment_vaccination + other_state_and_local + grants_to_tribal_governments + commerce_and_science_federal_spending +  other_transportation + child_care_and_development_block_grant_program + va +  mental_health + medical_supplies + homeland_security_grants + environment_grants + foreign_aid + miscellaneous_from_t9,
            other_vulnerable = housing_assistance + food + emergency_assistance + cobra + premium_tax_credits + ratepayer_protection + assistance_for_older_americans,
            state_ui,
            federal_ui = pua + puc + peuc + ui_tax_suspension + other_ui,
            aid_to_small_businesses = ppp + child_care_stabilization + grants_to_small_businesses + small_business_credit_initiative + paid_sick_leave + employee_retention + pensions + transit_and_aviation_support) %>% 
  left_join(arp_timing, by = 'date') %>% 
  summarize(rebate_checks_arp = 4 * rebate_checks * rebate_checks_timing,
            other_direct_aid = 4 *other_direct_aid * other_direct_aid_timing,
            health_grants_arp = 4 * health_grants * health_grants_timing,
            non_health_grants = 4 * non_health_grants * grants_timing, 
            other_vulnerable = 4 * other_vulnerable * other_vulnerable_timing, 
            federal_ui_arp = 4 * federal_ui * ui_timing,
            state_ui_arp = 4 * state_ui * ui_timing,
            aid_to_small_businesses = 4 * aid_to_small_businesses * aid_to_small_businesses_timing)


openxlsx::write.xlsx(arp, 'data/pandemic-legislation/arp_summary.xlsx')
