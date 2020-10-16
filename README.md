
![Logo of the project](https://github.com/Hutchins-Center/Fiscal-Impact-Measure/blob/master/images/HC_NEW_BROOKINGS_RGB.jpg)

# Fiscal Impact Measure
[![standard-readme compliant](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)

> Hutchins Center

The Hutchins Center Fiscal Impact Measure shows how much local, state, and federal tax and spending policy adds to or subtracts from overall economic growth, and provides a near-term forecast of fiscal policies’ effects on economic activity.

Click [here](https://www.brookings.edu/interactives/hutchins-center-fiscal-impact-measure/) to see the most recent version of the FIM.

## Motivation

The Fiscal Impact Measure (FIM) is a tool created the Hutchins Center to illustrate how much local, state and federal fiscal policy adds to or subtracts from overall economic growth. When the FIM is positive, policy is expansionary in the sense that it is pushing growth in real Gross Domestic Product (GDP) above its longer-run potential. When the FIM is negative, policy is lowering real GDP growth relative to potential. The FIM is broader than measures of fiscal impetus that rely on the size of the federal deficit because it includes the includes the direct effects of federal, state, and local government purchases as well as the more indirect effects of government taxes and government transfers, which affect private consumption.

## Methodology

The Hutchins Center FIM uses quarterly data from the U.S. Commerce Department’s Bureau of Economic Analysis (BEA) to measure the contribution of fiscal policy to GDP growth.  For a detailed derivation of the FIM components, please see the attached document.

The FIM for purchases is defined as the actual contribution of real government purchases to GDP less the contribution that would have prevailed if real purchases were growing with potential GDP. To calculate the actual contribution of real purchases to GDP, we take the growth rate of real government purchases times their share of GDP; for the counterfactual we perform the same calculation but use the growth rate of real potential GDP.

Likewise, the FIM for taxes and transfers is measured as the contribution of consumption to real GDP growth given actual net taxes (taxes less transfers) less the contribution that would have occurred had net taxes been rising with potential GDP. Calculating the contribution of taxes and transfers to consumption requires a number of assumptions. In particular, we make an assumption about the magnitude of the effect of a change in taxes and transfers on consumption (the MPC, or the marginal propensity to consume) and the timing of the effects (consumers may not respond instantaneously to changes in taxes or benefits). We then calculate what consumption growth would have been in the hypothetical scenario in which taxes and transfers rise with potential GDP—the counterfactual net taxes. The difference between that growth rate and the actual growth rate represents the share of consumption growth attributable to fiscal policy. To get an estimate of the contribution to GDP growth, we multiply the contribution to consumption growth by the nominal ratio of consumption to GDP.

Assumed Marginal Propensities to Consume: Our assumptions about marginal propensities to consume (MPCs) are similar to those discussed in Cashin et al. (2017). For transfers, we assume that the MPC is 0.9, reflecting the fact that some transfers—like Medicare and Medicaid—are likely to increase spending one for one, and others, like temporary changes in unemployment benefits, are received by liquidity-constrained households that are likely to spend most of their benefits. For individual taxes, we assign an MPC of 0.6. The MPC out of a corporate tax cut is much smaller, since most corporate equity is held by high-income individuals and pension plans, who are less likely to be liquidity constrained. Part of the effect may also be viewed as a responsiveness of investment to corporate tax changes. Cashin et al. (2017) note that changes in MPC do not have a large effect on their fiscal stance measure

Click [here](https://www.brookings.edu/research/the-hutchins-centers-fiscal-impact-measure/) for more information and [here](https://www.brookings.edu/interactives/hutchins-center-fiscal-impact-measure/) for a detailed derivation of the FIM. 
### Built with
* [R](https://www.r-project.org/)
* [R Studio](https://rstudio.com/)


## Contributing

If you'd like to contribute, please fork the repository and use a feature
branch. Pull requests are warmly welcome.


## Links

Relevant links for using this code:
- Repository: https://github.com/Hutchins-Center/Fiscal-Impact-Measure
- Issue tracker: https://github.com/Hutchins-Center/Fiscal-Impact-Measure/issues
  - In case of sensitive bugs, please contact malcalakovalski@brookings.edu
    directly instead of using issue tracker. We value your effort
    to improve this project!
- Relavant resources for understanding and extending code:
  - Version control: https://www.sas.upenn.edu/~jesusfv/Chapter_HPC_5_Git.pdf
                     https://swcarpentry.github.io/git-novice/
  - Metaprogramming: https://adv-r.hadley.nz/#
  - Programming and Non-Standard Evaluation with dplyr: https://shipt.tech/https-shipt-tech-advanced-programming-and-non-standard-evaluation-with-dplyr-e043f89deb3d
- Related literature:
  -  Fiscal effect measure: https://www.federalreserve.gov/econres/feds/files/2017061pap.pdf


## License

The code in this project is licensed under Brookings Institution license.

Copyright [2020] [The Brookings Institution]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0
