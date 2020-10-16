#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(reshape2)
library(DT)
library(grDevices)
library(ggthemes)
library(zip)

source('code/05_renderFigures.R')
# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # Theme
        theme <- shinytheme('lumen'),
        # App title
        navbarPage("Fiscal Impact Measure",
                   sidebarLayout(
                       slidebarPanel(
                           sliderInput(
                             "bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30   
                           )
                       )
                       
                    )
                   )
    )
)


    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))


