#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This applet was designed by Krissi Alari as apart of her senior capstone at
# Cal State Monterey Bay. This applet was designed specifically for E. coli
# testing in lettuce for a particular food testing company. The re-paramterization
# option of risk levels is only applicable for their testing purposes. The rest of
# the applet (using "Average (μ)" as the parameter of interest) is suitable for 
# general use of the SPRT.
#
# This version of the applet is designed for binary testing (Bernoulli outcomes)
# where X = 1 indicates a defective observation and X = 0 otherwise


library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Sequential Probability Ratio Test for Binary Data"),

    # Sidebar with inputs for numeric values and 
    sidebarLayout(
        sidebarPanel(
            
            textInput( inputId = "data.receive", 
                       label="Input your sample data points separated by commas. Data points should be binary (0s and 1s).",
                       value="1, 0, 1, 0, 0, 1"),
            
            numericInput( inputId = "alpha",
                          label="Input your significance level, α.", 
                          value=0.01),
            
            numericInput( inputId = "beta",
                          label="Input your value for β such that (1-β) is your desired power.", 
                          value=0.01),
            
            numericInput( inputId="p0",
                          label="What is your null value, p0?", 
                          value = 0.5),
            
            numericInput( inputId="p1",
                          label="What is your alternative value, p1? Please set p1 < p0.", 
                          value=0.2),

            submitButton( "Submit", icon("refresh") )
        ),

        # Output
        mainPanel(
            ## Allows the barplot and SPRT plot to be shown side by side
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("barplot"), plotOutput("sprtPlot")),
            br(), ## adds some space between outputs
            htmlOutput("headingtext"),  ## For text output
            htmlOutput("sprtRslt"), ## For text output
            br()
        )
    )
))
