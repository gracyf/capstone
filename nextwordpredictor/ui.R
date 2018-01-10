#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Simple Next Word Predictor App - A Prototype"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        em("Enter your sentence and you can get atmost three predicted words listed below."),
                        h3("Input Text"),
                        textInput("textEntered", "Enter Text:", "Happy birthday"),
                        
                        h3("The Predicted words:"),
                        span(textOutput("nextWord"), style="text-align:center;color:darkblue;font-size:150%")
                        
                        
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        
                        #h3("The Predicted words:"),
                        #span(textOutput("nextWord"), style ="color:red"),
                        
                        h3("The Sentences:"),
                        span(textOutput("sentence"), style="text-align:left;color:black;font-size:120%")
                )
        )
))
