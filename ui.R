library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("AnaLiTix!"),
 # tab set has a set of tabs to be displayed with functions to be executed in each tab
      tabsetPanel(
        tabPanel("Home", h4("Welcome to AnaLiTix!"), h4("AnaLiTix tries to analyse the time and language usage patterns of a person.")),
        
        tabPanel("Input", textInput("nameattrib", label = h4("Facebook Name of friend:"),value = ""),
                 actionButton("go", "Go !"), includeHTML("xz.html")), 
        
        
        tabPanel("Plots", plotOutput("Plot1"), plotOutput("Plot2"),plotOutput("Plot3"),plotOutput("Plot4")),
        
        tabPanel("Sentiment Analysis",plotOutput("sentanal"), plotOutput("binsent")),
        
        tabPanel("Posts", h3(" "),h4("Most Liked Status of the User:"), tableOutput("mostliked")),
        
        tabPanel("Words",plotOutput("WC"), plotOutput("sentcloud"))
      )

))
