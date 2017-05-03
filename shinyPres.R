library(shiny)
ui <- fluidPage(
  selectInput(inputId= "num", # unlike the first example of a histogram of random numbers, we can use a drop-down menu rather than a slider. Easy fix by replacing sliderInput with selectInput and change values to choices
              label = "Choose a number",
              choices = c(10,20,30,40,50,60,70,80,90,100)), # choices reflect what values the user can select
  plotOutput("hist"))

server <- function(input, output) {
  output$hist <- renderPlot({
    title <- "random normal values"
    hist(rnorm(input$num), main= title)})
}

shinyApp(ui = ui, server = server)

#We can also also have Shiny apps with multiple inputs and/or outputs:
  library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title",  # textInput allows the user to write something that is incorporated into the output, in this case, the user can add a title
            label = "Write a title",
            value = "Histogram of Random Normal Values"), # value signifies what the starting value will be
  actionButton(inputId = "button", label = "Update"), # adding an action button allows the user to manipulate the inputs and have the app only react when you select the action button
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    input$button # specify that there is an action button so the code knows now to update when inputs are changed until the action button is selected
    hist <- isolate(hist(rnorm(input$num), main = input$title)) # we can use the input element we created in the ui and call that in the code to generate a title
  })
}

shinyApp(ui = ui, server = server)


# HWE

library(shiny)
# using sliderInput to allow the user to select the observed counts for each genotype
ui <- fluidPage(sliderInput(inputId = "AA", 
                            label = "Choose a number of observed AA individuals", # this is what will be displayed above the slider
                            value = 500, min = 0, max = 1000),
                sliderInput(inputId = "AB", 
                            label = "Choose a number of observed AB individuals", 
                            value = 500, min = 0, max = 1000),
                sliderInput(inputId = "BB", 
                            label = "Choose a number of observed BB individuals", 
                            value = 500, min = 0, max = 1000),
                textOutput("signif")) # the output will be a direct copy of the output from the Chi-Squared test stored in signif

server <- function(input, output) {
  
  observe({ # observe is an observer function that tells the server to rerun the entire script when input values are changed
    N <-sum(input$AA,input$AB,input$BB) # calculating the total population size
    p <- (input$AA+(0.5*input$AB))/N # generating p and q frequencies
    q <- 1 - p
    ObsN <- c(input$AA,input$AB,input$BB) # making a vector of the observed counts for each genotype
    ExpAA <- (p^2)*N # calculating the expected counts for each genotype
    ExpAB <- 2*p*q*N
    ExpBB <- (q^2)*N
    ExpN <- c(ExpAA,ExpAB,ExpBB) # creatin a vector of the expected counts for each genotype
    matrix <- cbind(ObsN,ExpN) # generating a matrix of the expected and observed
    signif <- chisq.test(matrix,matrix,correct=TRUE) # determining significance using chisq.test
    output$signif <- renderPrint({print(signif)}) # use renderPrint to tell the code that the printed results of signif are what is to be displayed
  })
  
}

shinyApp(ui = ui, server = server)