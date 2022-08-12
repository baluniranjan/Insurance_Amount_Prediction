library(shiny)
library(shinythemes)

sex <- c("Male"=0, "Female"=1)
smoker <- c("Yes"=1, "No"=0)
region <- c("NorthWest"=2, "NorthEast"=1, "SouthWest"=3, "SouthEast"=4)
ui <- fluidPage(theme = shinytheme("superhero"),
  
  titlePanel("Insurance Amount Prediction"),
  
  sidebarLayout(
    
    sidebarPanel(
       
      #bmi = kg / (height/100)^2
      
      width = 5,

      sliderInput("age_cust", "Enter your age:",value = 18, min = 18, max = 64),
      
      radioButtons("sex", "Enter your Gender: ", sex),
      
      sliderInput("height_cust", "Enter your height(cms):", value = 20, min = 20, max = 200),
      
      sliderInput("weight_cust", "Enter your weight(kgs):", value = 20, min = 20, max = 200),
      
      sliderInput("no_children", "Enter the number of children:", value = 0, min = 0, max = 5),
      
      radioButtons("smoker", "Are you a smoker? ", smoker),
      
      radioButtons("region", "Enter your region: ", region),
      
      submitButton("SUBMIT")
    ),
    
    mainPanel(
      width = 7,
      h3(style="margin: auto;   width: 50%; padding: 10px;font-size: 30px","Predicted Insurance Amount :"),
      h4(style="margin: auto;   width: 50%; padding: 10px;font-size: 70px",textOutput("predicted")),
    )
  )
)

server <- function(input, output) {
  model <- readRDS("./1_insurance_model.rds")
  bmi <- reactive({round(input$weight_cust / (input$height_cust/100)^2 , 1)})
  
  data <- reactive({data.frame(age = input$age_cust,
                               sex=strtoi(input$sex),
                               bmi=bmi(),
                               children=input$no_children,
                               smoker=strtoi(input$smoker),
                               region=strtoi(input$region))})

  pred = reactive({ predict(model, data() ) })
  
  output$predicted <- renderText({ paste("$",round( pred(), 1))})
  }


shinyApp(ui = ui, server = server)