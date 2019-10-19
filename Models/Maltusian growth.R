Nyears <- 50
#Мальтузианская модель
random_add <- function(x, SD_perc=0){
  n <- length(x)
  x_modif <- sapply(1:n, simplify =TRUE,FUN = function(.i){x[.i] + rnorm(1, 0, x[.i]*SD_perc/100)})
  return(x_modif)
}



#Функция логистической модели
Maltus_dynamics <- function (x, N_start=100, SD_r=0,SD=0, Relative_N=FALSE){
  
  N_total <- data.frame(Nt=c(N_start, rep(0, (Nyears-1))))
  for (i in 2:Nyears){
    x <- x + rnorm(1, 0, SD_r)
    N_total$Nt[i] <- N_start*exp(x*i) 
    # N_total$Nt[i] <- N_total$Nt[i-1]*exp(x)
    N_total$Nt[i] <- random_add(N_total$Nt[i], SD)
    
    if(N_total$Nt[i] < 1) N_total$Nt[i] <- 1
    
    
  }
  N_total$Years <- seq(1, Nyears, 1)
  
  #     Приводим к относительной шкале
  if(Relative_N){
    N_max <- max(N_total$Nt)
    N_total$Nt <- N_total$Nt/N_max*100
  }
  
  
  library(ggplot2)
  pl_N <- ggplot (N_total[-1,], aes(x=Years, y=Nt)) + geom_line(colour="blue", size=2) + theme_bw() + labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20)) 
 
  
  results <- list (N_total, pl_N)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Мальтузианская модель"),
  
  sidebarLayout(
    sidebarPanel(
      #Задаем параметр r
      sliderInput(inputId = "r",
                  label = "Значение мальтузианского параметра (r):",
                  min = -2,
                  max = 2,
                  step = 0.1,
                  value = 0.1), #То с чего начинается
      

      
      #Задаем параметр случайную вариацию для R
      sliderInput(inputId = "SD_r",
                  label = "Случайные вариации для R:",
                  min = 0,
                  max = 0.1,
                  value = 0), 
      
      #Задаем параметр случайную вариацию Sample error (%)
      sliderInput(inputId = "SD",
                  label = "Случайные вариации (sample error):",
                  min = 0,
                  max = 100,
                  value = 0)
      
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("my_graph")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$my_graph <- renderPlot({
    
    #Здесь может быть код 
    #Функция рисующая график 
    Maltus_dynamics (x = input$r, SD_r = input$SD_r, SD = input$SD)[2]
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
