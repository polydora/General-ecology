
random_add <- function(x, SD_perc=0){
  n <- length(x)
  x_modif <- sapply(1:n, simplify =TRUE,FUN = function(.i){x[.i] + rnorm(1, 0, x[.i]*SD_perc/100)})
  return(x_modif)
}




# Функция для построения временного ряда численности популяции в соответствии с моделью двух конкурирующих видов

competitors_dynamics <- function (Nyears=50, N_start=1, P_start=1, SD=0, r1=2, r2=1, alpha=4, beta = 1, h=1, K1=1000, K2 = 1000, SD_r=0, Relative_N=FALSE) {
  N_total <- data.frame(Nt=c(N_start, rep(0, (Nyears-1))), Pt=c(P_start, rep(0, (Nyears-1))))
  
  for (i in 2:Nyears){
    # r <- r + rnorm(1, 0, SD_r)
    N_total$Nt[i] <- N_total$Nt[i-1]*exp(r1*(1-N_total$Nt[i-1]/K1)-alpha*N_total$Pt[i-1])
    N_total$Nt[i] <- random_add(N_total$Nt[i], SD)
    if(N_total$Nt[i] < 0.1) N_total$Nt[i] <- 0.1
    
    N_total$Pt[i] <- N_total$Pt[i-1]*exp(r2*(1-N_total$Pt[i-1]/K2) - beta*N_total$Nt[i-1]) 
    N_total$Pt[i] <- random_add(N_total$Pt[i], SD)
    if(N_total$Pt[i] < 0.1) N_total$Pt[i] <- 0.1
    
    
  }  
  
  #     Приводим к относительной шкале
  if(Relative_N){
    N_max <- max(N_total$Nt)
    N_total$Nt <- N_total$Nt/N_max*100
    P_max <- max(N_total$Pt)
    N_total$Pt <- N_total$Pt/P_max*100
  }
  
  N_total$Years <- seq(1, Nyears, 1)
  library(ggplot2)
  
  
  
  pl_N <- ggplot (N_total, aes(x=Years, y=Nt)) + geom_line(colour="red", size = 2) + theme_bw() + labs(y="Вид 1", x = "Время")
  pl_P <- ggplot (N_total, aes(x=Years, y=Pt)) + geom_line(colour="blue", size = 2) + theme_bw() + labs(y="Вид 2", x = "Время")
  library(gridExtra)
  results <- list (N_total, pl_N, pl_P)
  
}




library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Модель конкурентных отношений"),
  
  sidebarLayout(
    sidebarPanel(
      #Задаем параметр r
      sliderInput(inputId = "r1",
                  label = "Значение мальтузианского параметра Вида 1:",
                  min = 0,
                  max = 3.5,
                  step = 0.1,
                  value = 0.1), #То с чего начинается

      sliderInput(inputId = "K1",
                  label = "Емкость среды Вида 1:",
                  min = 50,
                  max = 3000,
                  step = 1,
                  value = 200), #То с чего начинается
      
      
        sliderInput(inputId = "r2",
                  label = "начение мальтузианского параметра Вида 2:",
                  min = 0,
                  max = 3.5,
                  step = 0.1,
                  value = 0.1), 
      
      sliderInput(inputId = "K2",
                  label = "Емкость среды Вида 2:",
                  min = 50,
                  max = 3000,
                  step = 1,
                  value = 200), #То с чего начинается
      
            
      sliderInput(inputId = "beta",
                  label = "Отрицательное влияние со стороны Вида 1 :",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0), 
      
      
      sliderInput(inputId = "alpha",
                  label = "Отрицательное влияние со стороны Вида 2:",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0)
      
      
    ),
    
    
    
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
    library(gridExtra)
    Pl <- competitors_dynamics (r1 = input$r1, r2 = input$r2, K1 = input$K1, K2 = input$K2, alpha = input$alpha, beta = input$beta )
    grid.arrange(Pl[[2]], Pl[[3]], ncol = 1)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


