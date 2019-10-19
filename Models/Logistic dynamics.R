# Логистическая модель со случайным эффектом 
random_add <- function(x, SD_perc=0){
  n <- length(x)
  x_modif <- sapply(1:n, simplify =TRUE,FUN = function(.i){x[.i] + rnorm(1, 0, x[.i]*SD_perc/100)})
  return(x_modif)
}



#Функция логистической модели
log_dynamics <- function (x, N_start=100, K=100000, Nyears=50, SD_r=0,SD=0, Relative_N=FALSE, Maltus_plot = FALSE){
  
  N_total <- data.frame(Nt=c(N_start, rep(0, (Nyears-1))), Nt2=c(N_start, rep(0, (Nyears-1))) )
  
  for (i in 2:Nyears){
    x <- x + rnorm(1, 0, SD_r)
    N_total$Nt[i] <- N_total$Nt[i-1]*exp(x*(1-N_total$Nt[i-1]/K))
    N_total$Nt2[i] <- N_total$Nt2[i-1]*x
    # N_total$Nt[i] <- N_total$Nt[i-1]*x*(1-N_total$Nt[i-1]))
    N_total$Nt[i] <- random_add(N_total$Nt[i], SD)
    N_total$Nt2[i] <- random_add(N_total$Nt2[i], SD)
    if(N_total$Nt[i] < 1) N_total$Nt[i] <- 1
    
    
  }
  N_total$Years <- seq(1, Nyears, 1)
  
  #     Приводим к относительной шкале
  if(Relative_N){
    N_max <- max(N_total$Nt)
    N_total$Nt <- N_total$Nt/N_max*100
  }
  
  
  library(ggplot2)
  pl_N <- ggplot (N_total, aes(x=Years, y=Nt)) + geom_line(colour="blue", size = 1) + theme_bw() + geom_hline(yintercept = K, color = "red") + labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20)) 
  
  if (Maltus_plot){
    pl_N <- ggplot (N_total, aes(x=Years, y=Nt)) + geom_line(colour="blue", size = 1) + theme_bw() + geom_hline(yintercept = K, color = "red") + labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20)) + geom_line(aes(y = Nt2), linetype = 2) 
  }
  
  
  if(Relative_N){
    pl_N <- ggplot (N_total, aes(x=Years, y=Nt)) + geom_line(colour="blue", size = 1) + theme_bw() + labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20)) 
  }
  
  results <- list (N_total, pl_N)
}

Two_log_dynam = function(x, y, ...) {

  Pop1 <- log_dynamics(x,N_start = 1000, SD=0,Nyears=50,  K = 100000, Relative_N = F)[1]
  Pop1 <- as.data.frame(Pop1)
  Pop1$pop <- "Population1"
  
  Pop2 <- log_dynamics(x,N_start = (1000 + y), SD=0,Nyears=50,  K = 100000, Relative_N = F)[1]
  Pop2 <- as.data.frame(Pop2)
  Pop2$pop <- "Population2"
  
  Pop <- rbind(Pop1, Pop2)
  
  ggplot(Pop, aes(x=Years, y = Nt, color = pop)) + geom_line() + theme_bw() + scale_color_manual(values = c("red", "blue")) + labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20), legend.text = element_text(size = 20))
  
    
}







library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Модель ограниченного роста и хаос"),
  
  sidebarLayout(
    sidebarPanel(
      #Задаем параметр R
      sliderInput(inputId = "r",
                  label = "Значение мальтузианского параметра (R):",
                  min = 0.1,
                  max = 4,
                  value = 0.25), #То с чего начинается
      #Задаем Различия в начальной численности 
      sliderInput(inputId = "DN",
                  label = "Величина различий в начальных численностях:",
                  min = -10,
                  max = 10,
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
    Two_log_dynam(y = input$DN, x = input$r )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
