
library(gridExtra)
library(ggplot2)



random_add <- function(x, SD_perc=0){
  n <- length(x)
  x_modif <- sapply(1:n, simplify =TRUE,FUN = function(.i){x[.i] + rnorm(1, 0, x[.i]*SD_perc/100)})
  return(x_modif)
}



# Функция для построения временного ряда численности популяции в соответствии с моделью хищник-жертва

pred_pray_dynamics <- function (Nyears=50, N_start=0.1, P_start=0.1, SD=0, r=2, g=4, s=1,h=1, K=1, SD_r=0, Relative_N=TRUE) {
  N_total <- data.frame(Nt=c(N_start, rep(0, (Nyears-1))), Pt=c(P_start, rep(0, (Nyears-1))))
  
  for (i in 2:Nyears){
    r <- r + rnorm(1, 0, SD_r)
    N_total$Nt[i] <- N_total$Nt[i-1]*exp(r*(1-N_total$Nt[i-1]/K)-g*N_total$Pt[i-1])
    N_total$Nt[i] <- random_add(N_total$Nt[i], SD)
    if(N_total$Nt[i] < 0.1) N_total$Nt[i] <- 0.1
    
    N_total$Pt[i] <- N_total$Pt[i-1]*exp(s*(1-h*N_total$Pt[i-1]/N_total$Nt[i-1])) 
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
  
  
  
  pl_N <- ggplot (N_total, aes(x=Years, y=Nt)) + geom_line(colour="black", size = 2) + theme_bw() + labs(x = "Время", y = "Численность жертвы")
  pl_P <- ggplot (N_total, aes(x=Years, y=Pt)) + geom_line(colour="blue", size = 2) + theme_bw() + labs(x = "Время", y = "Численность хищника")
  
  library(gridExtra)
  results <- list (N_total, pl_N, pl_P)
  
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Динамика популяции Хищника и Жертвы"),
  
  sidebarLayout(
    sidebarPanel(
      #Задаем параметр r
      sliderInput(inputId = "r",
                  label = "Значение мальтузианского параметра для жертвы (r):",
                  min = 0,
                  max = 3.5,
                  step = 0.1,
                  value = 1), #То с чего начинается
      
      
      
      #
      sliderInput(inputId = "g",
                  label = "Уровень смертности хищника при отсутствии жертвы",
                  min = 0,
                  max = 8,
                  value = 3), 
      
      #
      sliderInput(inputId = "s",
                  label = "Эффективность использования жертвы ",
                  min = 0,
                  max = 3.5,
                  value = 1)
      
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("my_graph", height = "700px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$my_graph <- renderPlot({
    
    #Здесь может быть код 
    #Функция рисующая график 
    Pl <- pred_pray_dynamics (r = input$r, s = input$s, g = input$g, Relative_N = T)
    
    phase <- ggplot(Pl[[1]], aes(x = Nt, y = Pt)) + geom_path(color = "red") + labs(x="Численность жертвы", y = "Численность хищника") + theme_classic()
    
    grid.arrange(Pl[[2]], Pl[[3]], phase, ncol = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
