# Модель динамики мидиевых поселений


d <- 0.05 # Стартовая смертность


l <- 0.85 # прирост смертности (параметр определяет форму кривой выживания)


s <- 0.92 # Смертность ювенилов


p <- 2.5 # Падение смертности ювенилов

# lesley() Функция для создания матрицы Лесли

lesley <- function (d=0.05, l = 0.85, s = 0.92, p =2.5){
  
  juv_dead <- rep(NA, 30)
  juv_dead[1] <- d
  
  for(i in 2:30) juv_dead[i] <- (juv_dead[i-1]*exp(l))/(1+((exp(l)-1)*juv_dead[i-1])) 
  
  dif_dead <- rep(NA, 30)
  dif_dead[1] <- s
  for(i in 2:30) dif_dead[i] <- dif_dead[i-1]/p 
  
  total_dead <- juv_dead + dif_dead
  total_dead[total_dead > 1] <- 1
  
  p_survival <- 1 - total_dead
  
  les <- matrix(rep(0,900), nrow=30, ncol=30)
  les[1, 4:30] <- 1
  for(i in 2:30) les[i, (i-1)] <- p_survival[(i-1)]
  return(les)
}

lesley()

##################3

juvdead <-  function(d=0.05, l = 0.85, s = 0.92, p =2.5){
  juv_dead <- rep(NA, 30)
  juv_dead[1] <- d
  for(i in 2:30) juv_dead[i] <- (juv_dead[i-1]*exp(l)) / (1+((exp(l)-1)*juv_dead[i-1])) 
  return(juv_dead)
}





totalsurviv <- function (d=0.05, l = 0.85, s = 0.92, p =2.5){
  
  juv_dead <- rep(NA, 30)
  juv_dead[1] <- d
  
  for(i in 2:30) juv_dead[i] <- (juv_dead[i-1]*exp(l))/(1+((exp(l)-1)*juv_dead[i-1])) 
  
  dif_dead <- rep(NA, 30)
  dif_dead[1] <- s
  for(i in 2:30) dif_dead[i] <- dif_dead[i-1]/p 
  
  total_dead <- juv_dead + dif_dead
  total_dead[total_dead > 1] <- 1
  
  p_survival <- 1 - total_dead
  return(p_survival)
}



biomassa <- function (x, l_inf=76.3,l0=0.35,k=0.1375, w0=0.000025, a=0.000103, b =2.964) {
  n <- length(x)
  Bert <- sapply(0:(n-1),simplify =TRUE,FUN = function(.i) {l0 + l_inf*(1-exp(.i*(-k)))})
  
  Allo <- w0 + a*Bert^b
  Allo * x
}



random_add <- function(x, SD_perc=0){
  n <- length(x)
  x_modif <- sapply(1:n, simplify =TRUE,FUN = function(.i){x[.i] + rnorm(1, 0, x[.i]*SD_perc/100)})
  return(x_modif)
}


Naumov_dynamics <- function (c, x=rep(0,30), Nyears=50, plot = TRUE, m=1, recruitment = 100000, Adult_age=4, SD=0, c_noise=0, Relative_N=FALSE, year_drop = 10, ...) {
  
  Lesley_matrix <- lesley(...)
  
  dynamics_N <- data.frame(A0=0, A1=0, A2=0, A3=0, A4=0, A5=0, A6=0, A7=0, A8=0, A9=0, A10=0, A11=0, A12=0, A13=0, A14=0, A15=0, A16=0, A17=0, A18=0, A19=0, A20=0, A21=0, A22=0, A23=0, A24=0, A25=0, A26=0, A27=0, A28=0, A29=0)
  
  dynamics_B<- data.frame(A0=0, A1=0, A2=0, A3=0, A4=0, A5=0, A6=0, A7=0, A8=0, A9=0, A10=0, A11=0, A12=0, A13=0, A14=0, A15=0, A16=0, A17=0, A18=0, A19=0, A20=0, A21=0, A22=0, A23=0, A24=0, A25=0, A26=0, A27=0, A28=0, A29=0)
  
  dynamics_N[1,] <- x
  dynamics_B[1,] <- biomassa(x)
  
  
  for (i in 2:Nyears) {
    
    dynamics_N[i,] <-  (Lesley_matrix) %*% (as.vector(dynamics_N[(i-1),], mode="numeric")) 
    dynamics_N[i,][dynamics_N[i,] < 1] <- 0        
    dynamics_N[i,] <- random_add(x=as.numeric(dynamics_N[i,]), SD_perc=SD)
    dynamics_N[i,][dynamics_N[i,] < 1] <- 0  
    
    #     dynamics_N[i,][dynamics_N[i,] > 0] <- dynamics_N[i,][dynamics_N[i,]>0] + rnorm(length(dynamics_N[i,][dynamics_N[i,]>0]), mean=0, sd=SD)
    dynamics_B[i,] <- biomassa(dynamics_N[i,])
    dynamics_B[i,][dynamics_B[i,] < 0] <- 0
    
    biom_adult <- sum(dynamics_B[i,Adult_age:ncol(dynamics_B)])
    
    #     ДИнамический шум!!!!
    c <-  c + rnorm(1, 0, c_noise) 
    
    if ((c * biom_adult^m) < recruitment) 
      dynamics_N[i,1] <- recruitment - (c * biom_adult^m)
    else
      dynamics_N[i,1] <- 0
  }
  
  dynamics_totalN <- apply(dynamics_N, 1, FUN="sum")
  dynamics_totalB <- apply(dynamics_B, 1, FUN="sum")
  dynamics_totalB_adult <- apply(dynamics_B[,(Adult_age+1):ncol(dynamics_B)], 1, FUN="sum")
  dynamics_totalN_Juv <- apply(dynamics_N[,1:((Adult_age+1)-1)], 1, FUN="sum")
  
  #     Приводим численность к относительной шкале
  if(Relative_N){
    N_max <- max(dynamics_totalN)
    dynamics_totalN <- dynamics_totalN/N_max*100
    B_max <- max(dynamics_totalB)
    dynamics_totalB <- dynamics_totalB/B_max*100
  }
  N_total <- data.frame(Years = seq((year_drop + 1), Nyears), Nt=dynamics_totalN[(year_drop + 1):Nyears])
  B_total <- data.frame(Years = seq((year_drop + 1), Nyears), Bt=dynamics_totalB[(year_drop + 1):Nyears])
  
  if (plot) #   рисуем картинку динамики численности
  {
    
    library(ggplot2)
    
    pl_N <- ggplot(N_total, aes(x=Years, y=Nt)) + geom_line(colour="blue") + theme_bw() + labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20), legend.text = element_text(size = 20))
    pl_B <- ggplot(B_total, aes(x=Years, y=Bt)) + geom_line(colour="blue") + theme_bw()+ labs(x= "Время", y = "Численность популяции") + theme(axis.title = element_text(size = 20), legend.text = element_text(size = 20))
  }   
  
  result <- list(dynamics_N, dynamics_B, N_total, dynamics_totalN_Juv, B_total, dynamics_totalB_adult, pl_N, pl_B)
  
  return(result)
  
}







library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Динамика обилия моллюсков-фильтраторов"),
  
  sidebarLayout(
    sidebarPanel(
      #Задаем параметр С
      sliderInput(inputId = "c",
                  label = "Степень отрицательного влияния взрослых на молодь",
                
                  min = -40,
                  max = 40,
                  step = 0.1,
                  value = 0) #То с чего начинается
      #Задаем Различия в начальной численности 

      
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
    Naumov_dynamics(c = input$c )[[7]]
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
