# Анализ попыток студентов 

trial <- read.csv("data/course-32065-submissions.csv", header = T)


trial$attempt <- format(as.POSIXct((trial$attempt_time), origin = "1970-01-01", tz = "UTC"), "%d-%m %H:%M")

trial$submission <- format(as.POSIXct((trial$submission_time), origin = "1970-01-01", tz = "UTC"), "%d-%m %H:%M")


library(ggplot2)

ggplot(trial, aes(x = submission_time )) + geom_histogram(binwidth = 1)

tail(trial[trial$first_name == "Смутин", ])


write.csv(trial[trial$first_name == "Смутин", ], "data/Smutin.csv")

write.csv(trial[trial$last_name == "Тишкова", ], "data/Tishkova.csv")


write.csv(trial[trial$last_name == "Воскресенсксая", ], "data/Voskresenskaya.csv")
