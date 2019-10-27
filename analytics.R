# Анализ попыток студентов 

trial <- read.csv("data/course-32065-submissions.csv", header = T)

class <- read.csv("data/class.csv")

class <- class[, c("Name", "ID", "Group")]

trial <- merge(class, trial, by = "ID")




trial$attempt <- format(as.POSIXct((trial$attempt_time), origin = "1970-01-01", tz = "UTC"), "%d-%m %H:%M")

trial$submission <- format(as.POSIXct((trial$submission_time), origin = "1970-01-01", tz = "UTC"), "%d-%m %H:%M")




tail(trial[trial$last_name == "Тарасова", ])

write.csv(trial, "data/all_attempts.csv")

write.csv(trial[trial$last_name == "Тарасова", ], "data/Tarasova.csv")


###############################

contr_trial <- read.csv("data/Control1_attempts.csv", header = T)

contr_trial$submission_time <- as.POSIXct(contr_trial$submission,format = "%d-%m %H:%M")


ggplot(contr_trial, aes(x = Group, y = submission_time)) + geom_boxplot() + labs(x = "Группа", y = "Час, когда начали работу")




