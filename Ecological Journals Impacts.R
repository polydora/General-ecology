# Картинка с запросами гугла

journals <- data.frame(Query = c(
  "Cell research journal",
  "Molecular biology research journal",
  "Ecology research journal",
  "Physiology research journal",
  "Morphology research journal",
  "Botany research journal", 
  "Zoology research journal",
  "Micology research journal"),
Number = c( 295000000,  246000000, 169000000, 101000000,  113000000, 58400000, 32700000, 10800  ))

journals$Number2 <- journals$Number / 1000

journals$Label <- factor(c(0,0,1,0, 0, 0, 0, 0)) 

library(ggplot2)


ggplot(journals, aes(y = Number2, x = reorder(Query, Number2), fill = Label )) + geom_bar(stat = "identity") + coord_flip() + theme_bw() + scale_fill_manual(values = c("blue", "red")) + labs(y = "Ссылки (тыс.)", x = "") + guides(fill = "none")


# Импакты журналов


mol_biol <- read.table("scimagojr 2018  Subject Category - Molecular Biology.csv", sep = ";", header = T, dec = ",")

mol_biol$Area <- "Molecular biology"

cyt <- read.table("scimagojr 2018  Subject Category - Cell Biology.csv", sep = ";", header = T, dec = ",")

cyt$Area <- "Cell biology"

ecol <- read.table("scimagojr 2018  Subject Category - Ecology.csv", sep = ";", header = T, dec = ",")

ecol$Area <- "Ecology"

print_product <- rbind(mol_biol, cyt, ecol)

print_product$SJR.Quartile

ggplot(print_product[print_product$SJR.Quartile == "Q1", ], aes(x = Area, y = SJR)) + stat_summary(fun.y = "mean", geom = "bar", fill = "blue") + theme_bw() + labs(x ="") + theme(axis.text = element_text(size = 15))

ggplot(print_product[print_product$SJR.Quartile == "Q1", ], aes(x = Area, y = SJR)) + stat_summary(fun.y = "mean", geom = "bar", fill = "blue") + theme_bw() + labs(x ="") + theme(axis.text = element_text(size = 15))


print_product[print_product$SJR.Quartile == "Q1", c("Area", "SJR")]



ggplot(print_product[print_product$SJR.Quartile == "Q1", ], aes(x = Area)) + geom_bar(stat = "count")  + theme_bw() + labs(x ="") + theme(axis.text = element_text(size = 15))



