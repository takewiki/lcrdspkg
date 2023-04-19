library(readxl)
bom_src4 <- read_excel("data-raw/bom_src4.xlsx",
                       sheet = "SYE601B672", n_max = 198)
View(bom_src4)



mydata <-"a,b,c,d,e,f,g"
res <-strsplit(mydata,",")

res[[1]]
