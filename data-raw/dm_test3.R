mydata <- dmQuery1_readDB_cn()

View(mydata)


library(readxl)
query2 <- read_excel("data-raw/DM配件混合查询.xlsx",
                       sheet = "DM配件混合查询")
View(query2)

is.na(query2$DM单号)


mydata2 <-dm_selectDB_detail_combo1()
 View(mydata2)




 mydata3 <- dmQuery_Batch_file()
 View(mydata3)
