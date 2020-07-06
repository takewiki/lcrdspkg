mydata <- dm_queryAll2()

View(mydata)


dm_queryAll_writeDB()


mydata2 <- dmList_toDo_Multi()
#View(mydata2)

mydata3 <- dmList_queryAll_Multi(data = mydata2)
View(mydata3)


dmList_Expand_Multi()



mydata4 <- dmList_readDB_Input_cn()

print(names(mydata4))
openxlsx::write.xlsx(mydata4,'dm_miltiple.xlsx')


data5 <-dmList_Expand_Multi()
View(data5)
