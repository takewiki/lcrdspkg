mydata <- dm_dealOne()

mydata$FLength <- 1

View(mydata)


mydata$FkeyNo


#第一种情况测试 件号变量
mydata2 <- dm_dealOne(FchartNo = 'SYE601B672',FGtab = 'G10',FLtab = 'L18')


View(mydata2)

#第二种情况 L翻变量

mydata3 <- dm_dealOne(FchartNo = 'YE604B852',FGtab = 'G52',FLtab = 'L03')


View(mydata3)

#第三种情况 G表变量

mydata4 <- dm_dealOne(FchartNo = 'YE604B853',FGtab = 'G01',FLtab = 'L03')
View(mydata4)



dm_dealAll()


dm_ReadBy_ChartNo_Ltab(FchartNo = 'YE604B797',FLtab = 'L01')


dm2 <-dm_ReadBy_ChartNo(FchartNo = 'YE604B797')
View(dm2)


dm_writeDB_ChartNo(FchartNo = 'YE604B797')




library(lcrdspkg)
mydata3 <-dm_selectDB_detail(FchartNo = 'YE500A838-S01',FParamG = 'G03',FParamL = NA)
View(mydata3)


mydata5 <-dm_queryAll()

View(mydata5)





