library(lcrdspkg)


mydata <-dm_ReadBy_ChartNo_Ltab(FchartNo = 'YX200A714')

mydata2 <-mydata[mydata$FParamG=='G08', ]
openxlsx::write.xlsx(mydata2,'bom_test.xlsx')

conn=tsda::conn_rds('lcrds')
FchartNo = 'YX200A714'

sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'")
#读取数据,其中r表示G表
r <- tsda::sql_select(conn,sql)


key <- r$FkeyNo

key_unique <- unique(key)


key_unique;

bb <- lapply(key_unique, function(keyNo){
  #print(keyNo)
  res <- bom_getKeyNoType(keyNo = keyNo)

  #print(res)
  r <- data.frame(res,keyNo)
  return(r)
})

cc <- do.call('rbind',bb)
View(cc)

unique(cc$res);




bom_getVarValueType('21')




mydata <- dm_selectDB_detail2(FParamL = 'L07,L08,L13,L27,LS01,LS17')
View(mydata)





conn=tsda::conn_rds('lcrds')
FchartNo='YX200A714'
FLtab ='L07'
sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'")
#读取数据,其中r表示G表
r <- tsda::sql_select(conn,sql)
print(r[r$FIndexTxt =='-13',])

mydata2 <-dm_ReadBy_ChartNo_Ltab()
View(mydata2)


mydata2[mydata2$FIndexTxt=='-13',]



dm_dealAll2()




data <- dm_ReadBy_ChartNo(FchartNo ='YX200A714' )

View(data)
