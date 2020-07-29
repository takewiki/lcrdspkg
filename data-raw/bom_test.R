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
