mydata  = Gtab_select_db_stat()
View(mydata)

str(mydata)



Gtab_write_db_stat()


#



Ltab_get_varValueBatch()


Ltab_get_varValueBatch(FLtabBatch = 'L33,L57,L65,L67,L73,L92,L99,L82')


Ltab_get_varValueBatch(FLtabBatch = 'L33,L98')
Ltab_get_varValueBatch(FLtabBatch = 'L57')




mydata3 = dm_ReadBy_ChartNo_LtabBatch_calc(FLtabBatch = 'L33,L57,L65,L67,L73,L92,L99,L82')



mydata3 = dm_ReadBy_ChartNo_LtabBatch_calc(FLtabBatch = 'L33')
View(mydata3)

names(mydata3)



FLtabBatch = 'L33,L57,L65,L67,L73,L92,L99,L82'


bbc = strsplit(FLtabBatch,',')
bbc = bbc[[1]]
bbc2 = sort(bbc)
bbc2 = paste0(bbc2,collapse = ",")
bbc2





mydata4 = dm_ReadBy_ChartNo_LtabBatch_calc(FLtabBatch = 'L57')
View(mydata4)





mydata5 = dm_ReadBy_ChartNo_LtabBatch_dealBom(FParamL = 'L33,L57,L65,L67,L73,L92,L99,L82')
View(mydata5)

FLtabBatch = 'L33'

varList_sort(FLtabBatch)








dm_ReadBy_ChartNo_LtabBatch_dealBom(FParamL = 'L33,L57,L65,L67,L73,L92,L99,L82')




dm_ReadBy_ChartNo_GL_dealOne(FParamL = 'L33,L57,L65,L67,L73,L92,L99,L82')



lapply(letters, function(i){
  print(i)
})



dm_writeDB_ChartNo()

dm_ReadBy_ChartNo()


dm_selectDB_detail2()



dm_selectDB_detail2()


dm_selectDB_detail2(FParamL = 'L33')
dm_selectDB_detail2(FParamL = 'L33,L57,L65,L67,L73,L92,L99,L82')


