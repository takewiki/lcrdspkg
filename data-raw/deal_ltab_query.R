dm_ReadBy_ChartNo_GL_dealOne(FchartNo = 'SE301B675',
                             FParamG = 'G01',
                             FParamL = 'L02',
                             page_size = 300)


dm_ReadBy_ChartNo_LtabBatch_calc(FchartNo = 'SE301B675',
                                 FGtab = 'G01',
                                 FLtabBatch = 'L02',
                                 page_size = 300)

dm_ReadBy_ChartNo_LtabBatch_dealBom(FchartNo = 'SE301B675',
                                    FParamG = 'G01',
                                    FParamL = 'L02',
                                    page_size = 300)


sql <- paste0("select  FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,1 as FLength, FQty*1 as FTotalQty,FGtab as FParamG,FParam_L as FParamL    from    [t_bom_Detail]
where FchartNo ='",FchartNo,"' and FGtab ='",FParamG,"' and FParam_L ='",FParamL,"' and FQty > 0 ")
data = tsda::sql_select(conn,sql)
ncount = nrow(data)
if(ncount >0){
  #针对数据进行处理
  #针对每一行处理
  res_raw = lapply(1:ncount, function(i){
    item =  data[i, ]
    flag = bom_getVarValueType(item$FkeyNo)
    if(flag =='length'){
      #如果件号是常用
      item$FLength = as.integer(item$FkeyNo)
      item$FTotalQty = as.integer(item$FQty) * item$FLength
    }
    return(item)

  })
  #合并相关数据
  data = do.call('rbind',res_raw)




}
#按G番及L番备份并删除相应的数据
dm_chartNo_delete_byGL(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL)
#写入数据库
totalRow <- nrow(data)
pages = tsdo::paging_setting(totalRow,page_size)
ncount_page = nrow(pages)
if(ncount_page >0){
  lapply(1:ncount_page, function(i){
    from = pages$FStart[i]
    to = pages$FEnd[i]
    item =  data[from:to, ]
    tsda::db_writeTable(conn=conn,table_name = 't_lcrds_bom',r_object = item,append = T)
    #print('step03')
  })
}





return(data)



