#' 针对数量进行处理
#'
#' @param key 关键词
#' @param searchTable 表
#' @param returnType 反回值类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' variable_search()
variable_search <- function(key,searchTable,returnType='int') {
  res =  key %in% searchTable
  if(returnType == 'int'){
    res = as.integer(res)
  }
  return(res)

}


#' 针对变更进行排序
#'
#' @param varList 变量
#' @param desc 是否降序
#' @param sep 分割符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' varList_sort()
varList_sort <- function(varList ='L33,L57,L65,L67,L73,L92,L99,L82',desc= FALSE,sep=',') {
  varList = strsplit(varList,sep)
  bbc = varList[[1]]
  bbc2 = sort(bbc,decreasing = desc)
  res = paste0(bbc2,collapse = sep)
  return(res)

}



#' 获取变量的业务模型
#'
#' @param conn 连接
#' @param FkeyNo_tag  G番标记
#' @param FLtab_tag   L番标记
#' @param FQty_tag    Q标记
#'
#' @return 返回值
#' @export
#'
#' @examples
#' variable_getMode()
variable_getMode <- function(conn=tsda::conn_rds('lcrds'),FkeyNo_tag =0,FLtab_tag =0,FQty_tag =0) {

sql <- paste0("select FMode,FNote  from t_rule_variable
where FkeyNo_tag = ",FkeyNo_tag," and FLtab_tag = ",FLtab_tag," and FQty_tag = ",FQty_tag," ")
data =  tsda::sql_select(conn,sql)

return(data)

}


#' 获取相当数据
#'
#' @param conn 连接
#' @param FchartNo 图号
#' @param FLtabBatch L号
#' @param FGtab G番
#' @param page_size 每页大小
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_ReadBy_ChartNo_LtabBatch_calc()
dm_ReadBy_ChartNo_LtabBatch_calc <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FGtab='G01',FLtabBatch ='L07',page_size = 300){
  #处理规则
  #针对多个变量先进行排序处理
  #FLtabBatch = varList_sort(FLtabBatch)
  #需要追加对L番的判断，如果L番都不存在，就不需要进行计算了

  data =  Gtab_write_db_stat(conn = conn,FchartNo = FchartNo,FGtab = FGtab,page_size = page_size)
  #View(data)
  ncount  = nrow(data)
  vars = Ltab_get_varValueBatch(conn = conn,FchartNo = FchartNo,FLtabBatch = FLtabBatch)
  data_raw = lapply(1:ncount, function(i){
    row = data[i, ]
    # print(row)
    FchartNo = row$FchartNo
    FItemName  = row$FItemName
    FSubChartNo  = row$FSubChartNo


    FkeyNo = row$FkeyNo
    FLtab  = row$FLtab
    FItemModel   = row$FItemModel
    FNote  = row$FNote
    FIndexTxt  =  row$FIndexTxt
    FQty = row$FQty
    FGtab =  row$FGtab

    FkeyNo_tag =  row$FkeyNo_tag
    FLtab_tag  = row$FLtab_tag
    FQty_tag = row$FQty_tag
    FTagCount_dim  = row$FTagCount_dim
    FTagCount_value  = row$FTagCount_value
    FTagCount_total = row$FTagCount_total
    ruleInfo = variable_getMode(FkeyNo_tag = FkeyNo_tag,FLtab_tag = FLtab_tag,FQty_tag = FQty_tag)
    ruleCount = nrow(ruleInfo)
    if(ruleCount >0){
      FTagMode = ruleInfo$FMode
      FTagNote = ruleInfo$FNote
    }else{
      FTagMode = '未知'
      FTagNote = '未知'
    }



    #针对件号进行处理
    if(FkeyNo_tag == 1){
      #变更需要进行取
      FkeyNo_sel = vars[vars$FkeyNo == FkeyNo,"FLength"]

      FkeyNo_len =  length(FkeyNo_sel)
      if(FkeyNo_len >0){
        #存在数据,并进行替代
        FkeyNo_var = FkeyNo
        FLtab_used4G = vars[vars$FkeyNo == FkeyNo,"FLtab"]
        FkeyNo = as.character(FkeyNo_sel)


      }else{
        #不存在数据,保留变量，不再替换
        FkeyNo_var = FkeyNo
        FkeyNo = FkeyNo
        FkeyNo_len = 0
        FLtab_used4G =''

      }

    }else{
      #常数项,不需要替代
      FkeyNo_var = ''
      FkeyNo = FkeyNo
      FkeyNo_len = 1
      FLtab_used4G =''

    }
    #针对L番进行处理
    if(FLtab_tag == 1){
      #变更需要进行取
      FLtab_sel = vars[vars$FkeyNo == FLtab,"FLength"]

      FLtab_len =  length(FLtab_sel)
      if(FLtab_len >0){
        #存在数据,并进行替代
        FLtab_var = FLtab
        FLtab_used4L = vars[vars$FkeyNo == FLtab,"FLtab"]
        #完整查询后再做替代
        FLtab = as.character(FLtab_sel)
        print(1)
        print(vars)
        print(FLtab)
        print(FLtab_used4L)
      }else{
        #不存在数据
        FLtab_var = FLtab
        FLtab = FLtab
        FLtab_len = 0
        FLtab_used4L = ''
        print(2)
        print(FLtab_used4L)

      }

    }else{
      #常数项,不需要替代
      FLtab_var = ''
      FLtab = FLtab
      FLtab_len = 1
      FLtab_used4L = ''
      print(3)
      print(FLtab_used4L)

    }
    #针对长度进行替代
    if(FQty_tag == 1){
      #变更需要进行取
      FQty_sel = vars[vars$FkeyNo == FQty,"FLength"]
      FQty_len =  length(FQty_sel)
      if(FQty_len >0){
        #存在数据,并进行替代
        FQty_var = FQty
        FLtab_used4Q = vars[vars$FkeyNo == FQty,"FLtab"]
        FQty = as.integer(FQty_sel)

      }else{
        #不存在数据
        FQty_var = FQty
        FQty = FQty
        FQty_len = 0
        FLtab_used4Q =''


      }

    }else{
      FQty_var = ''
      #常数项,不需要替代
      FQty = FQty
      FQty_len = 1
      FLtab_used4Q =''

    }
    #计算总长度
    total_len = FkeyNo_len*FLtab_len*FQty_len
    if(total_len == 0){
      #print('变量不全')
      FCalcNote = '变量替代不全'
      FQty = 0
      FParam_L = FLtabBatch
      #print(FLtab_used4G)
      #print(FLtab_used4L)
      # print(FLtab_used4Q)
      res = data.frame(FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab,FkeyNo_tag,FLtab_tag,FQty_tag,FTagCount_dim,FTagCount_value,FTagCount_total,FkeyNo_var,FkeyNo_len,FLtab_var,FLtab_len,FQty_var,FQty_len,total_len,FTagMode,FTagNote,FCalcNote,FLtab_used4G,FLtab_used4L,FLtab_used4Q,FParam_L,stringsAsFactors = F )
    }else{
      #print('OK')
      FCalcNote = '变量完成替代或不需替代'
      FchartNo = rep(FchartNo,total_len)
      FItemName = rep(FItemName,total_len)
      FSubChartNo = rep(FSubChartNo,total_len)
      #增加判断处理,防止产生多个匹配结果
      FkeyNo = rep(FkeyNo,total_len/FkeyNo_len)
      FLtab = rep(FLtab,total_len/FLtab_len)
      FItemModel =  rep(FItemModel,total_len)
      FNote = rep(FNote,total_len)
      FIndexTxt = rep(FIndexTxt,total_len)
      FQty =  rep(FQty,total_len/FQty_len)
      FGtab =  rep(FGtab,total_len)
      FkeyNo_tag =  rep(FkeyNo_tag,total_len)
      FLtab_tag = rep(FLtab_tag,total_len)
      FQty_tag =  rep(FQty_tag,total_len)
      FTagCount_dim = rep(FTagCount_dim,total_len)
      FTagCount_value = rep(FTagCount_value,total_len)
      FTagCount_total =  rep(FTagCount_total,total_len)
      FkeyNo_var =  rep(FkeyNo_var,total_len)
      FkeyNo_len = rep(FkeyNo_len,total_len)
      FLtab_var = rep(FLtab_var,total_len)
      FLtab_len =  rep(FLtab_len,total_len)
      FQty_var = rep(FQty_var,total_len)
      FQty_len = rep(FQty_len,total_len)
      FTagMode = rep(FTagMode,total_len)
      FTagNote = rep(FTagNote,total_len)
      FCalcNote = rep(FCalcNote,total_len)
      FLtab_used4G =rep(FLtab_used4G,total_len)
      FLtab_used4L = rep(FLtab_used4L,total_len)
      FLtab_used4Q = rep(FLtab_used4Q,total_len)
      FParam_L = rep(FLtabBatch,total_len)
      total_len = rep(total_len,total_len)
      #print(FLtab_used4G)
      #print(FLtab_used4L)
      #print(FLtab_used4Q)
      res = data.frame(FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab,FkeyNo_tag,FLtab_tag,FQty_tag,FTagCount_dim,FTagCount_value,FTagCount_total,FkeyNo_var,FkeyNo_len,FLtab_var,FLtab_len,FQty_var,FQty_len,total_len,FTagMode,FTagNote,FCalcNote,FLtab_used4G,FLtab_used4L,FLtab_used4Q,FParam_L,stringsAsFactors = F )


    }




    return(res)





  })

  data_com = do.call('rbind',data_raw)
  ncount_com =nrow(data_com)
  if(ncount_com >0){



    #step1 备份原来的数据,不存在性能问题
    sql_bak <- paste0("
insert into t_bom_DetailDel
select * from t_bom_Detail
where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"' and FParam_L ='",FLtabBatch,"'")
    tsda::sql_update(conn,sql_bak)

    #删除原有的数据，删除数据也可以接受
    sql_del <- paste0("delete  from t_bom_Detail
where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"' and FParam_L ='",FLtabBatch,"'")
    tsda::sql_update(conn,sql_del)
    #写入数据库
    pageInfo =  tsdo::paging_setting(volume = ncount_com,each_page = page_size)
    pageCount = nrow(pageInfo)
    lapply(1:pageCount, function(i){
      from = pageInfo$FStart[i]
      to = pageInfo$FEnd[i]
      item =  data_com[from:to, ]
      #写入数据
      try({
        tsda::db_writeTable(conn = conn,table_name = 't_bom_Detail',r_object = item,append = T)
      })

    })

  }

  return(data_com)




}




#' 针对数据进行处理
#'
#' @param conn 连接
#' @param FchartNo 图号
#' @param FGtab G番
#' @param FLtabBatch L番或多个
#' @param page_size 页面大小
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_ReadBy_ChartNo_LtabBatch_dealBom()
dm_ReadBy_ChartNo_LtabBatch_dealBom <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FParamG='G01',FParamL ='L33',page_size = 300) {

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


}






#' 获取相当数据,处理每一条记录
#'
#' @param conn 连接
#' @param FchartNo 图号
#' @param page_size 每页大小
#' @param FParamG 参数
#' @param FParamL 参数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_ReadBy_ChartNo_GL_dealOne()
dm_ReadBy_ChartNo_GL_dealOne<- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FParamG='G01',FParamL ='L33',page_size = 300){

    #数据处理
    dm_ReadBy_ChartNo_LtabBatch_calc(conn = conn,FchartNo = FchartNo,FGtab = FParamG,FLtabBatch = FParamL,page_size = page_size)
    #标准数据处理
    data = dm_ReadBy_ChartNo_LtabBatch_dealBom(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL,page_size = page_size)
    return(data)




}

