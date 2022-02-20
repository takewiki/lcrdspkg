#0.title-BOM单级展开------




#' 按图号读取数据
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_ReadBy_ChartNo()
dm_ReadBy_ChartNo <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200'){

  sql <- paste0("select * from   t_lcrds_bom
where  fchartNo ='",FchartNo,"'
order by  FParamG,FParamL, FIndexTxt")
  data =  tsda::sql_select(conn,sql)
  return(data)




  # raw <- lapply(FLtabs, function(FLtab){
  #   print(FLtab)
  #   res <- dm_ReadBy_ChartNo_Ltab(conn=conn,FchartNo = FchartNo,FLtab = FLtab)
  #   return(res)
  # })
  # data <- do.call('rbind',raw)
  # return(data)


}



#' 将图号数据写入数据库,只更新指定G番
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FParamG G番号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_writeDB_ChartNo_G()
dm_writeDB_ChartNo_G <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FParamG='G01'){

  FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)
  print(FLtabs)
  ncount_l = length(FLtabs)
  print(ncount_l)
  if(ncount_l >0 & ! is.na(FLtabs[1])){
    lapply(FLtabs,function(FParamL){
      print(1)
      #核心处理程序

      dm_ReadBy_ChartNo_GL_dealOne(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL,page_size = 300)

    })
  }

}



#' 将图号数据写入数据库
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_writeDB_ChartNo()
dm_writeDB_ChartNo <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200'){
  FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)
  FGtabs <- Gtab_get_uniqueMembers(conn = conn,FchartNo = FchartNo)

  ncount_g = length(FGtabs)
  ncount_l = length(FLtabs)
  if(ncount_g > 0 & !is.na(FGtabs[1])){
    if(ncount_l >0 & !is.na(FLtabs[1])){

      lapply(FGtabs, function(FParamG){

        lapply(FLtabs,function(FParamL){
          #核心处理程序

          dm_ReadBy_ChartNo_GL_dealOne(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL,page_size = 300)

        })

      })

    }

  }
   # data <- dm_ReadBy_ChartNo(conn=conn,FchartNo = FchartNo)
   # #删除要备份的数据
   # print('step01')
   # try({
   #   dm_chartNo_deleteDB(conn=conn,FchartNo = FchartNo)
   # })
   # print('step02')
   #
   # #插入新的数据
   # #tsda::upload_data(conn,'t_lcrds_bom',data)
   # #不做判断,直接写入数据
   # #这个地方可能是性能出现问题的重点
   # print('testDB')
   # #增加分页处理
   # #data =data[1:10, ]
   # totalRow <- nrow(data)
   # if(totalRow <=1000){
   #   pageRow = totalRow
   #
   # }else{
   #   pageRow=1000
   # }
   #
   # pages = page_setting(totalRow,pageRow)
   # lapply(pages, function(page){
   #   item =  data[page, ]
   #   tsda::db_writeTable(conn=conn,table_name = 't_lcrds_bom',r_object = item,append = T)
   #   print('step03')
   # })





}





#' 处理单个dm清单
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G表
#' @param FLtab L表值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_dealOne()
dm_dealOne <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672',FGtab ='G10',FLtab ='L02') {

  #读取基础表
  sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty  from t_lcrds_gtab where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"'")
  r <- tsda::sql_select(conn,sql)
  #print(r)
  ncount <- nrow(r)
  if(ncount>0){
    #针对整体数据进行处理
    r$FLength <- 1
    for (i in 1:ncount) {
      #针对每一行数据进行处理
      keyNo <- tsdo::na_replace(r[i,'FkeyNo'],'')
      #针对件号进行处理
      if(tsdo::len(keyNo) >0){
        #获取所有的变量变量
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( keyNo %in% vars){
          #需要处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = keyNo)
          length_value <- as.numeric(length_value)
          #针对值进行处理
          r[i,'FLength'] <- length_value
        }

      }
      #针对L翻进行处理
      ltab <-tsdo::na_replace(r[i,'FLtab'],'')
      if(tsdo::len(ltab)){
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( ltab %in% vars){
          #针对数据处理处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = ltab)
          print(length_value)
          r[i,'FLtab'] <- length_value



        }

      }
      #针对数量进行处理
      fqty <- tsdo::na_replace(r[i,'FQty'],"")
      if(tsdo::len(fqty)){
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( fqty %in% vars){
          #针对数据处理处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = fqty)
          length_value <- as.numeric(length_value)
          r[i,'FQty'] <- length_value



        }

      }




    }

    #数据已经处理完了
    r$FQty <- as.numeric(r$FQty)
    r$FLength <- as.numeric(r$FLength)
    r$FTotalQty <- r$FQty * r$FLength
    r$FParamG <- FGtab
    r$FParamL <- FLtab
    #针对空行进行处理,删除空行
    r <- r[!is.na(r$FQty),]
    print(r)
    #写入数据库
    try({
      dm_bom_deleteDB(conn = conn,FchartNo = FchartNo,FGtab = FGtab,FLtab = FLtab)
    })

    #写入数据库
    print('DB')
    tsda::upload_data(conn,'t_lcrds_bom',r)


  }else{
    r<-NA
  }

  return(r)



}

#' 获取待处理的图号
#'
#' @param conn 连接
#'
#' @return 返回主图事情
#' @export
#'
#' @examples
#' dm_getToDoChartNo()
dm_getToDoChartNo <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("select FchartNo from t_lcrds_UploadLog where FIsDo = 0")
  data <- tsda::sql_select(conn,sql)
  ncount <-nrow(data)
  if(ncount>0){
    res <- data$FchartNo
  }else{
    res <-NA
  }
  return(res)
}


#' 更新所有BOM
#'
#' @param conn 连接
#' @param show_process 是否显示进度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_dealAll()
dm_dealAll <- function(conn=tsda::conn_rds('lcrds'),show_process=FALSE) {

  chartNos <- dm_getToDoChartNo(conn)
  ncount <- length(chartNos)
  if(!is.na(chartNos)){
    #在存在数据的情况下处理
    #start

    if(show_process){

      withProgress(message = 'BOM运算处理中', value = 0, {
        for (i in 1:ncount) {
          FchartNo <-chartNos[i]

          FGtabs <-Gtab_get_uniqueMembers(conn,FchartNo)

          g_logical <- ! is.na(FGtabs[1])

          FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)

          l_logical <- ! is.na(FLtabs[1])
          #全部不为空时处理

          if( g_logical & l_logical){

            for (FGtab in FGtabs) {
              for (FLtab in FLtabs) {

                try(  dm_dealOne(conn,FchartNo,FGtab,FLtab))



              }

            }

          }



          #更新进度条
          incProgress(1/ncount, detail = paste("(",i,"/",ncount,")..."))
          #更新记录
          db_bom_setUpdate(conn,FchartNo)



        }

      })




    }else{

      for (FchartNo in chartNos) {

        FGtabs <-Gtab_get_uniqueMembers(conn,FchartNo)

        g_logical <- ! is.na(FGtabs[1])

        FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)

        l_logical <- ! is.na(FLtabs[1])
        #全部不为空时处理

        if( g_logical & l_logical){

          for (FGtab in FGtabs) {
            for (FLtab in FLtabs) {

              try(  dm_dealOne(conn,FchartNo,FGtab,FLtab))



            }

          }

        }





      #更新记录
        db_bom_setUpdate(conn,FchartNo)


      }

    }




    #end
  }



}



#' 写入数据库
#'
#' @param conn 连接
#' @param show_process 是否显示进度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_dealAll2()
dm_dealAll2 <- function(conn=tsda::conn_rds('lcrds'),show_process=FALSE) {
  chartNos <- dm_getToDoChartNo(conn)
  ncount <- length(chartNos)
  if(!is.na(chartNos)){
    #在存在数据的情况下处理
    #start

    if(show_process){

      withProgress(message = 'BOM运算处理中', value = 0, {
        for (i in 1:ncount) {
          FchartNo <-chartNos[i]
          #写入数据
          dm_writeDB_ChartNo(conn=conn,FchartNo = FchartNo)

          #更新进度条
          incProgress(1/ncount, detail = paste("(",i,"/",ncount,")..."))
          #更新记录
          db_bom_setUpdate(conn,FchartNo)



        }

      })




    }else{

      for (FchartNo in chartNos) {

        #写入数据
        print('step04')
        dm_writeDB_ChartNo(conn=conn,FchartNo = FchartNo)
        print('step05')

        #更新记录
        db_bom_setUpdate(conn,FchartNo)
        print('step06')


      }

    }




    #end
  }



}


#' 查询信息
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FParamG G番
#' @param FParamL L番
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_selectDB_detail()
dm_selectDB_detail <- function(conn=tsda::conn_rds('lcrds'),FchartNo ='P235009B198', FParamG ='G11'  , FParamL ='L42') {

  #针对NA做兼容性处理
  # if (FParamL == ''){
  #   FParamL <- NA
  # }
  if(is.na(FParamL)){
    sql <- paste0("select distinct
FchartNo 主图号
,FParamG [G番号-参数]
,'' as  [L番号-参数]
,FItemName 子项名称
,FSubChartNo 分图号
,FkeyNo 子项件号
,FLtab 子项L番
,FItemModel 子项规格
,FNote 子项备注
,FIndexTxt 子项序号
,FQty 子项基本数量
,FLength [子项长度/系数]
,FTotalQty 子项总数量
from t_lcrds_bom
where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  order by FIndexTxt")
  }else{
    sql <- paste0("select
FchartNo 主图号
,FParamG [G番号-参数]
,FParamL [L番号-参数]
,FItemName 子项名称
,FSubChartNo 分图号
,FkeyNo 子项件号
,FLtab 子项L番
,FItemModel 子项规格
,FNote 子项备注
,FIndexTxt 子项序号
,FQty 子项基本数量
,FLength [子项长度/系数]
,FTotalQty 子项总数量
from t_lcrds_bom
where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  and FParamL ='",FParamL,"'order by FIndexTxt")
  }


  res <- tsda::sql_select(conn,sql)
  return(res)

}





#' 读取DM清单
#'
#' @param file 文件
#' @param sheet 页答名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_read_list()
dm_read_list <- function(file="data-raw/bom_src4.xlsx",sheet = "DM清单"){

  data <- readxl::read_excel(file,
                         sheet = sheet)
  col_names_selected <-c("DM单号","级数","上级行号","物料号","名称","用量数","图号","G番","L番")
  data <- data[,col_names_selected]
  class(data) <-'data.frame'
  return(data)
}


#' 查询所有清单
#'
#' @param file 文件
#' @param sheet 页答
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_queryAll()
dm_queryAll <-function(file="data-raw/bom_src4.xlsx",sheet = "DM清单",conn=tsda::conn_rds('lcrds')){
  data <- dm_read_list(file=file,sheet = sheet)
  ncount <- nrow(data)
  if(ncount >0){
    res <- lapply(1:ncount, function(i){
      item <- data[i,]
      FchartNo =  data[i,"图号"]
      FParamG = data[i,"G番"]
      FParamL = data[i,"L番"]
      print(FParamL)
      #仅仅支持单个L类番进行查询
      #r <- dm_selectDB_detail(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL =FParamL)
      #用于支持多个L番进行进行
      print('start of multiple L query')
      print(FchartNo)
      print(FParamG)
      print(FParamL)
      r <- dm_selectDB_detail2(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL =FParamL)
      print(r)
      ncount_item <- nrow(r)
      if(ncount_item >0){
        item_1 <-tsdo::df_rowRepMulti(item,ncount_item)
        item_res <- cbind(item_1,r)
      }else{

        FchartNo2 = FchartNo
        FParamG2 = FParamG
        FParamL2 = FParamL
        FItemName = ""
        FSubChartNo =""
        FkeyNo  =""
        FLtab =""
        FItemModel =""
        FNote =""
        FIndexTxt =""
        FQty = 0
        FLength =0
        FTotalQty = 0
        item2 <- data.frame(FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
                            FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,stringsAsFactors = F)
        names(item2) <-c("主图号", "G番号-参数","L番号-参数","子项名称", "分图号","子项件号",
                          "子项L番", "子项规格", "子项备注", "子项序号", "子项基本数量" , "子项长度/系数",
                          "子项总数量")

        item_res <- cbind(item,item2)


      }
      return(item_res)
    })
    data2 <- do.call('rbind',res)

    return(data2)


  }

}




#' 判断G翻是否可以扩展
#'
#' @param x 向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_G_extendable()
dm_G_extendable <- function(x){
  res <- lapply(x, function(i){
    if (is.na(i)){
      value <- ""
    }else{
      value <- i
    }
    r <- tsdo::left(value) =='G'
    r <- as.integer(r)
    return(r)
  })
  info <- unlist(res)
  return(info)
}


#' 查询DM清单数据
#'
#' @param file 文件
#' @param sheet 页答
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_queryAll2()
dm_queryAll2 <-function(file="data-raw/bom_src4.xlsx",sheet = "DM清单",conn=tsda::conn_rds('lcrds')){

  data <- try({
    dm_queryAll(file = file,sheet = sheet,conn = conn)
  })
  names(data) <-c('FDmNo',
                  'FLevel',
                  'FParentRowNo',
                  'FParentItemNo',
                  'FParentItemName',
                  'FParentQty',
                  'FParentChartNo',
                  'FParentGNo',
                  'FParentLNo',
                  'FchartNo2','FParamG2','FParamL2','FItemName','FSubChartNo','FkeyNo','FLtab',
                  'FItemModel','FNote','FIndexTxt','FQty','FLength','FTotalQty'
)
  #针对数据进行判断
  data$FExtendable <- dm_G_extendable(data$FkeyNo)
  #完成针对子项的打标数据
  #针对不需要扩展的字段设置已完成处理
  data$FIsDo  <-0
  data$FIsDo[data$FExtendable == 0] <- 1
  #针对第一次查询,添加相应的上级数据数量
  data$FTotalQty <-data$FParentQty * data$FTotalQty
  #针对数据处理处理，针对数量为0也不需要进行展开
  data$FIsDo[data$FQty == 0] <- 1
  data$FIsDo[data$FTotalQty == 0] <- 1


  data$FExtendSeq <- 0
  data$FSubRowNo <-data$FParentRowNo
  #列示所有列名称
  #print(names(data))


  return(data)

}


#' 写入数据库
#'
#' @param file 文件
#' @param sheet 页签
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_queryAll_writeDB()
dm_queryAll_writeDB <- function(file="data-raw/bom_src4.xlsx",sheet = "DM清单",conn=tsda::conn_rds('lcrds')) {
  data <- try(dm_queryAll2(file=file,sheet = sheet,conn=conn))
  #写入数据库
  ncount = nrow(data)
  if(ncount >0)
  {
    #写入数据库
    tsda::db_writeTable(conn=conn,table_name = 't_lcrds_dmInput',r_object = data,append = TRUE)
  }


}

#' 判断是否需要处理
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_need_toDo()
dmList_need_toDo <- function(conn=tsda::conn_rds('lcrds')){
  sql <- paste0("select 1 as count
				   from vw_lcrds_dmInput_toDo2")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    info <- TRUE
  }else{
    info <- FALSE
  }
  return(info)
}


# 完善针对数据库的修改数量必须大于0---

# alter view vw_lcrds_dmInput_toDo
# as
# select
# FDmNo,                   FLevel+1 as FLevel,                   FParentRowNo,                   FParentItemNo,                   FParentItemName,                   FParentQty,                   FParentChartNo,                   FParentGNo,
# FParentLNo,                   FSubChartNo as FchartNo2,
# FKeyNo as FParamG2,
# FLtab as FParamL2,
# FIndexTxt,
# row_number() over (partition by FDmNo,FLevel,FParentRowNo
#                    order by FDmNo,FLevel,FParentRowNo,FIndexTxt) as FExtendSeq
# from t_lcrds_dminput where fisdo =0 and fextendable =1   and FQty >0




#针对第一次数据，没有进行更新展开，我们后续要处理多级展开的问题
#' 多次读取待处理的数据
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_toDo_Multi()
dmList_toDo_Multi <- function(conn=tsda::conn_rds('lcrds')){
  #添加上一级的TotalQty,用于向下一级传递
  sql <- paste0("select FDmNo,
                  FLevel,
                   FParentRowNo,
                  FParentItemNo,
                  FParentItemName,
                  FParentQty,
                  FParentChartNo,
                  FParentGNo,
                  FParentLNo,
                   FchartNo2,
				  FParamG2,
				   FParamL2,
				   FTotalQty
				   from vw_lcrds_dmInput_toDo2")
  res <- tsda::sql_select(conn,sql)
  return(res)
}

#check the wherethe is do is found
#' 更新序号及下级代码，方便后续进行排序查询
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_updateSubRowNo_Multi()
dmList_updateSubRowNo_Multi <- function(conn=tsda::conn_rds('lcrds')){
  sql <- paste0("update a set  a.FExtendSeq = b.FExtendSeq,a.fsubRowNo = b.FParentRowNo   from t_lcrds_dminput a
inner join  vw_lcrds_dmInput_toDo2 b
on a.FDmNo = b.FDmNo
and  a.FSubChartNo = b. FchartNo2
and a.FKeyNo =b. FParamG2
and a.FLtab =b. FParamL2
where a.fisdo =0 and a.fextendable =1")
  res <- tsda::sql_update(conn,sql)

}


#' 多次处理相关数据
#'
#' @param conn 连接
#' @param data  处理数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_queryAll_Multi()
dmList_queryAll_Multi <-function(conn=tsda::conn_rds('lcrds'),data){
  #读取待处理数据
  #将数据进行隔离，每个只处理一件事项
  #data <- dmList_toDo_Multi(conn=conn)
  ncount <- nrow(data)
  if(ncount >0){
    res <- lapply(1:ncount, function(i){

      FchartNo =  data[i,"FchartNo2"]
      FParamG = data[i,"FParamG2"]
      FParamL = data[i,"FParamL2"]
      #上级代入数据
      FTotalQty_input =data[i,"FTotalQty"]
      r <- dm_selectDB_detail(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL =FParamL)
      names(r) <-c('FchartNo2','FParamG2','FParamL2','FItemName','FSubChartNo','FkeyNo','FLtab',
                   'FItemModel','FNote','FIndexTxt','FQty','FLength','FTotalQty')
      #应用上级代入数量
      r$FTotalQty <- r$FTotalQty*FTotalQty_input
      #针对数据进行处理
      #读取有限数据行，用于数据复制
      item <- data[i,1:9]
      ncount_item <- nrow(r)
      if(ncount_item >0){
        item_1 <-tsdo::df_rowRepMulti(item,ncount_item)
        item_res <- cbind(item_1,r)
      }else{

        FchartNo2 = FchartNo
        FParamG2 = FParamG
        FParamL2 = FParamL
        FItemName = ""
        FSubChartNo =""
        FkeyNo  =""
        FLtab =""
        FItemModel =""
        FNote =""
        FIndexTxt =""
        FQty = 0
        FLength =0
        FTotalQty = 0
        item2 <- data.frame(FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
                            FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,stringsAsFactors = F)

        item_res <- cbind(item,item2)


      }
      return(item_res)
    })
    data <- do.call('rbind',res)
    #针对数据进行判断
    data$FExtendable <- dm_G_extendable(data$FkeyNo)
    #完成针对子项的打标数据
    #针对不需要扩展的字段设置已完成处理
    data$FIsDo  <-0
    data$FIsDo[data$FExtendable == 0] <- 1

    data$FExtendSeq <- 0
    data$FSubRowNo <-data$FParentRowNo
    #列示所有列名称
    #print(names(data))
    #设置已经完成处理数据处理


    return(data)


  }

}

# 针对input处理显得非常重要
#' 更新数据库，设置已完成
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dbList_setDone()
dbList_setDone <- function(conn=tsda::conn_rds('lcrds')){
  #设置数据已处理
  sql <- paste0(" update  t_lcrds_dminput  set fisdo =1  where fisdo =0 and fextendable =1")
  tsda::sql_update(conn,sql)

}


dmList_writeDB_Multi <- function(conn=tsda::conn_rds('lcrds'),data) {
  #data <- try(dm_queryAll2(file=file,sheet = sheet,conn=conn))
  #
  #写入数据库
  ncount = nrow(data)
  if(ncount >0)
  {
    tsda::db_writeTable(conn=conn,table_name = 't_lcrds_dmInput',r_object = data,append = TRUE)
  }



}


#' 读取DM清单数据
#'
#' @param conn  连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_readDB_Input()
dmList_readDB_Input <- function(conn=tsda::conn_rds('lcrds')){
  sql <- paste0("select

FDmNo
      ,FLevel
      ,FParentRowNo
      ,FParentItemNo
      ,FParentItemName
      ,FParentQty
      ,FParentChartNo
      ,FParentGNo
      ,FParentLNo

,FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
                            FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty
from t_lcrds_dmInput
order by FDmNo,FSubRowNo,FLevel,FIndexTxt")
  res <- tsda::sql_select(conn,sql)
  return(res)
}

#' 提供DM清单的单个查询
#'
#' @param conn 连接
#' @param FDmNo DM单号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmQuery1_readDB()
dmQuery1_readDB <- function(conn=tsda::conn_rds('lcrds'),FDmNo = 'DMP235000B1561920') {

  sql <- paste0("select

FDmNo
      ,FLevel
      ,FParentRowNo
      ,FParentItemNo
      ,FParentItemName
      ,FParentQty
      ,FParentChartNo
      ,FParentGNo
      ,FParentLNo

,FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
                            FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty
from t_lcrds_dmlist
where FDmNo = '",FDmNo,"'
order by FDmNo,FSubRowNo,FLevel,FIndexTxt
")
  res <- tsda::sql_select(conn,sql)
  return(res)
}

#' 提供对DM清单的单个查询并返回中文名列表
#'
#' @param conn 连接
#' @param FDmNo 单个DM清单
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmQuery1_readDB_cn()
dmQuery1_readDB_cn <- function(conn=tsda::conn_rds('lcrds'),FDmNo = 'DMP235000B1561920') {
  data <- dmQuery1_readDB(conn=conn,FDmNo = FDmNo)
  n1 <-c("DM单号","级数","上级行号","物料号","名称","用量数","图号","G番","L番")


  n2 <-c("主图号", "G番号-参数","L番号-参数","子项名称", "分图号","子项件号",
         "子项L番", "子项规格", "子项备注", "子项序号", "子项基本数量" , "子项长度/系数",
         "子项总数量")
  name_all <- c(n1,n2)

  names(data) <- name_all
  return(data)
}


#' 读取DM数据,提供中文名
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_readDB_Input_cn()
dmList_readDB_Input_cn <- function(conn=tsda::conn_rds('lcrds')){

  data <- dmList_readDB_Input(conn=conn)
  n1 <-c("DM单号","级数","上级行号","物料号","名称","用量数","图号","G番","L番")


  n2 <-c("主图号", "G番号-参数","L番号-参数","子项名称", "分图号","子项件号",
    "子项L番", "子项规格", "子项备注", "子项序号", "子项基本数量" , "子项长度/系数",
    "子项总数量")
  name_all <- c(n1,n2)

  names(data) <- name_all
  return(data)
}




#' 针对数据进行处理
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_data_sync()
dmList_data_sync <- function(conn=tsda::conn_rds('lcrds')){
  #备份数据
  #如果存在亲的数据即进行备份
  sql_bak <- paste0("insert into t_lcrds_dmlistDel
select * from  t_lcrds_dmlist
where FDmNo in
(
select distinct FDmNo from t_lcrds_dmInput)")
tsda::sql_update(conn,sql_bak)
#删除旧的数据
#删除旧有的数据
sql_del <- paste0("delete  from  t_lcrds_dmlist
where FDmNo in
(
select distinct FDmNo from t_lcrds_dmInput)")
 tsda::sql_update(conn,sql_del)
 #写入数据
 sql_ins <- paste0("insert into   t_lcrds_dmlist
select *  from t_lcrds_dmInput")
 tsda::sql_update(conn,sql_ins)
 #删除Input数据
 sql_input <- paste0("delete  from t_lcrds_dmInput")
 tsda::sql_update(conn,sql_input)


}

#多级展开的核心逻辑--------

#' 针对上传的DM清单进行多级展开的入口函数
#'
#' @param file 文件
#' @param sheet 页签
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_Expand_Multi()
dmList_Expand_Multi <- function(file="data-raw/bom_src4.xlsx",sheet = "DM清单",conn=tsda::conn_rds('lcrds')){
  #读取数据,针对容错处理
  #初始化第0，1级展开
  #已经开始写入数据库t_lcrds_dmInput
  try(dm_queryAll_writeDB(file = file,sheet = sheet,conn = conn))
  #进行多级展开
  #判断是否需要多级情节
  #定义了需要处理的视图
  while (dmList_need_toDo(conn=conn)) {
    #读取待处理数据
    #设置待处理数据
    #判断是否需要待处理数据
    data_todo <- dmList_toDo_Multi(conn=conn)
    #设置子项序号
    #设置字段序号
    dmList_updateSubRowNo_Multi(conn=conn)
    #设置数据处理
    data_done <- dmList_queryAll_Multi(conn = conn,data=data_todo)
    #设置数据处理已完成
    #获取已经处理的数据
    dbList_setDone(conn = conn)
    #写入数据库
    #向数据库t_lcrds_dmInput追加写入
    dmList_writeDB_Multi(conn = conn,data = data_done)

  }
  #针对数据进行查询
  #针对数据进行处理
  data <-dmList_readDB_Input_cn(conn=conn)
  #数据库查询完成中文
  #迁移进入dmlist及dmlistDel
  dmList_data_sync(conn=conn)
  #返回数据
  return(data)


}




#' 针对DM清单及BOM进行混合查询
#'
#' @param file 文件
#' @param sheet 页签
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmQuery_Batch_file()
dmQuery_Batch_file <- function(file="data-raw/DM配件混合查询.xlsx",sheet = "DM配件混合查询",conn=tsda::conn_rds('lcrds')) {

  param_input <- readxl::read_excel(file,sheet = sheet)
  ncount <- nrow(param_input)
  res <- lapply(1:ncount, function(i){
    FDmNo <- tsdo::na_replace(param_input[i,"DM单号",drop=TRUE],'')
    FchartNo <-param_input[i,"图号",drop=TRUE]
    FParamG <- param_input[i,"G番",drop=TRUE]
    FParamL <- tsdo::na_replace(param_input[i,"L番",drop=TRUE],'')
    print(FDmNo)
    if(tsdo::len(FDmNo) >0){
      #使用DM清单处理
      res<- dmQuery1_readDB_cn(conn = conn,FDmNo = FDmNo)
    }else{
      #使用图号处理
      res <-dm_selectDB_detail_combo1(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL)
    }
    return(res)
  })
  res2 <- do.call('rbind',res)
  return(res2)

}











