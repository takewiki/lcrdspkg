#' 针对BOM数据进行拆分处理
#'
#' @param mtrl_multiple_G 待处理的物料包含多个G
#' @param maxId 最大的内码号
#'
#' @return 返回一个处理后的数据框
#' @export
#'
#' @examples
#' bom_split()
bom_split <- function(mtrl_multiple_G= c('P203031A112G08G02G01G21G34G10G06G44G31','P203031A112G08G02G01G21G34G10G06G44G32'),
                      maxId=0) {

  aa <- mtrl_multiple_G
  #获取物料长度
  ncount_aa <- length(aa)
  #按字母G拆开,结果为list
  bb <-strsplit(aa,'G')
  #针对结果添加名称
  names(bb) <- aa
  #针对数据进行分行处理
  mydata <- lapply(1:ncount_aa, function(i){
    #针对每一行数据进行处理
    data <- bb[[i]]
    data_head <- aa[i]
    ncount <- length(data)
    if(ncount >1){
      data_share <- data[1]
      data_entry <- data[2:ncount]
      ncount_entry <- length(data_entry)
      data_body <-paste0(data_share,'G',data_entry)
      data_head <- rep(data_head,ncount_entry)
      idx = i + maxId
      data_interId <- rep(idx,ncount_entry)
      res <-data.frame(FInterId=data_interId,FParentItemNo=data_head,FEntryId=1:ncount_entry,FSubItemNo=data_body,stringsAsFactors = F)
      return(res)

    }


  })
  #针对所有行进行合并

  mydata2 <- do.call('rbind',mydata)
  #返回结果

 return(mydata2)

}
