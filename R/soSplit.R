
#' 销售订单处理业务流程
#'
#' @param x 图号带~
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soSplit()
soSplit <- function(x) {
   res_split <-stringr::str_split(x,'~')
   res <- character(length(res_split))

   lapply(1:length(res_split), function(i){
     res[i] <<- res_split[[i]][1]

   })
   return(res)

 }
