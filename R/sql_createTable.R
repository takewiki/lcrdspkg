# 增加条码同步的结构
#' 创建同步龙腾条码的表结构
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcode_sync_table()
barcode_sync_table <- function(conn=tsda::conn_rds('LCERP')) {
  sql <- paste0("create table t_rds_barcode_sync
(FBarcode nvarchar(200),
 FNote_ERP varchar(250),
 FNumber varchar(80),
 FChartNumber varchar(80),
 FBillNo varchar(30),
 FCreateDate datetime,
 FStatus int default(0))")
  tsda::sql_update(conn,sql)

}





#' 条码分配结果表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcode_allocated_res()
barcode_allocated_res <- function(){
  sql <- paste0("
CREATE TABLE [dbo].[takewiki_barcode_allocate_auto](
	[FSoNo] [varchar](50) NULL,
	[FChartNo] [varchar](200) NULL,
	FNote varchar(255),
	[FBarcode_ext] [varchar](1000) NULL,
	[FBarcode_inner] [varchar](1000) NULL,
	FCalcNo int
) ON [PRIMARY]
GO")
}

