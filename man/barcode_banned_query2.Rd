% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barcodeBan.R
\name{barcode_banned_query2}
\alias{barcode_banned_query2}
\title{查询已经被禁用的条码}
\usage{
barcode_banned_query2(
  conn = tsda::conn_rds("LCERP2"),
  FStartDate = "2021-01-01",
  FEndDate = "9999-12-31",
  FChartNumber = "YX201B497-02"
)
}
\arguments{
\item{conn}{连接}

\item{FStartDate}{开始日期}

\item{FEndDate}{结束日志}

\item{FChartNumber}{图号}
}
\value{
返回值
}
\description{
查询已经被禁用的条码
}
\examples{
barcode_banned_query()
}
