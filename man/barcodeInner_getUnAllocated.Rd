% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intBarCode.R
\name{barcodeInner_getUnAllocated}
\alias{barcodeInner_getUnAllocated}
\title{查询未分配的记录}
\usage{
barcodeInner_getUnAllocated(
  conn = tsda::conn_rds("LCERP"),
  FChartNo = "P203042B143G02",
  FNote_ERP = "",
  n = 10
)
}
\arguments{
\item{conn}{连接}

\item{FChartNo}{图号}

\item{FNote_ERP}{ERP备注}

\item{n}{返回数据}
}
\value{
返回值
}
\description{
查询未分配的记录
}
\examples{
barcodeInner_getUnAllocated()
}
