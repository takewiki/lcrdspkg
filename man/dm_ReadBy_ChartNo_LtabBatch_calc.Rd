% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataStat.R
\name{dm_ReadBy_ChartNo_LtabBatch_calc}
\alias{dm_ReadBy_ChartNo_LtabBatch_calc}
\title{获取相当数据}
\usage{
dm_ReadBy_ChartNo_LtabBatch_calc(
  conn = tsda::conn_rds("lcrds"),
  FchartNo = "SE304A200",
  FGtab = "G01",
  FLtabBatch = "L07",
  page_size = 300
)
}
\arguments{
\item{conn}{连接}

\item{FchartNo}{图号}

\item{FGtab}{G番}

\item{FLtabBatch}{L号}

\item{page_size}{每页大小}
}
\value{
返回值
}
\description{
获取相当数据
}
\examples{
dm_ReadBy_ChartNo_LtabBatch_calc()
}
