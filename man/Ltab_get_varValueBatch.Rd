% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ltable.R
\name{Ltab_get_varValueBatch}
\alias{Ltab_get_varValueBatch}
\title{获取L表的变量值批量}
\usage{
Ltab_get_varValueBatch(
  conn = tsda::conn_rds("lcrds"),
  FchartNo = "SE304A200",
  FLtabBatch = "L33,L67"
)
}
\arguments{
\item{conn}{连接}

\item{FchartNo}{主图号}

\item{FLtabBatch}{可能多L番}
}
\value{
返回具有数值
}
\description{
获取L表的变量值批量
}
\examples{
Ltab_get_varValueBatch()
}
