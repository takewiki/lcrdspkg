% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ltable.R
\name{Ltab_checkExist_one}
\alias{Ltab_checkExist_one}
\title{检查具体的某个L番是否存在}
\usage{
Ltab_checkExist_one(
  conn = tsda::conn_rds("lcrds"),
  FchartNo = "P203031B120",
  FLtab = "L04"
)
}
\arguments{
\item{conn}{连接}

\item{FchartNo}{主图号}

\item{FLtab}{具体的L番表}
}
\value{
返回值
}
\description{
检查具体的某个L番是否存在
}
\examples{
Ltab_checkExist_one()
}
