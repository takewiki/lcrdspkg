lcrdspkg::extBarcode_AllocateALL()



data_inner <-barcodeInner_getUnAllocated(conn = tsda::conn_rds('LCERP2'),
                                         FChartNo = 'P203031A112G01',
                                         FNote_ERP = '503-标配整合,G103,G108/G110/G407,G363*G168,',
                                         n = 4)
View(data_inner)

