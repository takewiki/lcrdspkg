res = dm_selectDB_detail2(FchartNo = 'SE304A200',FParamG = 'G01',FParamL = 'L04,L57,L63,L68,L72')
View(res)

dm_ReadBy_ChartNo_GL_dealOne(conn=tsda::conn_rds('lcrds'),
                             FchartNo='SE304A200',FParamG='G01',
                             FParamL ='L04,L57,L63,L68,L72,L81',
                             page_size = 300)


data = dm_ReadBy_ChartNo_LtabBatch_dealBom(conn=tsda::conn_rds('lcrds'),
                                           FchartNo='SE304A200',FParamG='G01',
                                           FParamL ='L04,L57,L63,L68,L72,L81,L92,L99',
                                           page_size = 300)
View(data)
