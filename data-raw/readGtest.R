library(lcrdspkg)

Gtab_read(sheetName = 'YE604B339') ->bb
View(bb)
#读取标准化后的数据
mydata <-bb$data_norm

View(mydata)


Gtab_write_db(sheetName = 'YE604B339')

#批写处理

Gtab_batchWrite_db(exclude_sheetName = lc_exclude_sheetNames())


#测试数据

Gtab_select_db(FchartNo = 'YE601B241')


YE603A049



Gtab_is_new(FchartNo = 'P235067C156')

Gtab_delete_db(FchartNo = 'P235067C156')


#读取G表中的元素

Gtab_get_uniqueMembers()


