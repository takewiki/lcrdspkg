library(lcrdspkg)
library(readxl)
library(reshape2)

data_many <- Ltab_read(sheetName = 'YE604B797')



View(data_many)


#写入数据库
Ltab_write_db (sheetName = 'YE604B797')

#批量写入数据库
Ltab_batchWrite_db(exclude_sheetName = lc_exclude_sheetNames())

#Ltab_batchWrite_db(conn = conn,file = file,exclude_sheetName = lc_exclude_sheetNames(),show_progress = TRUE)





data_null <- Ltab_read(sheetName = 'P235000B156')

class(data_null)
nrow(data_null)

View(data_null)

data_small <- Ltab_read(sheetName = 'SYE601B672')

View(data_small)


Ltab_select_db()


#L表中的变量
Ltab_get_uniqueVars()
#L表中的元素

Ltab_get_uniqueMembers()

Ltab_get_varValue(FLtab = 'L10')

lc_exclude_sheetNames()






