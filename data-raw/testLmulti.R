mydata2 <-dmList_Expand_Multi(file="data-raw/bom_L_multi.xlsx",
                    sheet = "DM清单")
View(mydata2)



dm_queryAll_writeDB(file="data-raw/bom_L_multi.xlsx",
                    sheet = "DM清单")



dm_queryAll2(file="data-raw/bom_L_multi.xlsx",
             sheet = "DM清单")


dm_queryAll(file="data-raw/bom_L_multi.xlsx",
            sheet = "DM清单")

FchartNo = "DMP235000B156"
FParamG = "G01"
FParamL =NA
r <- dm_selectDB_detail2(FchartNo = FchartNo,FParamG = FParamG,FParamL =FParamL)
print(r)

#dm_queryAll

mydata <- dm_read_list(file="data-raw/bom_L_multi.xlsx",
             sheet = "DM清单")
View(mydata)






#测试


FchartNo = 'DMP235000B156'
FParamG = 'G01'
FParamL = ''

r <- dm_selectDB_detail2(FchartNo = FchartNo,FParamG = FParamG,FParamL =FParamL)
View(r)



