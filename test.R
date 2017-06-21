#TODO: Refactor common.R, tak aby wyodrębnić kod przekształcający zbiór danych osobno od kodu zberającego metadane.
#      Przepisz to tak, aby maksymalnie dużo dało się tego użyć w danesurowe.
#
#


library(danesurowe)


#df1_key<-'q_0' df2_key<-NULL
df2_all<-danesurowe::readDaneSurowe4('/home/Adama-docs/Adam/MyDocs/praca/masia@ibib/ALSdatabase/data/dane surowe2d.xlsm', flag_keep_tagged_na = TRUE)
df2<-df2_all$dt

#df2<-danesurowe::readDaneSurowe3('/home/Adama-docs/Adam/MyDocs/praca/masia@ibib/ALSdatabase/data/dane surowe2c.xlsm')
df1<-danesurowe::readDaneSurowe3('/home/Adama-docs/Adam/MyDocs/praca/masia@ibib/ALSdatabase/data_review/corrections/als (2)_Lisbon reviewed_marta_1march.xlsm')
diffdb<-df_difference(df1 = df1, df2 = df2, df1_key = 'q_0', df2_key='q_0')
mydiffs<-diffdb$diffdb[status!=0,]
mydiffs<-diffdb$diffdb[!is.na(df2_value) & !is.na(df1_value),]
ans<-danesurowe::comment_diffs(diffdb = mydiffs, df1 = diffdb$df1, df1keys = diffdb$df1keys, df2 = diffdb$df2, df2keys = diffdb$df2keys)


corfile<-'/home/adam/Documents/praca/masia@ibib/ALSdatabase/data_review/corrections/als (2)_Lisbon reviewed_marta_1march.xlsm'
cordt<-danesurowe::readDaneSurowe3(corfile)

origfile<-'/home/adam/Documents/praca/masia@ibib/ALSdatabase/data/dane surowe2d.xlsm'
origdt<-danesurowe::readDaneSurowe4(origfile)

comparedf(cordt, origdt$dt)




file<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/ThinVB/RDep/dane surowe2c.xlsm'
file<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/ThinVB/RDep/dane surowe2d.xlsm'
#debugonce(readDaneSurowe)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)
dt<-danesurowe::readDaneSurowe(file)




dt<-danesurowe::readDaneSurowe('/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/R/danesurowe/tests/testthat/dane surowe2c.xlsm')
#namedRange<-'CustomerFolder'
namedRange<-'SubcustomerFolder'
namedRange<-'DBLongName'
namedRange<-'RObjName'
namedRange<-'Units'
namedRange<-'DataOrigin'

address<-danesurowe::getNamedRange(file, namedRange)

danesurowe::readCellAtAddress(address)

danesurowe::readCellAtAddress(address)

dt<-danesurowe:::readDataSheet(file)
danesurowe::readLabelSheet(file,dt)



mean((df1 %>% filter(q_16b=='Yes'))$q_52b,na.rm=TRUE)
sd((df1 %>% filter(q_16b=='Yes'))$q_52b,na.rm=TRUE)
min((df1 %>% filter(q_16b=='Yes'))$q_52b,na.rm=TRUE)
max((df1 %>% filter(q_16b=='Yes'))$q_52b,na.rm=TRUE)
sum(!is.na((df1 %>% filter(q_16b=='Yes'))$q_52b))

mean((df1 %>% filter(q_16a=='Yes'))$q_52b,na.rm=TRUE)
sd((df1 %>% filter(q_16a=='Yes'))$q_52b,na.rm=TRUE)
min((df1 %>% filter(q_16a=='Yes'))$q_52b,na.rm=TRUE)
max((df1 %>% filter(q_16a=='Yes'))$q_52b,na.rm=TRUE)
sum(!is.na((df1 %>% filter(q_16a=='Yes'))$q_52b))

mean((df1 %>% filter(q_16c=='Yes' | q_16d=='Yes' | q_16e=='Yes' | q_16f=='Yes' | q_16g=='Yes'))$q_52b,na.rm=TRUE)
sd((df1 %>% filter(q_16c=='Yes' | q_16d=='Yes' | q_16e=='Yes' | q_16f=='Yes' | q_16g=='Yes'))$q_52b,na.rm=TRUE)
min((df1 %>% filter(q_16c=='Yes' | q_16d=='Yes' | q_16e=='Yes' | q_16f=='Yes' | q_16g=='Yes'))$q_52b,na.rm=TRUE)
max((df1 %>% filter(q_16c=='Yes' | q_16d=='Yes' | q_16e=='Yes' | q_16f=='Yes' | q_16g=='Yes'))$q_52b,na.rm=TRUE)
sum(!is.na((df1 %>% filter(q_16c=='Yes' | q_16d=='Yes' | q_16e=='Yes' | q_16f=='Yes' | q_16g=='Yes'))$q_52b))

