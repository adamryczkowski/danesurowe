#TODO: Refactor common.R, tak aby wyodrębnić kod przekształcający zbiór danych osobno od kodu zberającego metadane.
#      Przepisz to tak, aby maksymalnie dużo dało się tego użyć w danesurowe.
#
#


library(danesurowe)

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
