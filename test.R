file<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/ThinVB/RDep/dane surowe2c.xlsm'
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
