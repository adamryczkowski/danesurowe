#TODO: Refactor common.R, tak aby wyodrębnić kod przekształcający zbiór danych osobno od kodu zberającego metadane.
#      Przepisz to tak, aby maksymalnie dużo dało się tego użyć w danesurowe.
#
#


library(danesurowe)

folder2<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/EwaG/02 31III2018'
folder1<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/EwaG/01 31III2018'
plik2<-pathcat::path.cat(folder2, 'dane surowe1.xlsm')
plik1<-pathcat::path.cat(folder2, 'dane surowe gr.xls')
plik3<-pathcat::path.cat(folder1, 'dane surowe gr (pomiary).xls')


dt<-readDaneSurowe1(plik3)

zmA<-paste0('pom_A_I_', 1:8)
zmB<-paste0('pom_B_I_', 1:8)
zmC<-paste0('pom_C_I_', 1:8)

zm<-c(zmA, zmB, zmC)

dt %>% select_at(c('grupa', 'q_9', zmA)) %>% mutate(a = ifelse(q_9=='Prawa', !!expr_text('pom_A_I_1'), pom_A_I_2))

mydt<-data.frame(dt)[c('grupa','q_9', zm)]
sufix<-c(rep('A', 4), rep('B', 4), rep('C', 4))
library(rlang)
for(i in seq_along(sufix)) {
  mydt[,(paste0('pom',sufix[[i]], '_', i)):=ifelse(dt$q_9=='Prawa', mydt[[(i-1)*2+3]], mydt[[(i-1)*2+4]])]
}

mydtA %>% group_by(grupa) %>% summarize_all(~mean(., na.rm=TRUE))
