regression_recode_factor_1<-function(varname, dt, count_threshold=1) {
  var<-dt[[varname]]
  mylabel<-danesurowe::GetVarLabel(dt, varname)
  levels<-as_tibble(table(var)) %>% filter(n>count_threshold)
  base_level<-arrange(levels, -n)$var[[1]]
  levels<-levels[2:nrow(levels),]
  newname<-paste0(varname,'_', padded_count(1, nrow(levels)))
  newvar<-as.integer(var==levels$var[[1]])
  newvar[is.na(newvar)]<-0
  ans<-data.table(var=newvar)
  data.table::setnames(ans, 'var',  newname)
  setattr(ans[[newname]], 'label', paste0(mylabel,' ',levels$var[[1]] ))

  for(i in seq(2, nrow(levels))) {
    newname<-paste0(varname,'_', padded_count(i, nrow(levels)))
    newvar<-as.integer(var==levels$var[[i]])
    newvar[is.na(newvar)]<-0
    ans[,(newname):=newvar]
    setattr(ans[[newname]], 'label', paste0(mylabel,' ',levels$var[[i]] ))
  }
  if(sum(is.na(var))>count_threshold) {
    newname<-paste0(varname,'_NA')
    ans[,(newname):=as.integer(is.na(var))]
    setattr(ans[[newname]], 'label', paste0('Braki danych w ',mylabel))
  }
  return(ans)
}

regression_recode_binary_1<-function(varname, dt, count_threshold=1) {
  var<-as.numeric(dt[[varname]])
  levels<-unique(var)
  levels<-levels[!is.na(levels)]
  mylabel<-danesurowe::GetVarLabel(dt, varname)
  newvar<-as.integer(var==levels[[2]])
  ans<-data.table(var=newvar)
  danesurowe::copy_var_attributes(dt[[varname]], var_dest_name = 'var', dt_dest = ans)
  data.table::setnames(ans, 'var',  varname)

  if(sum(is.na(var))>count_threshold) {
    newname<-paste0(varname,'_NA')
    ans[,(newname):=as.integer(is.na(var))]
    setattr(ans[[newname]], 'label', paste0('Braki danych w ',mylabel))
  }

  return(ans)
}


#Functions to create dataset where there is no nominal variables.
#flag_fix_NA - if set, the resulting dataframe will not have any NAs.
#flag_tolerate_ordinal - if set, we will treat ordinal variables as numeric. Otherwise we will
#treat it as nominal and disassembly into dummies

make_dummy_variables<-function(dt, flag_fix_NA=TRUE) {
  tolerated_fobs<- if (flag_tolerate_ordinal) {c(0,2)} else {0}
  if (flag_tolerate_factor) {tolerated_fobs <- c(tolerated_fobs, 1)}

  fobs<-map_dbl(args$zn, ~danesurowe::GetFOB(mydt[[.]], flag_recalculate_uniques = TRUE))

  ordinals<-args$zn[fobs==2]
  ans<-list()
  if(length(ordinals)>0) {
    a<-mydt %>% select_(.dots=ordinals) %>% map(as.integer) %>% as.data.table
    ans<-c(ans, a)
  }
  nominals<-args$zn[fobs==1]
  if(length(nominals)>0) {
    a<-flatten(purrr::map(nominals, regression_recode_factor_1, dt=mydt))
    ans<-c(ans,a)
  }
  binaries<-args$zn[fobs==3]
  if(length(binaries)>0) {
    a<-flatten(purrr::map(binaries, regression_recode_binary_1, dt=mydt))
    ans<-c(ans,a)
  }
  numerics<-args$zn[fobs==0]

  ans2<-as.data.table(c(ans, as.list(mydt %>% select_(.dots=c(numerics) ))))

  numdb<-mydt %>% select_(.dots=c(numerics, ordinals) )

  if(flag_fix_NA) {
    missing_pattern<-mice::md.pattern(numdb)
    imputes<-NULL
    if(nrow(missing_pattern)>2) {
      if(nrow(mydt)>40000) {
        m<-missing_pattern[nrow(missing_pattern),1:ncol(missing_pattern)-1]
        vars_miss<-names(m[m>0])
        ms<-list()
        for(colname in vars_miss) {
          var<-ans2[[colname]]
          m<-mean(var, na.rm = TRUE)
          imputes<-c(imputes, list(sumNA=sum(is.na(var))))
          var[is.na(var)]<-m
          ms<-c(ms, list(m))
          ans2[,(colname):=as.numeric(var)]
        }
        imputes<-data.frame(varname=vars_miss, missing_count=unlist(imputes), mean=unlist(ms))

      } else {
        library(mice)
        imp<-mice::mice(ans2)
        browser() #Należy teraz wykorzystać te dane, czyli włożyć je do bd
      }
    }
    mydt<-ans2
    a<-sum(map_dbl(mydt, ~sum(is.na(.))))
    if(a>0) {
      stop("Coś nie tak poszło z usuwaniem braków")
    }
  } else {
    mydt<-numdb
  }
  return(mydt)
}
