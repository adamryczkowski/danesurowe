#' This function reads the database in file,
#' and puts returns its contents.
#'
#' @export

getVersion<-function(file) {
	ans<-tryCatch(addr<-danesurowe::getNamedRange(file, getOption('rng_Version')),
								error=function(e) e)
  if('error' %in% class(ans)) {
  	val<-3
  }else{
  	val<-suppressWarnings(readSingleCellAtAddress(addr))
  }
  return(as.numeric(val))

}

readDaneSurowe<-function(file) {
  ver<-getVersion(file)
  if (identical(ver, 3))
  {
    return(readDaneSurowe3(file))
  }
  if (identical(ver, 4))
  {
    return(readDaneSurowe4(file))
  }
}

readDaneSurowe3 <- function(file) {
  ans<-readDataSheet(file)
  ans$dt[,(1):=NULL] #Remove the first 'lp' column
#  if (length(na.colnames)>0)
#  {
#    na.colnames<-na.colnames-1 #Because we deleted the first column with rownames
#  }

  dt<-ans$dt

  readLabelSheet3(file,dt, ncol(dt)*2+2)
  readMeasureAndUnits(file,dt)
  readVariableGroups(file, df)

  address<-danesurowe::getNamedRange(file, getOption('rng_DBLongName'))
  globalname<-readSingleCellAtAddress(address)
  setattr(dt,'label',globalname)

  address<-danesurowe::getNamedRange(file, getOption('rng_CustomerFolder'))
  customerfolder<-readSingleCellAtAddress(address)

  address<-danesurowe::getNamedRange(file, getOption('rng_SubcustomerFolder'))
  subcustomerfolder<-readSingleCellAtAddress(address)

  paths<-c(getOption('globalPathPrefix'),customerfolder, subcustomerfolder)

  setattr(dt,'paths',paths)
  #  browser()
  for(i in rev(ans$na.colnames))
  {
    dt[,(i):=NULL]
  }
  setattr(dt,'path', file)
  return(dt)
}


readDaneSurowe4 <- function(file, flag_keep_tagged_na = FALSE, flag_add_warnings=FALSE, flag_keep_empty=TRUE) {
  ans<-readDataSheet(file)
  txt<-colnames(ans$dt)
  na.colnames<-c(ans$na.colnames,which(stringi::stri_count(txt, regex='^\\..*$')==1))
  validation_pos<-which(stringi::stri_count(txt, regex='^\\.validation.*$')==1)
  validation_names<-rep(NA_character_, length(txt))
  for(i in seq_along(validation_pos)) {
    pos<-validation_pos[[i]]
    for(j in seq(pos-1, 1)){
      if(j>=1) {
        if(txt[[j]]!='.validation') {
          newname<-txt[[pos]]
          if(newname=='.validation') {
            newname<-paste0('.validation_', txt[[j]], '_', pos-j)
          }
          tryname<-newname
          k<-1
          while(newname %in% c(na.colnames, validation_names )) {
            tryname<-paste0(newname, '_', k)
            k<-k+1
          }
          validation_names[[i]]<-tryname
          break
        }
      } else {
        break
      }
    }
  }
  validation_pos<-validation_pos[!is.na(validation_names)]
  validation_names<-validation_names[!is.na(validation_names)]
  colnames(ans$dt)[validation_pos]<-validation_names

  na.colnames<-c(ans$na.colnames,which(stringi::stri_count(txt, regex='^\\..*$')==1))
  validation_pos<-which(stringi::stri_count(txt, regex='^\\.validation.*$')==1)


  labels<-readLabelSheet4(file, ncol(ans$dt))

  types<-readTypes(file, ncol(ans$dt), flag_keep_empty=flag_keep_empty)
#	browser()
  ans<-set_apply_labels(dt = ans$dt, labels=labels, vartypes = types, flag_keep_tagged_na=flag_keep_tagged_na)
  dt<-ans$dt

  errors<-ans$errors
  mywarnings<-ans$warnings

  ans<-readMinMax(file, dt)
  dt<-set_TheoreticalMinMax(dt, ans$mins, ans$maxs)

  forceIntegers<-readForceInteger(file, ncol(dt))
  dt<-set_ForcedIntegers(dt, forceIntegers)

  dt<-ValidateTypes(dt)

  required<-readRequired(file, ncol(dt))
  dt<-set_Required(dt, required)

 # browser()
  ints<-readLimitedToLabels(file, ncol(dt))
  dt<-set_LimitToLabels(dt, ints)


  xlsformulas<-readXLSFormulas(file, ncol(dt))
  rformulas<-readRFormulas(file, ncol(dt))
  dt<-set_Formulas(dt, xlsformulas=xlsformulas, rformulas=rformulas)

  dt <- ValidateCustom(dt)

#  browser()
  if(!flag_keep_empty) {
    vars_to_keep <- setdiff(colnames(dt)[types == '0'], validation_names)
    for(varnr in rev(vars_to_keep))
    {
      if(sum(!is.na(dt[[varnr]]))==0) {
        dt[,(varnr):=NULL]
      }
    }
  }
  setattr(dt,'path', file)

  if(!flag_add_warnings) {
    warnings<-rep('',  length(dt))
    for(i in seq_along(colnames(dt))) {
      cname<-colnames(dt)[[i]]
      warnings[[i]]<-paste0(attr(dt[[cname]], 'warnings'), collapse = '\n')
      setattr(dt[[cname]],'warnings', NULL)
    }
  }

  return(dt)
}


#Applies labels together with measures to the data.table dt. It modifies the data.table in place.
set_apply_labels<-function(dt, labels, vartypes, flagUseTaggedNA=TRUE, in_varnames=NULL, flag_keep_tagged_na=FALSE)
{
  #  browser()
  if (is.null(in_varnames))
  {
    varnames=colnames(dt)
  } else
  {
    varnames=in_varnames
  }

  suggestedTaggedMissings <- character(0)
  errors<-new.env()
  mywarnings<-new.env()

  fn_baseclass<-function(varnr)
  {
      #if (varnr==547) browser();
    #    if (colnames(dt)[[varnr]]=='q_88e') browser()
    #    cat(paste(varnr,'\n'))
    #First we find missings and convert them into proper tagged NA
    myALLlevels<-labels[[as.character(varnr)]]$levels
    myALLlabels<-labels[[as.character(varnr)]]$labels
    charLevels<-is.na(suppressWarnings(as.numeric(myALLlevels)))
    myNAlevels<-myALLlevels[charLevels]
    myNAlabels<-myALLlabels[charLevels]

    info<-assign_nas(myNAlabels, suggestedTaggedMissings=suggestedTaggedMissings)
    suggestedTaggedMissings<<-info$suggestedTaggedMissings
    myNAtags<-info$NAtags
    myNAlabels<-myNAlabels[!is.na(myNAtags)]
    myNAlevels<-myNAlevels[!is.na(myNAtags)]
    myNAtags<-myNAtags[!is.na(myNAtags)]

    mylevels<-as.numeric(myALLlevels[!charLevels])
    mylabels<-myALLlabels[!charLevels]

    origvar <- dt[[varnr]]
    var <- origvar #copy


    if (length(myNAlevels)>0){
      var[var %in% myNAlevels] <- NA
    }

    # Now we have properly replaced all missings with adequate tagged missings.
    # Now we should decide what base class the variable should have. Choices are:
    # date			 if we are forced to do this via the 'class' parameter. If we fail to do that continue:
    if (vartypes[[varnr]]=='D')
    {
      varDate<-tryCatch(
        as.Date(origvar,origin="1899-12-30") #We assume Excel for Windows format
        , error=function(e){return(NULL)})
      if (!is.null(varDate))
      {
      	copy_obj_attributes(var, varDate)

        return(list(var=varDate, NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
      }
    }

    # Now we check, whether the variable is a valid number

    numvar<-tryCatch(
      as.numeric(var),
      warning=function(w){NULL}
    )

    # character  if there is at least one character or type is character
    if (length(numvar)==0 || vartypes[[varnr]]=='S')
    {
      if (vartypes[[varnr]]=='D')
      {
        var<-add_msg_var(var = colnames(dt)[[varnr]], message =
                           paste0("cannot be converted to Date."),
                         flag_show_type=TRUE, flag_error=TRUE )
                return(list(var=var, NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
      }

      #Variable is a character string
      if (length(myNAlabels)>0)
      {
        if(flag_keep_tagged_na)
        {
          msg<-paste0("doesn't support tagged NA. Keeping all tagged NA as non-missing text labels")
          var <- origvar
          copy_obj_attributes(origvar, var)

        } else {
          msg<-paste0("doesn't support tagged NA. Removing all tagged NA into plain NA. Consider replacing the vector into numeric")
        }
        var<-add_msg_var(var, msg, flag_show_type=TRUE, flag_warning=TRUE)
      }
      return(list(var=var, NAlabels=character(0), NAlevels=character(0), levels=mylevels, labels=mylabels ))
    }
    copy_obj_attributes(var, numvar)

    if (vartypes[[varnr]]=='D')
    {
      if (length(myNAlabels)>0)
      {
        msg<-paste0("doesn't support tagged NA. Replacing all tagged NA with plain NA")
        numvar<-add_msg_var(numvar,
                message = msg,
                flag_show_type=TRUE, flag_warning=TRUE)
      }
      varDate<-tryCatch(
        as.Date(numvar,origin="1899-12-30") #We assume Excel for Windows format
        , error=function(e){
          newvar<-add_msg_var(var,
                  message = paste0("cannot be converted into date"),
                  flag_show_type=TRUE, flag_warning=TRUE);
          copy_obj_attributes(origvar, newvar)

          return(list(newvar))
        }
      )
      if (!is.list(varDate))
      {
        return(list(var=varDate[[1]], NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
        #        return(list(var=varDate, NAlabels=myNAlabels, NAlevels=myNAtags, levels=mylevels, labels=mylabels ))
      }
    }

    #We can put all NAs in place
    if (flagUseTaggedNA)
    {
      for (i in seq_along(myNAlevels))
      {
        n=myNAlevels[[i]]
        t=myNAtags[[i]]
        numvar[origvar==n] <- haven::tagged_na(t)
      }
    }

    #Now we know, we have a proper numeric number.
    if ((length(myNAlevels)>0) && vartypes[[varnr]] %in% c('I','F') && flagUseTaggedNA){
      msg<-paste0("doesn't support tagged NAs. Promoting integer into the numeric")
#      warning(msg)
      numvar<-add_msg_var(numvar,
              message = msg,
              flag_show_type=TRUE, flag_warning=TRUE)
      forceNumeric=TRUE
    } else {
      forceNumeric=FALSE
    }

    #Now we need to decide whether to keep the numeric

    if (((length(myNAlevels)==0) || flagUseTaggedNA) && !forceNumeric && vartypes[[varnr]] %in% c('I','F'))
    {
      newnumvar <- as.integer(numvar)
      copy_obj_attributes(numvar, newnumvar)
      return(list(var=newnumvar, NAlabels=list(), NAlevels=list(), levels=mylevels, labels=mylabels ))

    }
    if (length(levels)==0 && length(NAlevels)>0)
    {
      numvar <- add_msg(numvar,
              message = paste0("doesn't support named missings. Discarding the names for the missings (but leaving the attributes and tagged na)"),
              flag_show_type=TRUE, flag_warning=TRUE);
    }

    return(list(var=numvar, NAlabels=myNAlabels, NAlevels=myNAtags, levels=mylevels, labels=mylabels ))
  }

  fn_labelled<-function(var, NAlabels, NAlevels, labels, levels)
  {

    if (length(NAlabels)>0)
    {
      if (length(levels)>0)
      {
        varret <- labelled::labelled(var, c(setNames(levels, labels), setNames(haven::tagged_na(NAlevels),NAlabels)))
      } else {
        #        browser()
        setattr(var, "labels", setNames(haven::tagged_na(NAlevels),NAlabels))
        varret <- var
      }
    	copy_obj_attributes(var, varret)
      return(varret)
    }

    if (length(labels)>0)
    {
      if (is.integer(var))
      {
        if (all(sort(unique(var)) %in% levels))
        {
          varret <- factor(var, levels=levels, labels=labels)
          copy_obj_attributes(var, varret)
          setattr(varret, 'labels', NULL)
          return(factor(var, levels=levels, labels=labels))
        }
      }
      varret <- labelled::labelled(var, setNames(levels, labels))
      copy_obj_attributes(var, varret)
      return(varret)
    }
    return(var)
  }

  fn<-function(varnr)
  {
    #if (varnr==573) browser();

    #cat(paste0(varnr,'\n'))

    info<-fn_baseclass(varnr)
    var<-do.call(fn_labelled, info)
    #browser()
    if (!is.null(in_varnames))
    {
      if (varnr==1)
      {
        if (varnames[[varnr]]=='var1')
        {
          setnames(dt,'var2')
          n <- 'var2'
        } else { n <- 'var1'}
      }
      if (varnames[[varnr]] %in% names(dt))
      {
        newname <- make.un-ique(c(names(dt), varnames[[varnr]]))[[length(dt)+1]]
        add_msg_var(var,
                message = paste0('has duplicated name; it will be renamed into `',
                                 newname,
                                 "`"),
                flag_show_varnr=TRUE )

      } else {
        newname <- varnames[[varnr]]
      }
      dt[, (newname):=var]
      if (varnr==1)
      {
        dt[,(n):=NULL]
      }
    } else {
#      if('logical' %in% class(var) && sum(!is.na(var))==0 ){
        if(vartypes[[varnr]]=='S') {
          var2<-as.character(var)
        } else if (vartypes[[varnr]]=='D') {
          var2<-as.Date(var)
        } else if (vartypes[[varnr]]=='I') {
          var2<-as.integer(var)
        } else if (vartypes[[varnr]]=='N') {
          var2<-as.numeric(var)
        } else {
          var2<-var
#          browser()
        }
        copy_obj_attributes(obj_source = var, obj_dest = var2)
        var<-var2
 #     }
      dt[[varnr]]<-var
    }

    if (length(vartypes)>0 && length(vartypes[[varnr]]>0)) {
      setattr(dt[[varnr]],'measure_type', vartypes[[varnr]])
      if (vartypes[[varnr]]=='O' && is.factor(dt[[varnr]]))
      {
        setattr(dt[[varnr]],'class', c(attr(dt[[varnr]])),"ordered", exact = TRUE)
      }
    }
    return(var)
  }


  dts<-plyr::alply(seq_along(varnames),1, fn)
  newdt=as.data.table(dts)
  rownames(newdt) <- rownames(dt)
  names(newdt) <- names(dt)

  #  browser()
  plyr::l_ply(seq_along(dt), function(varnr) setattr(newdt[[varnr]], 'label', Hmisc::label(dt[[varnr]])) )

  return(list(errors=errors, warnings=mywarnings, dt=newdt))
}
