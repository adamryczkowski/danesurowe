readMeasureAndUnits<-function(file, dt)
{

  address<-danesurowe::getNamedRange(file, getOption('rng_Measure'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  measures<-rng[[address$colnr]][(address$rownr+2):(address$rownr+ncol(dt))]

  address<-danesurowe::getNamedRange(file, getOption('rng_Units'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  units<-rng[[address$colnr]][(address$rownr+2):(address$rownr+ncol(dt))]

  for (varnr in seq_along(measures))
  {
    if (!is.na(units[[varnr]]))
    {
      setattr(dt[[varnr]], 'units', units[[varnr]])
    }
    setattr(dt[[varnr]],'f.o.b',switch(measures[[varnr]],N=1,O=2,D=3,0))
    if (measures[[varnr]]=='O' && is.factor(dt[[varnr]]))
    {
      setattr(dt[[varnr]], 'class', c(attr(dt[[varnr]],'class'), 'ordered'))
    }
  }

}

readXLSFormulas<-function(file, varcnt) {
  address<-danesurowe::getNamedRange(file, getOption('rng_XLSFormulas'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  xlsformulas<-rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)]
  xlsformulas[xlsformulas=='']<-NA
  return(xlsformulas)
}

readRFormulas<-function(file, varcnt) {
  address<-danesurowe::getNamedRange(file, getOption('rng_RFormulas'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  rformulas<-rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)]
  rformulas[rformulas=='']<-NA
  return(rformulas)
}

readTypes<-function(file, varcnt)
{
  address<-danesurowe::getNamedRange(file, getOption('rng_IntendedVariableClass'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  measures<-rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)]
  measures[is.na(measures)]<-'0'
  return(measures)
}

readMinMax<-function(file, dt)
{
  varcnt <- ncol(dt)
  address<-danesurowe::getNamedRange(file, getOption('rng_TheoreticalMin'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  mins<-rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)]

  namins<-is.na(mins)
  minsnum <- suppressWarnings(as.numeric(mins))

  address<-danesurowe::getNamedRange(file, getOption('rng_TheoreticalMax'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  maxs<-rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)]

  namaxs<-is.na(maxs)
  maxsnum <- suppressWarnings(as.numeric(maxs))

  fn<-function(varnr)
  {
    wrongmin <- is.na(minsnum[[varnr]]) && !namins[[varnr]]
    wrongmax <- is.na(maxsnum[[varnr]]) && !namaxs[[varnr]]
    if (wrongmin && wrongmax)
    {
      add_msg(dt = dt, varname = colnames(dt)[[varnr]],
              message = paste0("Minimal and maximal theroretical value must be numeric, not \"",
                      mins[[varnr]],
                      "\" and \"",
                      maxs[[varnr]],
                      "\".")
      )
    } else {
      if (wrongmin)
      {
        add_msg(dt = dt, varname = colnames(dt)[[varnr]],
                message = paste0("Minimal theoretical value must be numeric, not \"",
                        mins[varnr],
                        "\".")
        )
      } else if (wrongmax)
      {
        add_msg(dt = dt, varname = colnames(dt)[[varnr]],
                message = paste0("Maximal theoretical value must be numeric, not \"",
                        maxs[varnr],
                        "\".")
        )
      } else
      {
        #errors[[varnr]] <<- character(0)
      }
    }

  }
#  browser()
  plyr::a_ply(seq(varcnt-1), 1, fn)
  return(list(mins=minsnum, maxs=maxsnum))
}

readForceInteger<-function(file, varcnt)
{
  address<-danesurowe::getNamedRange(file, getOption('rng_ForceInteger'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  return(rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)])
}

readRequired<-function(file, varcnt)
{
  address<-danesurowe::getNamedRange(file, getOption('rng_Required'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  return(rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)])
}

readLimitedToLabels<-function(file, varcnt)
{
  address<-danesurowe::getNamedRange(file, getOption('rng_OnlyLabelledValues'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)

  ints <- rng[[address$colnr]][(address$rownr+1):(address$rownr+varcnt)]
  ints <- as.logical(as.numeric(ints))
  ints[is.na(ints)] <- FALSE
  return(ints)
}
