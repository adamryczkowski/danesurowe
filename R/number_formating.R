report_pvalue_long_1<-function(pvalue, pvalue_levels=c(0.1, 0.05, 0.01, 0.001, 0.0001), pvalue_signs=c(NA, '*', '**', '†', '‡')) {
  if(pvalue == 0) {
    ndigits <- 1000
    pvalue <- .Machine$double.xmin
    msg<-paste0('p\uA0<\uA0')
  } else {
    ndigits <- max(3, as.integer(-log10(pvalue))+3)
    msg<-paste0('p\uA0=\uA0')
  }
  if(ndigits <=7) {
    msg<-paste0(msg, format(pvalue, digits=0, nsmall = ndigits, scientific = FALSE, big.mark = '\uA0'))
  }else{
    msg<-paste0(msg, format(pvalue, digits=3, scientific = TRUE, big.mark = '\uA0'))
  }
  pos<-which.max(c(pvalue_levels<pvalue,TRUE))-1
  if(pos>0) {
    level<-pvalue_levels[pos]
    sign<-pvalue_signs[pos]
    if(!is.na(sign)) {
      msg <- paste0(msg, "; p\uA0<\uA0", format(level,scientific = FALSE, digits=10))
    }
  } else {
    msg <- paste0(msg, "; p\uA0>\uA0", format(pvalue_levels[[1]],scientific = FALSE, digits=10))
  }
  return(msg)
}

report_pvalue_long <- Vectorize(report_pvalue_long_1, vectorize.args = 'pvalue')

report_values<-function(values, ...) {
  if(!is.null(attr(values,'verbatim'))) {
    return(values)
  } else {
    UseMethod("report_values", values)
  }
}

report_values.default<-function(values, ...) {
  return(format_values(values))
}

report_values.integer<-function(values, ...) {
  return(report_integer(values))
}

report_values.numeric<-function(values, ...) {
  return(report_single_value(values, ...))
}

report_single_value_1<-function(value, n.significant=2, max_width=9) {
  if(is.na(value)) {
    return("―")
  }
  if(value==0) {
    return("0")
  }
  ndigits <- as.integer(-log10(abs(value)) + n.significant)
  value<-round(value,ndigits)
  if(ndigits >= max_width + n.significant) {
    msg <- format(value, scientific = TRUE, nsmall = ndigits)
  } else if (ndigits > 0) {
    msg <- format(value, scientific = FALSE, big.mark = '\uA0', nsmall = ndigits)
  } else if (ndigits >= -max_width + n.significant) {
    msg <- format(value, scientific = FALSE, big.mark = '\uA0')
  } else {
    msg <- format(value, scientific = TRUE)
  }
  return(msg)
}

report_single_value<-Vectorize(report_single_value_1, vectorize.args = 'value')

report_integer<-function(value) {
  msg <- ifelse(is.na(value), "―",
                format(value, scientific = FALSE, big.mark = '\uA0'))
  return(msg)
}


report_value_with_error_1<-function(value, ci, n.significant=2) {
  if(is.na(value) || is.na(ci)) {
    return("―")
  }

  if(ci==0) {
    return(paste0(report_single_value_1(value=value, n.significant = n.significant),
                  '\uA0±\uA0', '0'))
  }

  ndigits <- as.integer(-log10(abs(ci+0.00000001)) + n.significant)
  ci <- round(ci,ndigits)
  value<-round(value,ndigits)
  if(ndigits > 0) {
    msg <- paste0(format(value, scientific = FALSE, big.mark = '\uA0', nsmall = ndigits), '\uA0±\uA0',
                  format(ci, scientific = FALSE, nsmall=ndigits, big.mark = '\uA0'))
  } else {
    msg <- paste0(format(value, scientific = FALSE, big.mark = '\uA0'), '\uA0±\uA0',
                  format(ci, scientific = FALSE, big.mark = '\uA0'))
  }
  return(msg)
}

report_value_with_error <- Vectorize(report_value_with_error_1, vectorize.args = c('value','ci'))

report_value_with_bounds_1<-function(value, ci_low, ci_high, n.significant=2) {
  if(is.na(value) ) {
    return("―")
  }

  ndigits1 <- as.integer(-log10(abs(ci_low+0.00000001)) + n.significant)
  ndigits2 <- as.integer(-log10(abs(ci_high+0.00000001)) + n.significant)
  ndigits <- max(ndigits1, ndigits2)
  ci_low <- round(ci_low,ndigits)
  ci_high <- round(ci_high,ndigits)
  value<-round(value,ndigits)
  if(ndigits > 0) {
    msg <- paste0(format(value, big.mark = '\uA0', nsmall = ndigits, scientific = FALSE), ' (',
                  ifelse(is.na(ci_low), "―", format(ci_low, big.mark = '\uA0', nsmall=ndigits, scientific = FALSE)), ";\uA0",
                  ifelse(is.na(ci_low), "―", format(ci_high, big.mark = '\uA0', nsmall=ndigits, scientific = FALSE)), ")")
  } else {
    msg <- paste0(format(value, big.mark = '\uA0', scientific = FALSE), ' (',
                  ifelse(is.na(ci_low), "―", format(ci_low, big.mark = '\uA0', scientific = FALSE)), ";\uA0",
                  ifelse(is.na(ci_low), "―", format(ci_high, big.mark = '\uA0', scientific = FALSE)), ")")
  }
  return(msg)
}

report_value_with_bounds <- Vectorize(report_value_with_bounds_1, vectorize.args = c('value','ci_low', 'ci_high'))

report_F_test<-function(F, df1, df2) {
  msg<-ifelse(is.na(F)|is.na(df1)|is.na(df2),
              "―",
              paste0('F(', report_integer(df1),',\uA0', report_integer(df2), ')\ua0=\ua0', report_single_value(F, n.significant = 4)))
  return(msg)
}

report_pvalue_long <- Vectorize(report_pvalue_long_1, vectorize.args = 'pvalue')

touch<-function(filename){
  write.table(data.frame(), file=filename, col.names=FALSE)
}
