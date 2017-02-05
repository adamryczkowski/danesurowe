#' danesurowe: Functions to parse and read the danesurowe format directly from within R
#'
#'It uses readr to validate and read the file and to convert it into the canonical data_frame structure.
#'
#'
#' @docType package
#' @name denesurowe
#' @import readxl
NULL


# nocov start
.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.danesurowe	<-	list(
    wew_sheet_name	=	'pointers',
    wew_pointer_column = 1,
    wew_pointer_row = 2,
    rng_DataOrigin = 'DataOrigin',
    rng_LabelOrigin = 'LabelOrigin',
    rng_Measure = 'Measure',
    rng_Units = 'Units',
    rng_VariableGroupsColumn = 'VariableGroupsColumn',
    rng_VariableGroupNames = 'VariableGroupNames',
    rng_CustomerFolder = 'CustomerFolder',
    rng_SubcustomerFolder = 'SubcustomerFolder',
    rng_DBLongName = 'DBLongName',
    rng_RObjName = 'RObjName',
    globalPathPrefix = '/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy'
  )
  toset	<-	!(names(op.danesurowe)	%in%	names(op))
  if(any(toset))	options(op.danesurowe[toset])
  invisible()
}
# nocov end
