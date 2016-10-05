# Definition of the S3 class 'data.axaml' ###########################################
#####################################################################################

## constructor. Adds class by reference
#'@export
data.axaml <- function(DT){

  if(!data.table::is.data.table(DT))
    stop("DT should be a data.table")

  ## add new class data.axaml
  if(!"data.axaml" %in% class(DT))
    data.table::setattr(DT, "class", c("data.axaml", class(DT)))
}

## special behaviour when copying: copy attributes also
#'@export
"[.data.axaml" <- function(DT, ...){

  ret <- NextMethod("[")
  for(nm in names(attributes(DT)))
    if(!nm %in% c("names", "row.names", "class", ".internal.selfref"))
      data.table::setattr(ret, nm, attr(DT, nm))
  return(ret)

}


# set_functions #####################################################################
#####################################################################################

# Externals #########################################################
#####################################################################

#' set attributes to a dataset
#'
#' Description
#'
#' @param DT [data.table]
#' @param ... attributes name and value as arguments to the function
#' @export
set_attributes <- function(DT, exposure = NULL, claimcount = NULL, claimcost = NULL,
                           premium = NULL, costthreshold = NULL,
                           binaryTarget = NULL, ...)
{
  if(class(DT)[1] != "data.axaml")
    stop("Input dataset should be of type 'data.axaml'. Use data.axaml()")

  args = as.list(match.call())
  args = args[!names(args) %in% c("", "DT")]

  for(nm in names(args))
  {
    if(!is.numeric(args[[nm]]))
      eval(parse(text = paste0("set_", nm,"(DT = DT, value = '",args[[nm]],"')")))
    else
      eval(parse(text = paste0("set_", nm,"(DT = DT, value = ",args[[nm]],")")))
  }
}


# Internals #########################################################
#####################################################################

check_varexists <- function(DT, var)
{
  ##verify that the variable exists in DT
  if(!var %in% names(DT))
    stop(paste0(var, ": variable doesn't exist in the dataset"))
}

check_isset <- function(DT, attribute, value)
{
  ##check if the attribute wasn't already set
  att = attr(DT, attribute)
  if(!is.null(att))
    if(att != value)
      warning(paste0(attribute, " was previously set to ", att))
}

## set the attribute by reference
set_attr <- function(DT, attribute, value)
{
  data.table::setattr(DT, attribute, value)
  message(paste0("set as ",attribute,": ", value))
}

set_exposure <- function(DT, value)
{
  check_varexists(DT, value)
  check_isset(DT, "exposure", value)

  #### TESTS #############
  ########################

  if(!class(DT[[value]]) %in% c("numeric","integer"))
    stop(paste0(value, " is not of type numeric or integer"))

  incorrect_vals = which(DT[[value]] < 0 | DT[[value]] > 1)
  if(length(incorrect_vals) > 0)
    stop(paste0(value, " not in [0,1]"))

  #########################

  set_attr(DT, "exposure", value)
}


set_claimcount <- function(DT, value)
{
  check_varexists(DT, value)
  check_isset(DT, "claimcount", value)

  #### TESTS #############
  ########################

  if(!class(DT[[value]]) %in% c("integer"))
    stop(paste0(value, " is not of type integer"))
  if(!all(DT[[value]] >= 0))
    stop(paste0(value, " has negative values"))

  ########################

  set_attr(DT, "claimcount", value)
}


set_claimcost <- function(DT, value)
{
  check_varexists(DT, value)
  check_isset(DT, "claimcost", value)

  #### TESTS #############
  ########################`

  if(!class(DT[[value]]) %in% c("numeric","integer"))
    stop(paste0(value, " is not of type numeric or integer"))
  if(!all(DT[[value]] >= 0))
    stop(paste0(value, " has negative values"))

  ########################

  set_attr(DT, "claimcost", value)
}


set_premium <- function(DT, value)
{
  check_varexists(DT, value)
  check_isset(DT, "premium", value)

  #### TESTS #############
  ########################`

  if(!class(DT[[value]]) %in% c("numeric","integer"))
    stop(paste0(value, " is not of type numeric or integer"))
  if(!all(DT[[value]] >= 0))
    stop(paste0(value, " has negative values"))

  ########################

  set_attr(DT, "premium", value)
}


set_costthreshold <- function(DT, value)
{
  #### TESTS #############
  ########################`

  if(!class(value) %in% c("integer", "numeric"))
    stop("value given should be integer/numeric")

  if(is.null(attr(DT, "claimcost")))
    stop("claimcost attribute is not set")

  if(value > max(DT[[attr(DT, "claimcost")]]) |
     value < min(DT[[attr(DT, "claimcost")]]))
    stop(paste0("value is not in the range of claimcost"))

  ########################

  set_attr(DT, "costthreshold", value)
}


set_binaryTarget <- function(DT, value)
{
  check_varexists(DT, value)
  check_isset(DT, "binaryTarget", value)

  #### TESTS #############
  ########################`

  if(!class(DT[[value]]) %in% c("numeric","integer"))
    stop(paste0(value, " is not of type numeric or integer"))
  if(!all(DT[[value]] %in% c(0,1)))
    stop(paste0(value, " is not in {0,1}"))

  ########################

  set_attr(DT, "binaryTarget", value)
}

