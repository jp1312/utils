parse_statargs <- function(dtarg)
{
  if(data.table::is.data.table(dtarg))
    return(dtarg)
  else if(is.character(dtarg))
  {
    args = data.table::data.table(formula = dtarg, type = "summary",
                                  global = T, nm = F, fim = F)
    if(length(dtarg) > 1)
    {
      for(ff in dtarg[2:length(dtarg)])
      {
        args = rbind(args, data.table::data.table(formula = ff, type = "summary",
                                                  global = T, nm = F, fim = F))
      }
    }
    return(args)
  }
  else if(is.list(dtarg))
  {
    if(is.null(dtarg$type))
      dtarg$type = rep("summary", length(dtarg$formula))
    dtarg$global  = ifelse(is.null(dtarg$global), T, dtarg$global)
    dtarg$nm      = ifelse(is.null(dtarg$nm), F, dtarg$nm)
    dtarg$fim     = ifelse(is.null(dtarg$fim), F, dtarg$fim)

    args = data.table::data.table(formula = dtarg$formula[1], type = dtarg$type[1],
                                  global = T, nm = F, fim = F)
    if(length(dtarg) > 1)
    {
      for(ff in dtarg[2:length(dtarg)])
      {
        args = rbind(args, data.table::data.table(formula = ff, type = "summary",
                                                  global = T, nm = F, fim = F))
      }
    }
    return(args)
  }
}


### S3 CLASS 'ml.stats' ######################################################
#################################################################################

## constructor. Adds class by reference
ml.stats <- function(tbl){

  if(!data.table::is.data.table(tbl))
    stop("tbl should be a data.table")

  ## add new class data.ml
  if(!"ml.stats" %in% class(tbl))
    data.table::setattr(tbl, "class", c("ml.stats", class(tbl)))
}

## special behaviour when copying: copy attributes also
#'@export
"[.ml.stats" <- function(tbl, ...){
  ret <- NextMethod("[")
  if(!is.null(attr(tbl, "global")))
      data.table::setattr(ret, "global", attr(tbl, "global"))
  return(ret)
}


### MAIN STAT FUNCTION ##########################################################
#################################################################################

#' Statistics table
#'
#' Get statistics by variable level (for categorical anf numerical variables)
#'
#' @param DT [ml.data] dataset
#' @param varnames [character(n)] variable names
#' @param elements [character(n)] elements to be put in the statistics table
#' @param default_elems [boolean] whether default elements should be added.
#' Given the special attributes set (through set_attributes()), all possible
#' statistics will be added. True by default.
#' @param base_levels [list] base levels for particular variables. Ex:
#' base_levels = list(Occupation = "Unemployed", Type_Car = "B").
#' @param order_by [character(1)] the variable in the statistics table to order by
#' the results. If NULL (default), the statistics table will be ordered
#' alphabetically according to the names of the levels.
#' @param order {-1,1} if the ordering should be ascending (1, by default),
#' or decreasing (-1).
#' @param model [glm] a glm model. Results from the glm model (coefficient estimates,
#' standard errors, predicted relativities, ...) will be added to the
#' statistics table for predictors.
#' @param add_tbls [list(data.table) / data.table] table of statistics to add to
#' the final statistics table. The first column of each table should be the name
#' of a variable and contain its levels.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export
stats <- function(DT, varnames = NULL, elements = NULL,
                  default_elems = T, base_levels = list(),
                  order_by = NULL, order = 1,
                  model = NULL, add_tbls = list())
{
  ## check that the input is of type data.ml
  if(class(DT)[1] != "data.ml")
    stop("data input should be of type data.ml. Use data.ml()")

  ## Constructing the arguments to create the summary stat table ####
  ###################################################################

  ## default value for elements:
  if(default_elems)
  {
    dft_el = character()
    if(!is.null(attr(DT, "exposure")))
      dft_el = c(dft_el, "exposure")
    if(!is.null(attr(DT, "claimcount")))
      dft_el = c(dft_el, "claimcount")
    if(!is.null(attr(DT, "claimcost")))
      dft_el = c(dft_el, "claimcost")
    if(!is.null(attr(DT, "premium")))
      dft_el = c(dft_el, "premium")
    if(all(c("exposure", "claimcount") %in% dft_el))
      dft_el = c(dft_el, "freq")
    if(all(c("claimcount","claimcost","freq") %in% dft_el))
      dft_el = c(dft_el, "purepremium")
    if(all(c("premium","exposure") %in% dft_el))
      dft_el = c(dft_el, "earnedpremium")
    if(all(c("claimcost","earnedpremium") %in% dft_el))
      dft_el = c(dft_el, "lossratio")
    if(!is.null(attr(DT, "costthreshold")) &
       all(c("claimcount", "claimcost") %in% dft_el))
      dft_el = c(dft_el, "severeclaims")
    if("freq" %in% dft_el)
     dft_el = c(dft_el, "freq_rel")
    if(!is.null(model))
    {
      if(model$target == attr(DT, "claimcount"))
        dft_el = c(dft_el, "claimcount_pred", "freq_pred",
                   "freq_pred_rel", "mdl_coeffs","freq_mdl_rel")
    }
    if(!is.null(attr(DT, "binaryTarget")))
      dft_el = c(dft_el, "binaryTarget")

    elements = unique(c(dft_el, elements))
  }

  if(is.null(elements))
    stop("no statistics to display (elements = NULL and default_elems = F)")
  if(length(elements) == 0)
    stop("no statistics to display (elements = NULL and default_elems = F)")

  call_fct = paste0("parse_statargs(stat_", elements, "(DT, varnames, elements, model))")

  args = eval(parse(text = call_fct[1]))
  if(length(call_fct) > 1)
    for(i in 2:length(call_fct))
      args = rbind(args, eval(parse(text = call_fct[i])))

  ## If the model is given, calculate predictions ##################
  ##################################################################

  if(!is.null(model))
    suppressWarnings(predictions <- predict(model, newdata = DT, type = 'response'))
  else
    predictions = numeric()

  ## Constructing the table ########################################
  ##################################################################

  global_tbl = data.table::as.data.table(
          DT %>%
          custom_summarise(NULL, args, model) %>%
          custom_add(NULL, args, add_tbls, DT, model, predictions) %>%
          custom_mutate(v=NULL, args=args, base_levels, order_by, order, model))

  if(is.null(varnames))
    stats_tbl = ml.stats(global_tbl)
  else
  {
    stats_tbl = list()

    for(v in varnames)
    {
      stats_tbl[[v]] =
        ml.stats(
        data.table::as.data.table(
          DT %>%
            dplyr::group_by_(v) %>%
            custom_summarise(v, args, model) %>%
            custom_add(v, args, add_tbls, DT, model, predictions) %>%
            custom_mutate(v, args, base_levels, order_by, order, model) %>%
            custom_arrange(v, order_by, order)))

      data.table::setattr(stats_tbl[[v]], "global", global_tbl)
    }
  }
  return(stats_tbl)
}


### STAT_FUNCTIONS ##############################################################
#################################################################################

### TEMPLATE ###########################################################
########################################################################
# one stat function by formula defined
# possible to define a regrouping stat function though.
# stat_attribute <- function(DT, varnames, elements)
# {
    ## tests
    # formula1 = "..."
    # ret1 = data.table::data.table(formula = formula1, type = "...",
    # global = T/F, nm = T/F, fim = T/F)
    # return(rbind(ret1, ret2, ...))
    # type can be "summary", "mutate", "add"
# }

### EXPOSURE ###########################################################
########################################################################

stat_exposure <- function(DT, varnames, elements, ...)
{
  if(is.null(attr(DT, 'exposure')))
    stop("stat_exposure error: 'exposure' attribute not defined")

  formula1 = paste0("exposure = round(sum(",attr(DT, 'exposure'),
                    ", na.rm = T)*1000)/1000")

  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  formula2 = "prop_exposure = round(exposure/sum(exposure) * 100)/100"

  ret2 = data.table::data.table(formula = formula2, type = "mutate",
                                global = F, nm = F, fim = F)

  return(rbind(ret1, ret2))
}


### NB OF CLAIMS #######################################################
########################################################################

stat_claimcount <- function(DT, varnames, elements, ...)
{
  if(is.null(attr(DT, "claimcount")))
    stop("stat_claimcount error: 'claimcount' attribute not defined")

  formula1 = paste0("nbclaims = round(sum(", attr(DT, 'claimcount'),
                    ",na.rm = T))")

  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  return(ret1)
}

#custom_add <- function(tbl, v, args, add_tbls, DT, model, ...)
stat_claimcount_pred <- function(DT, varnames, elements, model, ...)
{
  if(is.null(attr(DT, "claimcount")))
    stop("stat_claimcount_pred error: 'claimcount' attribute not defined")
  if(is.null(model))
    stop("stat_claimcount_pred error: model is NULL")
  else if(!model$target == attr(DT, "claimcount"))
    stop("stat_claimcount_pred error: target variable is
         not associated to the 'claimcount' attribute.")

  formula1 =
    "
  if(is.null(v))
  {
  ret = data.table::data.table(nbclaims_pred = sum(predictions, na.rm = T))
  } else
  {
  lvls = tbl[[v]]
  nbclaims_pred = sapply(lvls, function(l){
  sum(predictions[DT[[v]] == l], na.rm = T)
  })
  ret = data.table::data.table(lvls, nbclaims_pred)
  data.table::setnames(ret, names(ret), c(v, 'nbclaims_pred'))
  }
  ret
  "
  ret1 = data.table::data.table(formula = formula1, type = "add",
                                global = T, nm = T, fim = F)
  return(ret1)
}


### FREQUENCY ##########################################################
########################################################################

stat_freq <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "freq")
  if(!("exposure" %in% elements[1:ind] & "claimcount" %in% elements[1:ind]))
    stop("stat_freq error: define exposure/claimcount before freq")

  formula1 = "freq = round(nbclaims/exposure*1000)/1000"

  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = T, nm = F, fim = F)

  return(ret1)
}

stat_freq_rel <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "freq_rel")
  if(!("freq" %in% elements[1:ind]))
    stop("stat_freq_rel error: freq not defined before freq_rel")

  formula1 = "freq_rel = round(freq/freq[base_level_index]*100)/100"

  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = F, nm = F, fim = F)

  return(ret1)
}

stat_freq_pred <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "freq_pred")
  if(!("claimcount_pred" %in% elements[1:ind] &
       "exposure" %in% elements[1:ind]))
    stop("stat_freq_pred error: claimcount_pred/exposure not defined before
         freq_pred")

  formula1 = "freq_pred = round(nbclaims_pred/exposure*1000)/1000"

  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = T, nm = T, fim = F)

  return(ret1)
}

stat_freq_pred_rel <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "freq_pred_rel")
  if(!("freq_pred" %in% elements[1:ind]))
    stop(paste0("stat_freq_pred_rel error: freq_pred not defined ",
                "before freq_pred_rel"))

  formula1 = paste0("freq_pred_rel = round(freq_pred/",
                    "freq_pred[base_level_index]*100)/100")

  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = F, nm = T, fim = F)

  return(ret1)
}

stat_freq_mdl_rel <- function(DT, varnames, elements, model, ...)
{
  if(is.null(model))
    stop("stat_freq_mdl_rel error: model not provided")
  if(model$target != attr(DT, "claimcount"))
    stop("stat_freq_mdl_rel error: target of the model isn't ",
         "claimcount")

  ind = which(elements == "freq_mdl_rel")
  if(!("mdl_coeffs" %in% elements[1:ind]))
    stop("stat_freq_mdl_rel error: mdl_coeffs not defined before ",
         "freq_mdl_rel")

  formula1 = "freq_mdl_rel = exp(estimate)"
  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = F, nm = T, fim = T)

  formula2 = "freq_mdl_rel_p2se = exp(estimate+(2*se))"
  ret2 = data.table::data.table(formula = formula2, type = "mutate",
                                global = F, nm = T, fim = T)

  formula3 = "freq_mdl_rel_m2se = exp(estimate-(2*se))"
  ret3 = data.table::data.table(formula = formula3, type = "mutate",
                                global = F, nm = T, fim = T)

  return(rbind(ret1, ret2, ret3))
}


### CLAIMS COST ########################################################
########################################################################

stat_claimcost <- function(DT, varnames, elements, ...)
{
  if(is.null(attr(DT, "claimcost")))
    stop("stat_claimcost error: claimcost attribute not defined")

  formula1 = paste0("cost = round(sum(", attr(DT, "claimcost"),",
                    na.rm = T))")

  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  ind = which(elements == "claimcost")
  if(!"claimcount" %in% elements[1:ind])
    stop("stat_claimcost (avg cost) error: claimcount not defined before
         claims_avg_cost")

  formula2 = "avgcost = ifelse(nbclaims > 0, round(cost/nbclaims), 0)"

  ret2 = data.table::data.table(formula = formula2, type = "mutate",
                                global = T, nm = F, fim = F)

  return(rbind(ret1, ret2))
}


### PREMIUM ############################################################
########################################################################

stat_premium <- function(DT, varnames, elements, ...)
{
  if(is.null(attr(DT,'premium')))
    stop("stat_premium error: premium attribute not defined")

  formula1 = paste0("premium = round(sum(",attr(DT,'premium'),", na.rm = T))")

  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  formula2 = paste0("avgpremium = round(mean(", attr(DT,'premium'),", na.rm = T))")

  ret2 = data.table::data.table(formula = formula2, type = "summary",
                                global = T, nm = F, fim = F)

  return(rbind(ret1, ret2))
}


stat_purepremium <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "purepremium")
  if(!("claimcount" %in% elements[1:ind] &
       "claimcost" %in% elements[1:ind] &
       "freq" %in% elements[1:ind]))
    stop("stat_purepremium error: define claimcount/
         claimcost/freq before purepremium")

  formula1 = "purepremium = round(freq * avgcost)"

  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = T, nm = F, fim = F)

  return(ret1)
}


## defines:
## earnedpremium:    earned premium = premium * exposure for each level
stat_earnedpremium <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "earnedpremium")
  if(!("premium" %in% elements[1:ind] &
       "exposure" %in% elements[1:ind]))
    stop("stat_earnedpremium error: define premium/exposure before earnedpremium")

  formula1 = paste0("earnedpremium = round(sum(", attr(DT,'premium')," * ",
                    attr(DT, 'exposure'), ", na.rm = T))")

  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  return(ret1)
}

### SEVERE CLAIMS ######################################################
########################################################################

## defines:
## nb_severe:           number of severe claims
## cost_severe:         total cost of severe claims
## prop_severe:         proportion of severe claims
## prop_cost_severe:    proportion of severe claims in terms of cost
stat_severeclaims <- function(DT, varnames, elements, ...)
{
  if(is.null(attr(DT,"costthreshold")))
    stop("stat_costthreshold error:  costthreshold not defined.")

  ind = which(elements == "severeclaims")
  if(!("claimcount" %in% elements[1:ind] &
       "claimcost" %in% elements[1:ind]))
    stop("stat_severeclaims error: define claimcount/claimcost before
         severclaims")

  formula1 = paste0("nb_sev = sum(", attr(DT, 'claimcost'),
                    " > ", attr(DT, 'costthreshold'),", na.rm = T)")
  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  formula2 = paste0("cost_sev = sum(", attr(DT, 'claimcost'), "[",
                    attr(DT, 'claimcost')," > ", attr(DT, 'costthreshold'),
                    "], na.rm = T)")
  ret2 = data.table::data.table(formula = formula2, type = "summary",
                                global = T, nm = F, fim = F)

  formula3 = "prop_nbsev = round(nb_sev/nbclaims * 100)/100"
  ret3 = data.table::data.table(formula = formula3, type = "mutate",
                                global = T, nm = F, fim = F)

  formula4 = "prop_costsev = round(cost_sev/cost * 100)/100"
  ret4 = data.table::data.table(formula = formula4, type = "mutate",
                                global = T, nm = F, fim = F)

  return(rbind(ret1, ret2, ret3, ret4))
}

### MODEL ##########################################################################
####################################################################################

stat_mdl_coeffs <- function(DT, varnames, elements, model, ...)
{
  if(is.null(model))
    stop("stat_mdl_coeffs error: model is NULL")

  formula1 = "model$summarytbl_list"

  ret1 = data.table::data.table(formula = formula1, type = "add",
                                global = F, nm = T, fim = T)

  return(ret1)
}

stat_mdl_coeffs_numeric <- function(DT, varnames, elements, model, ...)
{
  if(is.null(model))
    stop("stat_mdl_coeffs error: model is NULL")

  formula1 = "model$summarytbl_list"

  ret1 = data.table::data.table(formula = formula1, type = "add",
                                global = F, nm = T, fim = T)

  return(ret1)
}

### OTHER ##########################################################################
####################################################################################

## defines:
## lossratio:    loss ratio = cost/earnedpremium
stat_lossratio <- function(DT, varnames, elements, ...)
{
  ind = which(elements == "lossratio")
  if(!("claimcost" %in% elements[1:ind] &
       "earnedpremium" %in% elements[1:ind]))
    stop("stat_lossratio error: define claimcost/earnedpremium before lossratio")

  formula1 = "lossratio = round(cost/earnedpremium * 100)/100"

  ret1 = data.table::data.table(formula = formula1, type = "mutate",
                                global = T, nm = F, fim = F)

  return(ret1)
}


stat_binaryTarget <- function(DT, varnames, elements, ...)
{
  if(is.null(attr(DT, "binaryTarget")))
    stop("stat_binaryTarget error: 'binaryTarget' attribute not defined")

  formula1 = paste0("binaryTarget = mean(", attr(DT, 'binaryTarget'),
                    ",na.rm = T)")

  ret1 = data.table::data.table(formula = formula1, type = "summary",
                                global = T, nm = F, fim = F)

  return(ret1)
}

### STATS ADDIN #################################################################
#################################################################################

#'@importFrom miniUI miniPage miniContentPanel gadgetTitleBar
#'@importFrom shiny textInput uiOutput radioButtons conditionalPanel reactive renderUI checkboxGroupInput dialogViewer runGadget observeEvent
statsAddin <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("computing stats()"),
    miniContentPanel(
      textInput("data", "Data", value = defaultData),
      uiOutput("pending"),
      radioButtons('typestats', 'Type of stats',
                   c(Global='global',
                     'By variable' ='byvar'),
                   'global'),
      conditionalPanel("input.typestats === 'byvar'",
                       uiOutput('varSelect'))
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {

    reactiveData <- reactive({
      # Collect inputs.
      dataString <- input$data

      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))

      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)
    })

    output$varSelect <- renderUI({
      data <- reactiveData()
      checkboxGroupInput('showVars', 'Variables available:',
                         names(data), selected = NULL)
    })

    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })

    # Listen for 'done'.
    observeEvent(input$done, {

      # Emit a subset call if a dataset has been specified.
      if (nzchar(input$data)) {
        if(input$typestats == "global")
          varstr = "varnames = NULL"
        else
          varstr = paste0("varnames = c('",
                          paste0(input$showVars, collapse = "','"), "')")
        code <- paste0("\n", "stats(DT = ", input$data, ", ", varstr, ")")
        rstudioapi::insertText(text = code)
      }

      invisible(stopApp())
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Hello World", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)

}
