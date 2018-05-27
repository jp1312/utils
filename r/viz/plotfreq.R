#' Frequency plot
#'
#' Plot the frequency graph associated with a variable
#'
#' @param statfreq list of stat tables containing frequency values
#'
#' @return list of plot objects
#'
#' @export
plotfreq <- function(statsfreq, fix_expo = F, fix_freq = F)
{
  ## test sur presencedes trucs qu'il faut
  res = list()

  if(data.table::is.data.table(statsfreq))
  {
    nmvar = names(statsfreq)[1]
    statsfreq = list(statsfreq)
    names(statsfreq) = nmvar
  }
  else if(!is.list(statsfreq))
    stop("statsfreq should be a data.table or list")

  for(var in names(statsfreq))
  {
    tbl <- statsfreq[[var]]
    gbl <- attr(tbl, "global")

    op <- list()
    op$chart <- list(zoomType = "xy", alignTicks = F)
    op$title <- list(text = paste0('Frequency Graph - ', var), x = -20)

    ## transform NA into string "NA" for correct display on graph
    tbl[[var]] = as.character(tbl[[var]])
    tbl[[var]][is.na(tbl[[var]])] = "NA"

    op$xAxis <- list(list(categories = tbl[[var]]))

    ## Exposure
    ### fixed scale for exposure ?
    if(fix_expo)
      maxexpo = max(sapply(statsfreq, function(ttt){max(ttt$exposure)}))
    else
      maxexpo = max(tbl$exposure)

    op$yAxis <- list(list(max = maxexpo + 100,
                          title = list(text = "Exposure"),
                          opposite = TRUE, gridLineWidth = 0))
    op$series <- list(list(name = "Exposure", type = "column", yAxis = 0,
                           data = tbl$exposure,
                           color = 'rgba(255,215,0, 0.5)'))
    ## Frequency
    ### fixed scale for frequency ?
    if(fix_freq)
    {
      maxfreq = max(sapply(statsfreq, function(ttt){
        ifelse("freq_pred" %in% names(ttt), max(ttt$freq, ttt$freq_pred),
               max(ttt$freq))
      }))
    }
    else if("freq_pred" %in% names(tbl))
      maxfreq = max(tbl$freq, tbl$freq_pred)
    else
      maxfreq = max(tbl$freq)

    if("freq_pred" %in% names(tbl))
    {
      pfseries = list(list(name = "Predicted Frequency", yAxis = 1,
                           data= tbl$freq_pred))
      overall_pfseries = list(list(name = "overall predicted frequency",
                                  type = "line",
                                  yAxis = 1, dashStyle = 'shortdash',
                                  data = rep(gbl$freq_pred, nrow(tbl)),
                                  label = list(text = "overall predicted frequency"),
                                  width = 2, color = "orange",
                                  marker = list(enabled = F),
                                  visible = T))
    }
    else
    {
      pfseries = NULL
      overall_pfseries = NULL
    }

    op$yAxis <- append(op$yAxis,
                       list(list(max = maxfreq+0.05,
                                 title = list(text = "Frequency"))))
    op$series <- append(op$series,
                        list(list(name = "Frequency", yAxis = 1,
                                  data = tbl$freq)))
    op$series <- append(op$series, pfseries)
    op$series <- append(op$series,
                        list(list(name = "average frequency", type = "line",
                                  yAxis = 1, dashStyle = 'shortdash',
                                  data = rep(gbl$freq, nrow(tbl)),
                                  label = list(text = "average frequency"),
                                  width = 2, color = "red",
                                  marker = list(enabled = F),
                                  visible = T)))
    op$series <- append(op$series, overall_pfseries)

    res[[var]] = highcharter::highchart(op)
  }
  return(res)
}
