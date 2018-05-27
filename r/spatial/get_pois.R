#' Retrieve information on the Point Of Interest around a location
#'
#' Wrapper around the call of the Google Places API
#'
#' @param lat integer or character representing the latitude
#' @param lng integer or character representing the longitude
#' @param radius integer or character representing the radius of search in meters
#' @param key string of character giving the API KEY for the google places API
#' @param keyword string of character limiting the search of places.
#'
#' @return a data table
#'
#' @export

getPOIs <- function(lat, lng, radius = 5000, key, keywords = "",
                    out = T)
{
  # Function to get the Place Of Interest based on some given
  # coordinates and a radius of influence

  if (missing(lat) | missing(lng))
  {
    stop("You need to provide coordinates")
  }
  if (missing(key))
  {
    warning("Without a key, you will be highly limited. Visit: https://developers.google.com/places/web-service/ to create your free key. ")
  }

  data = data.table::data.table()
  for (keyword in keywords)
  {
    bool = T
    token = ""
    sleep = 0

    # Define the keyword for the search when required
    keyword_char = as.character(keyword)
    keyword = ifelse(keyword == "", "", paste0("&keyword=",
                                               keyword))
    # While loop to all the gathering of additional json pages is
    # necessary
    while (bool)
    {
      # Define the token of the next page
      s_token = ifelse(token == "", "", paste0("&pagetoken=",
                                               token))
      # Creates the url to be called to get the json data
      url1 = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
      url2 = gsub(" ", "", paste0(url1, lat, ",", lng,
                                  "&radius=", radius, "&key=", key, keyword, s_token))
      if (out)
        print(url2)
      # Call the API and get the JSON data
      json <- try(RCurl::getURL(url2, ssl.verifyPeer = FALSE))
      doc = tryCatch(expr = jsonlite::fromJSON(json), error = function(e) NULL)

      if (is.null(doc))
      {
        # error handling not getting results.
        tmp_data = data.table::data.table(lat = NA, lng = NA,
                                          name = NA, vicinity = NA, type = NA, keyword = keyword_char)
      } else
      {

        # Look if there is a next page on the JSON
        if (!is.null(doc$next_page_token))
        {
          token = doc$next_page_token
          bool = T
          sleep = 2
        } else
        {
          bool = F
          sleep = 0
        }

        # Parse the JSON to gather the coordinates, the name, the
        # location and the types of the POIs
        if (doc$status == "OK")
        {

          coord = doc$results$geometry$location
          types = unlist(lapply(doc$results$types, function(x) paste(x,
                                                                     collapse = ";")))
          tmp_data = cbind(coord, doc$results$id, doc$results$name, doc$results$vicinity,
                           types, keyword_char)
          # Rename the columns
          data.table::setnames(tmp_data, c("lat", "lng", "id",
                                           "name", "vicinity", "type", "keyword"))
          if (NCOL(tmp_data) != 7)
          {
            # Means that some information are missing. I do not want to
            # keep it
            tmp_data = data.table::data.table(lat = NA,
                                              lng = NA, id = NA, name = NA, vicinity = NA, type = NA,
                                              keyword = keyword_char)
          }
        } else if (doc$status == "ZERO_RESULTS")
        {
          # No results
          tmp_data = data.table::data.table(lat = NA,
                                            lng = NA, id = NA, name = NA, vicinity = NA, type = NA,
                                            keyword = keyword_char)
        } else if (doc$status == "OVER_QUERY_LIMIT" | doc$status ==
                   "REQUEST_DENIED")
        {
          break
        } else if (doc$status == "INVALID_REQUEST")
        {
          break
        } else
        {
          break
        }
      }
      data = rbind(data, tmp_data)
      Sys.sleep(sleep)
    }
  }
  return(data)
}
