#' Retrieve details information for one Point Of Interest
#'
#' Wrapper around the call of the Google Places API
#'
#' @param place_id A textual identifier that uniquely identifies a place, returned from a Place Search
#' @param key string of character giving the API KEY for the google places API
#'
#' @return a data table
#'
#' @export

key = 'AIzaSyAlibnTRYnUJeiWtpnv1nzOv6LBgbEzCTA'
url1 = "https://maps.googleapis.com/maps/api/place/details/json?placeid="
place_id = 'ChIJy5DkgiBiLxMRFr1YqGYxF30'
url2 = gsub(" ", "", paste0(url1, place_id, "&key=", key))

# Call the API and get the JSON data
doc <- fromJSON(url2)
names(doc$result)


str(doc$result)


jj2$result$name


str(jj)
fromJSON(jj)
?json

json <- try(RCurl::getURL(url2, ssl.verifyPeer = FALSE))
json
str(json)
class(json)
library(jsonlite)
fromJSON(json)
doc = tryCatch(expr = jsonlite::fromJSON(json), error = function(e) NULL)
str(doc$html_attributions)

https://maps.googleapis.com/maps/api/place/details/json?placeid=001e25d62888b9e83d862159119bf54451a79e7f&key=AIzaSyAlibnTRYnUJeiWtpnv1nzOv6LBgbEzCTA"
https://maps.googleapis.com/maps/api/place/details/json?placeid=ChIJN1t_tDeuEmsRUsoyG83frY4&key=YOUR_API_KEY

getDetails <- function(key, place_id,
                    out = T)
{

  if (missing(key))
  {
    warning("Without a key, you will be highly limited. Visit: https://developers.google.com/places/web-service/ to create your free key. ")
  }
  if (missing(place_id))
  {
    warning("provide a place id")
  }
  
  
  data = data.table::data.table()
      s_token = ifelse(token == "", "", paste0("&pagetoken=",
                                               token))
      # Creates the url to be called to get the json data
      url1 = "https://maps.googleapis.com/maps/api/place/details/json?placeid="
      url2 = gsub(" ", "", paste0(url1, place_id, "&key=", key, s_token))
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
          tmp_data = cbind(coord, doc$results$name, doc$results$vicinity,
                           types, keyword_char)
          # Rename the columns
          data.table::setnames(tmp_data, c("lat", "lng",
                                           "name", "vicinity", "type", "keyword"))
          if (NCOL(tmp_data) != 6)
          {
            # Means that some information are missing. I do not want to
            # keep it
            tmp_data = data.table::data.table(lat = NA,
                                              lng = NA, name = NA, vicinity = NA, type = NA,
                                              keyword = keyword_char)
          }
        } else if (doc$status == "ZERO_RESULTS")
        {
          # No results
          tmp_data = data.table::data.table(lat = NA,
                                            lng = NA, name = NA, vicinity = NA, type = NA,
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
