#' Get popularity of one or more tracks on Spotify
#'
#' @description Obtain a popularity of a track ID o a vector of track IDs
#'
#' @name gettrack
#'
#' @param trackid a character vector with Spotify IDs of tracks
#' @param output a character vector to define the output items
#' @param simplifyoutput choose if the output is a completed information in data.frame
#' @param stoken Spotify Web API token. Defaults to spotify:tools::get_spotify_token()
#'
#' @return a data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET
#' @importFrom purrr map_df

#' @import dplyr httr purrr
#'
#' @examples
#' #Get track popularity of Madonna
#' \dontrun{
#' get_track_popularity("5U1tMecqLfOkPDIUK9SVKa")
#' }
#'
#' @export
#'
gettrack <- function(trackid,output=c("id","name","popularity","duration","islocal","explicit","disc_position","artist","artistid","totaltracks","availablecountries")  ,simplyoutput=FALSE, stoken = get_spotify_token()) {

  tracks <- unique(trackid)
  num_loops <- ceiling(length(tracks) / 50)

  available_countries <- data.frame(matrix(rep(0,249),ncol = 249, nrow = 1))
  ISOCodes <- c('AW','AF','AO','AI','AX','AL','AD','AE','AR','AM','AS','AQ','TF','AG','AU','AT','AZ','BI','BE','BJ','BQ','BF','BD','BG','BH','BS','BA','BL','BY','BZ','BM','BO','BR','BB','BN','BT','BV','BW','CF','CA','CC','CH','CL','CN','CI','CM','CD','CG','CK','CO','KM','CV','CR','CU','CW','CX','KY','CY','CZ','DE','DJ','DM','DK','DO','DZ','EC','EG','ER','EH','ES','EE','ET','FI','FJ','FK','FR','FO','FM','GA','GB','GE','GG','GH','GI','GN','GP','GM','GW','GQ','GR','GD','GL','GT','GF','GU','GY','HK','HM','HN','HR','HT','HU','ID','IM','IN','IO','IE','IR','IQ','IS','IL','IT','JM','JE','JO','JP','KZ','KE','KG','KH','KI','KN','KR','KW','LA','LB','LR','LY','LC','LI','LK','LS','LT','LU','LV','MO','MF','MA','MC','MD','MG','MV','MX','MH','MK','ML','MT','MM','ME','MN','MP','MZ','MR','MS','MQ','MU','MW','MY','YT','NA','NC','NE','NF','NG','NI','NU','NL','NO','NP','NR','NZ','OM','PK','PA','PN','PE','PH','PW','PG','PL','PR','KP','PT','PY','PS','PF','QA','RE','RO','RU','RW','SA','SD','SN','SG','GS','SH','SJ','SB','SL','SV','SM','SO','PM','RS','SS','ST','SR','SK','SI','SE','SZ','SX','SC','SY','TC','TD','TG','TH','TJ','TK','TM','TL','TO','TT','TN','TR','TV','TW','TZ','UG','UA','UM','UY','US','UZ','VA','VC','VE','VG','VI','VN','VU','WF','WS','YE','ZA','ZM','ZW')
  colnames(available_countries)<- ISOCodes

  map_df(1:num_loops, function(this_loop) {

    current <- tracks[((this_loop * 50) - 49):(ifelse(this_loop * 50>=length(tracks),length(tracks),this_loop*50))]
    uris <- paste0(current,collapse = ',')
    res <- GET(url = paste0('https://api.spotify.com/v1/tracks/?ids=',uris), query = list(access_token = stoken), quiet = TRUE) %>% content

    content <- res$tracks

    df <- map_df(1:length(content), function(this_row) {

      this_track <- content[[this_row]]
      available_countries_current <- available_countries
      available_countries_current[1,intersect(ISOCodes ,unlist(this_track$available_markets))] <- 1
      basic_info<- data.frame(
        id = this_track$id,
        name=this_track$name,
        popularity = this_track$popularity,
        duration = this_track$duration_ms/1000,
        is_local = this_track$is_local,
        explicit = this_track$explicit,
        disc_position = this_track$track_number,
        artist = this_track$artists[[1]]$name,
        artistid = this_track$artists[[1]]$id,
        album = this_track$album$name,
        total_tracks=this_track$album$total_tracks,
        stringsAsFactors = FALSE
      )
      cbind(basic_info,available_countries_current)
    })
    return(df)
  })
}
