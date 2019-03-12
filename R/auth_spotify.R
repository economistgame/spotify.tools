# environment to store credentials
.sstate <- new.env(parent = emptyenv())

#' Get Spotify Access Token
#'
#' This function creates a Spotify access token.
#'
#' @param client_id Spotify Web API application key. Default value is a System Environment variable "SPOTIFY_CLIENT_ID"
#' @param client_secret Spotify Web API application secret. Default value is System Environment variable "SPOTIFY_CLIENT_SECRET"
#' @param cache By default your credentials are locally cached in a file called
#'   \code{.httr-oauth}. Set to FALSE if you need to authenticate separately
#'   each time.
#' @param rdstoken File path to stored RDS token. In server environments where
#'   interactive OAuth is not possible, a token can be created on a desktop
#'   client and used in production. See examples.
#'
#' @return A Token2.0 object, invisibly
#'
#' @examples
#' \dontrun{
#' get_spotify_access_token()
#' }
#'
#' @importFrom httr POST accept_json status_code

#' @export
spotify_auth <- function(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
                         client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'),
                         cache = TRUE,
                         rdstoken = NA) {
  if (!is.na(rdstoken)) {

    # read token or error
    if (file.exists(rdstoken)) {
      .sstate$token <- readRDS(rdstoken)

    } else {
      stop("token file not found")
    }

    # authenticate normally
  } else {
  post <- POST('https://accounts.spotify.com/api/token',
                 accept_json(), authenticate(client_id, client_secret),
                 body = list(grant_type = 'client_credentials'),
                 encode = 'form', httr::config(http_version = 2)) %>% content

  if (!is.null(post$error)) {
    stop(paste0('Could not authenticate with given Spotify credentials:\n\t',post$error_description))
  }

   access_token <- post$access_token

  if (file.exists(".httr-oauth")) {

    message("Reading old credentials...")
    file.remove(".httr-oauth")
  }
   .sstate$client_id <- client_id
   .sstate$client_secret <- client_secret
   .sstate$token <- access_token


  }
  }

#' Retrieve oauth2 token from spotify.utils-namespaced environment
#'
#' Retrieves a token if it is previously stored, otherwise prompts user to get one.
#'
#' @keywords internal
get_spotify_token <- function() {

    if (!exists('.sstate') || is.null(.sstate$token)) {
    spotify_auth()
  } else {
    res <- GET(url = paste0('https://api.spotify.com/v1/users/smedjan'), query = list(access_token =.sstate$token), quiet = TRUE)
    if(status_code(res)!="200" && status_code(res)!="201"){
      spotify_auth()
    } else{
    return(.sstate$token)
    }
  }
}

