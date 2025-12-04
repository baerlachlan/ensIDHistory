#' Get the current Ensembl data release
#'
#' Query the Ensembl REST API to determine the current Ensembl
#' data release used by the REST server.
#'
#' This calls the `/info/data` endpoint on the specified REST server and
#' extracts the release number from the returned JSON.
#'
#' @param rest_server Character scalar. Base URL of the Ensembl REST server.
#'   Defaults to `"https://rest.ensembl.org"`. For GRCh37, you might use
#'   `"https://grch37.rest.ensembl.org"`.
#'
#' @return An integer scalar giving the current Ensembl data release, or
#'   `NA_integer_` if it could not be determined.
#'
#' @examples
#' \dontrun{
#' ensCurrentRelease()
#' ensCurrentRelease("https://grch37.rest.ensembl.org")
#' }
#'
#' @export
ensCurrentRelease <- function(
        rest_server = "https://rest.ensembl.org"
) {

    req <- httr2::request(paste0(rest_server, "/info/data"))
    req <- httr2::req_headers(req, Accept = "application/json")
    resp <- httr2::req_perform(req)
    info <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    as.integer(max(info$releases))

}
