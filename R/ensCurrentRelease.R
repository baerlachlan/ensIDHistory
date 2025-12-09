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
#' \donttest{
#' ensCurrentRelease()
#' ensCurrentRelease("https://grch37.rest.ensembl.org")
#' }
#'
#' @export
ensCurrentRelease <- function(
        rest_server = "https://rest.ensembl.org"
) {

    req <- httr2::request(rest_server)
    req <- httr2::req_url_path_append(req, "info/data")
    req <- httr2::req_headers(req, Accept = "application/json")
    req <- httr2::req_user_agent(
        req,
        "EnsIDHistory (https://github.com/baerlachlan/EnsIDHistory)"
    )
    resp <- try(httr2::req_perform(req), silent = TRUE)

    if (inherits(resp, "try-error")) {
        stop(
            "Failed to query Ensembl REST server for /info/data.",
            call. = FALSE
        )
    }

    status <- httr2::resp_status(resp)
    if (status >= 400L) {
        stop(
            sprintf(c(
                "Ensembl REST /info/data returned HTTP status %d"
            ), status),
            call. = FALSE
        )
    }

    info <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (!is.null(info$releases)) {
        return(as.integer(max(info$releases)))
    } else if (!is.null(info$release)) {
        return(as.integer(info$release))
    } else {
        stop(
            "Could not find 'releases' or 'release' in Ensembl",
            " /info/data response.",
            call. = FALSE
        )
    }

}
