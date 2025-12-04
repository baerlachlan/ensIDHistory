#' Get Ensembl stable ID history
#'
#' Retrieve the history of Ensembl stable identifiers across Ensembl releases.
#' Uses the Ensembl core MySQL schema (`stable_id_event`, `mapping_session`)
#' or the Ensembl REST Archive API (`/archive/id`).
#'
#' @param ids Character vector of Ensembl stable IDs (genes, transcripts, etc.).
#' @param species Character scalar. Species name (e.g. "homo_sapiens",
#'   "mus_musculus"). For the MySQL backend this is used to construct the
#'   core database name as
#'   `<species>_core_<release>_<build>`.
#' @param release Integer. Ensembl release number (e.g. `113`) for the
#'   MySQL backend.
#' @param build Integer. Assembly/build number suffix (e.g. `38` for GRCh38)
#'   for the MySQL backend.
#' @param mysql_host MySQL host for the Ensembl core DB
#'   (default `"ensembldb.ensembl.org"`).
#' @param mysql_user MySQL user (default `"anonymous"`).
#' @param mysql_port Integer port (default `3306`).
#' @param rest_server Base URL for the REST API
#'   (default `"https://rest.ensembl.org"`).
#' @param ... Passed on to backend-specific helpers.
#'
#' @return A tibble with at least the columns:
#'   \describe{
#'     \item{old_stable_id}{Original (old) stable ID.}
#'     \item{new_stable_id}{Mapped stable ID or `NA` if retired.}
#'     \item{old_release}{Source Ensembl release.}
#'     \item{new_release}{Target Ensembl release.}
#'     \item{score}{Mapping score (MySQL backend only).}
#'     \item{backend}{Backend used: `"mysql"` or `"rest"`.}
#'   }
#'
#' @export
ensIDHist <- function(
        ids,
        species,
        release,
        build,
        mysql_host = "ensembldb.ensembl.org",
        mysql_user = "anonymous",
        mysql_port = 3306L,
        rest_server = "https://rest.ensembl.org",
        ...
) {

    ids <- unique(ids[!is.na(ids)])

    if (!length(ids)) {
        return(tibble::tibble())
    }

    if (missing(release) || missing(build)) {
        stop("Both 'release' and 'build' must be specified.", call. = FALSE)
    }

    .getHistoryMySQL(
        ids     = ids,
        species = species,
        release = release,
        build   = build,
        host    = mysql_host,
        user    = mysql_user,
        port    = mysql_port
    )

}
