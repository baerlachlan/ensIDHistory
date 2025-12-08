#' Get Ensembl stable ID history
#'
#' Retrieve the history of Ensembl stable identifiers across Ensembl releases.
#' Uses the Ensembl core MySQL schema (`stable_id_event`, `mapping_session`)
#' or the Ensembl REST Archive API (`/archive/id`).
#'
#' @param ids Character vector of Ensembl stable IDs (genes, transcripts, etc.).
#' @param species Character scalar. Species name (e.g. "homo_sapiens",
#' "mus_musculus"). For the MySQL backend this is used to construct the
#' core database name as
#' `<species>_core_<release>_<build>`.
#' @param db_release Integer. Ensembl release number (e.g. `113`) for the
#' MySQL backend.
#' @param build Integer. Assembly/build number suffix (e.g. `38` for GRCh38)
#' for the MySQL backend.
#' @param backend tmp.#' @param backend Character scalar.
#' Backend to use for the query. Currently only `"mysql"` is implemented.
#' `"rest"` is reserved for a future Ensembl REST Archive API backend.
#' @param mysql_host MySQL host for the Ensembl core DB
#' (default `"ensembldb.ensembl.org"`).
#' @param mysql_user MySQL user (default `"anonymous"`).
#' @param mysql_port Integer port (default `3306`).
#' @param rest_server Base URL for the REST API
#' (default `"https://rest.ensembl.org"`).
#'
#' @return A tibble with at least the columns:
#' \describe{
#'     \item{old_stable_id}{Original (old) stable ID.}
#'     \item{new_stable_id}{Mapped stable ID or `NA` if retired.}
#'     \item{old_release}{Source Ensembl release.}
#'     \item{new_release}{Target Ensembl release.}
#'     \item{score}{Mapping score (MySQL backend only).}
#' }
#'
#' @export
ensIDHist <- function(
        ids,
        species,
        build,
        db_release,
        backend = c("mysql", "rest"),
        mysql_host = "ensembldb.ensembl.org",
        mysql_user = "anonymous",
        mysql_port = 3306L,
        rest_server = "https://rest.ensembl.org"
) {

    backend <- match.arg(backend)

    if (backend == "mysql") {
        .getHistoryMySQL(
            ids = ids,
            species = species,
            build = build,
            db_release = db_release,
            mysql_host = mysql_host,
            mysql_user = mysql_user,
            mysql_port = mysql_port
        )
    }

}
