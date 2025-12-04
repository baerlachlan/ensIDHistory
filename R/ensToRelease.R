#' Map Ensembl IDs to a target Ensembl release
#'
#' @param ids Character vector of Ensembl stable IDs.
#' @param species Character; species name.
#' @param release Integer; target Ensembl release number.
#' @param build Integer; target assembly/build number (e.g. `38` for GRCh38).
#' @inheritParams ensIDHist
#'
#' @return A tibble with columns `input_id`, `target_id`, `target_release`,
#'   `score`, `mapping_status`.
#'
#' @export
ensToRelease <- function(
        ids,
        species,
        release,
        build,
        mysql_host = "ensembldb.ensembl.org",
        mysql_user = "anonymous",
        mysql_port = 3306L
) {
    policy <- match.arg(policy)

    if (missing(release) || missing(build)) {
        stop("'release' and 'build' must be specified.", call. = FALSE)
    }

    hist <- .getHistoryMySQL(
        ids     = ids,
        species = species,
        release = release,
        build   = build,
        host    = mysql_host,
        user    = mysql_user,
        port    = mysql_port
    )

    if (!nrow(hist)) {
        return(tibble::tibble())
    }

    # At this point hist$new_release should be the provided release;
    # we keep the filter for safety / future flexibility.
    hist <- dplyr::filter(hist, .data$new_release == release)

    if (!nrow(hist)) {
        return(tibble::tibble())
    }

    if (policy == "best_score") {
        hist <- hist %>%
            dplyr::group_by(.data$old_stable_id) %>%
            dplyr::slice_max(.data$score, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()
    }

    dplyr::transmute(
        hist,
        input_id       = .data$old_stable_id,
        target_id      = .data$new_stable_id,
        target_release = .data$new_release,
        score          = .data$score,
        mapping_status = dplyr::case_when(
            is.na(.data$new_stable_id) ~ "retired",
            TRUE                       ~ "mapped"
        )
    )
}
