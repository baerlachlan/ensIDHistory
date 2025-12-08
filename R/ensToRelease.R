#' Map Ensembl IDs to a target Ensembl release
#'
#' @param ids Character vector of Ensembl stable IDs.
#' @param species Character; species name.
#' @param build Integer; target assembly/build number (e.g. `38` for GRCh38).
#' @param to_release Integer; target Ensembl release number.
#' @inheritParams ensIDHist
#'
#' @return A tibble with columns `old_stable_id`, `new_stable_id`,
#'  `target_release`, `status`.
#'
#' @export
ensToRelease <- function(
        ids, species, build, to_release = db_release,
        db_release = ensCurrentRelease(), mysql_host = "asiadb.ensembl.org",
        mysql_user = "anonymous", mysql_port = 3306L
) {
    hist <- ensIDHist(
        ids, species, build, db_release, mysql_host, mysql_user, mysql_port
    )

    ## Base output, one row per input id
    out <- tibble::tibble(
        old_stable_id = ids, new_stable_id = NA_character_,
        target_release = NA_integer_, status = NA_character_
    )

    ## IDs that appear in history at all
    has_any_history <- out$old_stable_id %in% hist$old_stable_id

    ## Filter to mappings with new_release <= to_release
    hist_eligible <- hist[hist$new_release <= to_release, , drop = FALSE]

    ## For each old_stable_id, keep the entry closest to but not
    ## exceeding `to_release`
    if (nrow(hist_eligible) > 0L) {
        hist_split <- split(hist_eligible, hist_eligible$old_stable_id)
        hist_best_list <- lapply(hist_split, function(x) {
            max_rel <- max(x$new_release)
            x[x$new_release == max_rel, , drop = FALSE][1L, ]
        })
        hist_best <- do.call(rbind, hist_best_list)

        ## Align best mappings back to the original order of ids
        idx <- match(out$old_stable_id, hist_best$old_stable_id)
        has_best <- !is.na(idx)

        if (any(has_best)) {
            out$new_stable_id[has_best] <- hist_best$new_stable_id[
                idx[has_best]
            ]
            out$target_release[has_best] <- hist_best$new_release[
                idx[has_best]
            ]
        }

        ## Retired if we have a best row but new_stable_id is NA
        is_retired <- has_best & is.na(out$new_stable_id)
        is_mapped <- has_best & !is_retired

    } else {
        ## No eligible history under the to_release constraint
        has_best <- rep(FALSE, length(ids))
        is_retired <- rep(FALSE, length(ids))
        is_mapped <- rep(FALSE, length(ids))
    }

    out$status[!has_any_history] <- "unmapped_no_history"
    out$status[has_any_history & !has_best] <- "unmapped_no_release"
    out$status[is_retired] <- "unmapped_retired"
    out$status[is_mapped] <- "mapped"

    out
}
