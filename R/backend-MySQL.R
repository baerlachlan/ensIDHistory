.connectMySQL <- function(
        species,
        release,
        build,
        host = "ensembldb.ensembl.org",
        user = "anonymous",
        port = 3306L
) {

    if (missing(release) || missing(build)) {
        stop(
            paste0(
                "`release` and `build` must be supplied to construct",
                "the core DB name."),
            call. = FALSE
        )
    }

    species_db_prefix <- tolower(gsub(" ", "_", species))

    dbname <- sprintf("%s_core_%d_%d", species_db_prefix, as.integer(release), as.integer(build))

    con <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host     = host,
        user     = user,
        password = "",
        port     = port,
        dbname   = dbname
    )

    con
}

.getHistoryMySQL <- function(
        ids,
        species,
        release,
        build,
        host = "ensembldb.ensembl.org",
        user = "anonymous",
        port = 3306L
) {

    con <- .connectMySQL(
        species = species,
        release = release,
        build   = build,
        host    = host,
        user    = user,
        port    = port
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    ids <- unique(ids)

    ## Chunk to avoid giant IN clauses
    chunks <- split(ids, ceiling(seq_along(ids) / 500L))

    res_list <- lapply(chunks, function(x) {
        placeholders <- paste(rep("?", length(x)), collapse = ", ")
        sql <- sprintf(
            "SELECT
            sie.old_stable_id,
            sie.old_version,
            ms.old_release,
            sie.new_stable_id,
            sie.new_version,
            ms.new_release,
            sie.score
            FROM stable_id_event AS sie
            JOIN mapping_session AS ms USING (mapping_session_id)
            WHERE sie.old_stable_id IN (%s)
            ORDER BY sie.old_stable_id ASC,
                sie.old_version ASC,
                CAST(ms.new_release AS UNSIGNED)",
            placeholders
        )
        DBI::dbGetQuery(con, sql, params = as.list(x))
    })

    out <- do.call(rbind, res_list)

    if (!nrow(out)) {
        return(tibble::tibble())
    }

    tibble::tibble(out)

}
