.connectMySQL <- function(
        species,
        build,
        db_release,
        mysql_host = "ensembldb.ensembl.org",
        mysql_user = "anonymous",
        mysql_port = 3306L
) {

    if (missing(build)) {
        stop(
            "`build` must be specified to construct the core DB name.",
            call. = FALSE
        )
    }

    species_db_prefix <- tolower(gsub(" ", "_", species))
    dbname <- sprintf(
        "%s_core_%d_%d",
        species_db_prefix, as.integer(db_release), as.integer(build)
    )

    DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = mysql_host,
        user = mysql_user,
        password = "",
        port = mysql_port,
        dbname = dbname
    )

}

.getHistoryMySQL <- function(
        ids,
        species,
        build,
        db_release,
        mysql_host = "ensembldb.ensembl.org",
        mysql_user = "anonymous",
        mysql_port = 3306L
) {

    con <- .connectMySQL(
        species = species,
        build = build,
        db_release = db_release,
        mysql_host = mysql_host,
        mysql_user = mysql_user,
        mysql_port = mysql_port
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)

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

    if (!nrow(out)) return(tibble::tibble())

    out$old_release <- as.numeric(out$old_release)
    out$new_release <- as.numeric(out$new_release)

    tibble::tibble(out)

}
