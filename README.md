# ensIDHistory

> ⚠️ **Under active development**  
> This package is currently under development and breaking changes may occur.

`ensIDHistory` provides programmatic access to Ensembl stable ID history, enabling users to track how Ensembl gene, transcript, and protein identifiers change across releases.

The package allows users to:

- Retrieve full Ensembl ID history via the public Ensembl MySQL database  
  (`stable_id_event`, `mapping_session`)
- Map Ensembl IDs to a specified Ensembl release
- Identify retired, merged, or renamed identifiers
- Work across all species available in Ensembl
- Use a minimal dependency footprint (`DBI`, `RMariaDB`, `httr2`)

Ensembl “stable” identifiers are only stable *within* a given release and may change between releases due to updates in genome assemblies, gene models, or annotation strategies.  
`ensIDHistory` acts as a lightweight R interface to the Ensembl infrastructure that records these changes.

In particular, this package re-implements the functionality of Ensembl’s [`IDmapper.pl`](https://github.com/Ensembl/ensembl-tools/blob/release/115/scripts/id_history_converter/IDmapper.pl) script entirely within the R environment, making ID history queries reproducible and easily integrable into R-based analysis workflows.

## Installation

The development version of `ensIDHistory` can be installed from GitHub:

```r
remotes::install_github("baerlachlan/ensIDHistory")
```
