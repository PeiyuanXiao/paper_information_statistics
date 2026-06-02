required_packages <- c(
  "pdftools", "purrr", "dplyr", "tidyr", "readr", "stringr", "tibble", "here"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Run renv::restore() or install them before running this script.",
    call. = FALSE
  )
}

library(here)

source(here::here("R", "profiles.R"))
source(here::here("R", "analyze_paper.R"))

default_input_dirs <- c(
  here::here("JAMT_paper_sample"),
  here::here("JAS_paper_sample")
)

input_dirs <- getOption("paperstats.input_dirs", default_input_dirs)

output_dir <- here::here("output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

discover_pdfs <- function(input_dir) {
  journal <- basename(input_dir)
  paths <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE)

  if (length(paths) == 0) {
    return(tibble::tibble(
      FilePath = character(),
      Journal = character(),
      ProfileName = character()
    ))
  }

  tibble::tibble(
    FilePath = paths,
    Journal = journal,
    ProfileName = journal
  )
}

files <- purrr::map_dfr(input_dirs, discover_pdfs)

if (nrow(files) == 0) {
  stop("No PDF files found in input_dirs.", call. = FALSE)
}

results <- purrr::pmap_dfr(
  files,
  function(FilePath, Journal, ProfileName) {
    analyze_paper_safe(
      path = FilePath,
      journal = Journal,
      profile = get_profile(ProfileName)
    )
  }
)

summary_metrics_overall <- summarise_metrics(results)
summary_metrics_by_journal <- summarise_metrics(results, group_cols = "Journal")
diagnostics <- build_diagnostics(results)
heading_candidates <- build_heading_candidates(results)
review_summary <- build_review_summary(diagnostics, heading_candidates)

readr::write_csv(results, file.path(output_dir, "paper_metrics.csv"))
readr::write_csv(summary_metrics_by_journal, file.path(output_dir, "summary_metrics_by_journal.csv"))
readr::write_csv(summary_metrics_overall, file.path(output_dir, "summary_metrics_overall.csv"))
readr::write_csv(diagnostics, file.path(output_dir, "diagnostics.csv"))
readr::write_csv(heading_candidates, file.path(output_dir, "heading_candidates.csv"))
readr::write_csv(review_summary, file.path(output_dir, "review_summary.csv"))

cat(
  paste0(
    "\nPaper information statistics report\n",
    "- PDF files discovered: ", nrow(files), "\n",
    "- Papers analyzed: ", nrow(results), "\n",
    "- Standard-structure papers: ", sum(results$IsStandardStructure, na.rm = TRUE), "\n",
    "- Papers needing review: ", sum(results$NeedsReview, na.rm = TRUE), "\n",
    "- Summary files: summary_metrics_by_journal.csv, summary_metrics_overall.csv\n",
    "- Output directory: ", output_dir, "\n"
  )
)

print(summary_metrics_by_journal, n = Inf)
