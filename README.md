# Paper Information Statistics

This project extracts structural statistics from academic paper PDFs:
page count, abstract word count, section word counts, figure/table counts,
caption density, reference count, and review diagnostics.

The current implementation is designed as a configurable pipeline rather
than a script tied to one or two journals. Journal-specific rules live in
`R/profiles.R`; reusable parsing and summarisation functions live in
`R/analyze_paper.R`.

## Quick Start

Requirements:

- R >= 4.1.0
- R packages: `pdftools`, `purrr`, `dplyr`, `tidyr`, `readr`, `stringr`,
  `tibble`, `here`

1. Restore or install the required R packages.

```r
renv::restore()
```

2. Put PDF files in one or more input folders.

3. Edit `default_input_dirs` in `paper_information_statistics.R`, or set the
   `paperstats.input_dirs` option, if you want to analyze different folders.

4. Run the script.

```r
source("paper_information_statistics.R")
```

To run a one-off folder without editing the script:

```r
options(paperstats.input_dirs = c("path/to/pdf_folder"))
source("paper_information_statistics.R")
```

## Outputs

The script writes six CSV files to `output/`:

- `paper_metrics.csv`: one row per paper, including section counts and
  detected structural features. `IsStandardStructure` means required sections
  were found; `NeedsReview` means the paper has missing sections, parsing
  errors, inferred section boundaries, or warning-level signals such as
  unusually long sections.
- `summary_metrics_by_journal.csv`: aggregate statistics by journal for papers
  with standard required structure.
- `summary_metrics_overall.csv`: aggregate statistics across all journals.
- `diagnostics.csv`: papers with `NeedsReview = TRUE`.
- `heading_candidates.csv`: probable heading lines extracted from papers with
  `NeedsReview = TRUE`, useful for extending profiles.
- `review_summary.csv`: compact review table that joins diagnostics with the
  most relevant candidate headings.

## Adding a New Journal

Add a profile to `paper_profiles` in `R/profiles.R`.

```r
paper_profiles$MyJournal <- make_profile(
  name = "MyJournal",
  header_patterns = c(
    "My Journal Name",
    "^\\s*https?://doi\\.org/"
  )
)
```

Then add the PDF folder to `default_input_dirs` in
`paper_information_statistics.R`, or pass it through `paperstats.input_dirs`.
If the folder name matches the profile name, that profile is used
automatically. If no matching profile exists, the default profile is used.

## Current Approach

The pipeline is intentionally rule-first:

- clean page-level noise such as page numbers, journal headers, and DOI lines;
- detect common paper sections with configurable regular expressions;
- map section variants such as `Materials and Methods` or `Background` to
  standard fields;
- infer missing Methods/Results boundaries from numbered topic headings when
  a paper lacks a conventional top-level IMRaD heading;
- record diagnostics for low-confidence or non-standard papers.

This keeps the system transparent and easy to debug. Machine learning or deep
learning can be added later for OCR, layout detection, caption boundary
detection, or semantic section classification once enough failure cases have
been collected.

## Recommended Next Steps

- Run the script on a mixed set of journals and inspect `diagnostics.csv` or
  `review_summary.csv`.
- Add profiles for journals that show repeated header/footer or heading
  patterns.
- Create a small hand-checked validation set to measure section detection
  accuracy and word-count error.
- Snapshot the full R environment with `renv::snapshot()` after dependencies
  are stable.

```r
readr::read_csv("output/review_summary.csv", show_col_types = FALSE)
```
