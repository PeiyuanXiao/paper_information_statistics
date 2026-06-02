empty_location <- function() {
  matrix(
    NA_integer_,
    nrow = 1,
    ncol = 2,
    dimnames = list(NULL, c("start", "end"))
  )
}

loc_val <- function(mat, row = 1, col = 1) {
  if (is.null(mat) || all(is.na(mat))) return(NA_integer_)
  mat[row, col]
}

first_non_missing <- function(...) {
  values <- unlist(list(...))
  values <- values[!is.na(values)]
  if (length(values) == 0) NA_integer_ else values[[1]]
}

first_after <- function(lower_bound, ...) {
  if (is.na(lower_bound)) return(NA_integer_)

  values <- unlist(list(...))
  values <- values[!is.na(values) & values > lower_bound]
  if (length(values) == 0) NA_integer_ else values[[1]]
}

matches_any <- function(text, patterns) {
  if (length(patterns) == 0) return(rep(FALSE, length(text)))

  purrr::reduce(
    patterns,
    function(acc, pat) acc | stringr::str_detect(text, stringr::regex(pat, ignore_case = TRUE)),
    .init = rep(FALSE, length(text))
  )
}

try_locate <- function(text, patterns) {
  for (pat in patterns) {
    match <- stringr::str_locate(
      text,
      stringr::regex(pat, ignore_case = TRUE, multiline = TRUE)
    )
    if (!is.na(match[1, 1])) return(match)
  }

  empty_location()
}

build_line_index <- function(text) {
  lines <- stringr::str_split(text, "\n", simplify = FALSE)[[1]]
  starts <- cumsum(c(1L, nchar(lines[-length(lines)], type = "chars") + 1L))

  tibble::tibble(
    line = lines,
    line_start = starts,
    line_end = starts + nchar(lines, type = "chars") - 1L
  )
}

try_locate_in_lines <- function(line_index, patterns) {
  for (pat in patterns) {
    matches <- stringr::str_locate(
      line_index$line,
      stringr::regex(pat, ignore_case = TRUE)
    )
    idx <- which(!is.na(matches[, 1]))

    if (length(idx) > 0) {
      first_idx <- idx[[1]]
      return(matrix(
        c(
          line_index$line_start[[first_idx]] + matches[first_idx, 1] - 1L,
          line_index$line_start[[first_idx]] + matches[first_idx, 2] - 1L
        ),
        nrow = 1,
        dimnames = list(NULL, c("start", "end"))
      ))
    }
  }

  empty_location()
}

try_locate_fallback_in_lines <- function(line_index,
                                         patterns,
                                         lower_bound = NA_integer_,
                                         upper_bound = Inf,
                                         profile = NULL,
                                         max_line_chars = 240) {
  if (length(patterns) == 0) return(empty_location())

  squished_lines <- stringr::str_squish(line_index$line)
  eligible <- line_index$line_start <= upper_bound &
    nchar(squished_lines, type = "chars") <= max_line_chars

  if (!is.na(lower_bound)) {
    eligible <- eligible & line_index$line_start > lower_bound
  }

  if (!is.null(profile)) {
    eligible <- eligible & !matches_any(squished_lines, profile$caption_drop_patterns)
  }

  for (pat in patterns) {
    matches <- stringr::str_locate(
      line_index$line,
      stringr::regex(pat, ignore_case = TRUE)
    )
    idx <- which(eligible & !is.na(matches[, 1]))

    if (length(idx) > 0) {
      first_idx <- idx[[1]]
      return(matrix(
        c(
          line_index$line_start[[first_idx]] + matches[first_idx, 1] - 1L,
          line_index$line_start[[first_idx]] + matches[first_idx, 2] - 1L
        ),
        nrow = 1,
        dimnames = list(NULL, c("start", "end"))
      ))
    }
  }

  empty_location()
}

first_section_start <- function(locations, sections, default = Inf) {
  starts <- purrr::map_int(sections, \(section) loc_val(locations[[section]], 1, 1))
  starts <- starts[!is.na(starts)]
  if (length(starts) == 0) default else min(starts)
}

locate_last_reference_heading <- function(text) {
  matches <- stringr::str_locate_all(
    text,
    stringr::regex("\\bReferences\\b", ignore_case = TRUE)
  )[[1]]

  if (nrow(matches) == 0) return(empty_location())

  min_start <- nchar(text) * 0.45
  candidates <- matches[matches[, 1] >= min_start, , drop = FALSE]
  if (nrow(candidates) == 0) return(empty_location())

  last_idx <- nrow(candidates)
  matrix(
    candidates[last_idx, ],
    nrow = 1,
    dimnames = list(NULL, c("start", "end"))
  )
}

count_words <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(NA_integer_)
  stringr::str_count(text, "\\b[[:alpha:]]+(?:[-'][[:alpha:]]+)?\\b")
}

extract_between <- function(text, start_idx, end_idx) {
  if (is.na(start_idx) || is.na(end_idx) || start_idx > end_idx) return(NA_character_)
  stringr::str_sub(text, start_idx, end_idx)
}

strip_page_noise <- function(page_text, profile) {
  if (is.null(page_text) || length(page_text) == 0 || is.na(page_text)) {
    return("")
  }

  lines <- stringr::str_split(page_text, "\n")[[1]]
  drop_line <- stringr::str_detect(lines, "^\\s*\\d+\\s*$") |
    matches_any(lines, profile$header_patterns) |
    matches_any(lines, profile$footer_patterns)

  paste(lines[!drop_line], collapse = "\n")
}

drop_caption_lines <- function(text, profile) {
  if (is.na(text)) return(NA_character_)

  lines <- stringr::str_split(text, "\n")[[1]]
  keep <- !matches_any(lines, profile$caption_drop_patterns)
  paste(lines[keep], collapse = "\n")
}

locate_sections <- function(full_text, profile) {
  line_index <- build_line_index(full_text)
  purrr::map(profile$section_patterns, \(patterns) try_locate_in_lines(line_index, patterns))
}

infer_missing_section_locations <- function(full_text, locations, end_idx, profile) {
  fallback_patterns <- profile$section_fallback_patterns %||% list()
  if (length(fallback_patterns) == 0) return(locations)

  line_index <- build_line_index(full_text)
  warnings <- character()

  if (is.na(loc_val(locations$Methods, 1, 1))) {
    methods_upper <- first_section_start(
      locations,
      c("ResultsDiscussion", "Results", "DiscussionConclusion", "Discussion", "Conclusion", "References", "Appendix"),
      default = end_idx
    )
    methods_lower <- first_non_missing(loc_val(locations$Introduction, 1, 2), 0L)
    method_loc <- try_locate_fallback_in_lines(
      line_index,
      fallback_patterns$Methods %||% character(),
      lower_bound = methods_lower,
      upper_bound = min(methods_upper, end_idx, na.rm = TRUE),
      profile = profile
    )

    if (!is.na(loc_val(method_loc, 1, 1))) {
      locations$Methods <- method_loc
      warnings <- c(warnings, "Methods inferred from numbered/topic heading")
    }
  }

  if (is.na(loc_val(locations$ResultsDiscussion, 1, 1)) &&
      is.na(loc_val(locations$Results, 1, 1))) {
    results_lower <- first_non_missing(
      loc_val(locations$Methods, 1, 2),
      loc_val(locations$Introduction, 1, 2),
      0L
    )
    results_upper <- first_section_start(
      locations,
      c("DiscussionConclusion", "Discussion", "Conclusion", "References", "Appendix"),
      default = end_idx
    )
    results_discussion_patterns <- c(
      profile$section_patterns$ResultsDiscussion %||% character(),
      "\\b\\d+(\\.\\d+)*\\.?\\s+Results?\\s+and\\s+Discussion\\b",
      "\\bCase\\s+Study.*Results?\\s+and\\s+Discussion\\b"
    )
    results_discussion_loc <- try_locate_fallback_in_lines(
      line_index,
      results_discussion_patterns,
      lower_bound = results_lower,
      upper_bound = min(results_upper, end_idx, na.rm = TRUE),
      profile = profile
    )

    if (!is.na(loc_val(results_discussion_loc, 1, 1))) {
      locations$ResultsDiscussion <- results_discussion_loc
      warnings <- c(warnings, "Results and Discussion inferred from numbered/topic heading")
    } else {
      results_loc <- try_locate_fallback_in_lines(
        line_index,
        fallback_patterns$Results %||% character(),
        lower_bound = results_lower,
        upper_bound = min(results_upper, end_idx, na.rm = TRUE),
        profile = profile
      )

      if (!is.na(loc_val(results_loc, 1, 1))) {
        locations$Results <- results_loc
        warnings <- c(warnings, "Results inferred from numbered/topic heading")
      }
    }
  }

  attr(locations, "warnings") <- warnings
  locations
}

section_location_summary <- function(locations, end_idx = Inf) {
  found <- names(locations)[purrr::map_lgl(locations, function(loc) {
    start <- loc_val(loc, 1, 1)
    !is.na(start) && start <= end_idx
  })]
  paste(found, collapse = ";")
}

trim_locations_to_body <- function(locations, end_idx) {
  purrr::map(locations, function(loc) {
    start <- loc_val(loc, 1, 1)
    if (!is.na(start) && start > end_idx) empty_location() else loc
  })
}

extract_section_words <- function(full_text, locations, end_idx, profile) {
  results_discussion_found <- !is.na(loc_val(locations$ResultsDiscussion, 1, 1))
  discussion_conclusion_found <- !is.na(loc_val(locations$DiscussionConclusion, 1, 1))

  section_starts <- list(
    Introduction = loc_val(locations$Introduction, 1, 2),
    Methods = loc_val(locations$Methods, 1, 2),
    Results = if (results_discussion_found) {
      loc_val(locations$ResultsDiscussion, 1, 2)
    } else {
      loc_val(locations$Results, 1, 2)
    },
    Discussion = if (results_discussion_found) {
      NA_integer_
    } else if (discussion_conclusion_found) {
      loc_val(locations$DiscussionConclusion, 1, 2)
    } else {
      loc_val(locations$Discussion, 1, 2)
    },
    Conclusion = if (discussion_conclusion_found) {
      NA_integer_
    } else {
      loc_val(locations$Conclusion, 1, 2)
    }
  )

  section_starts <- Filter(Negate(is.na), section_starts)
  section_starts <- section_starts[order(unlist(section_starts))]

  section_words <- list()
  section_warnings <- character()

  for (i in seq_along(section_starts)) {
    section_name <- names(section_starts)[i]
    seg_start <- section_starts[[i]]
    seg_end <- if (i < length(section_starts)) section_starts[[i + 1]] - 1 else end_idx

    if (is.na(seg_start) || is.na(seg_end)) {
      section_words[[section_name]] <- NA_integer_
      next
    }

    if ((seg_end - seg_start) > profile$max_section_chars) {
      section_warnings <- c(section_warnings, paste(section_name, "exceeds max_section_chars"))
    }

    seg_text <- extract_between(full_text, seg_start, seg_end)
    seg_text <- drop_caption_lines(seg_text, profile)
    section_words[[section_name]] <- count_words(seg_text)
  }

  list(words = section_words, warnings = section_warnings)
}

summarise_combined_sections <- function(locations) {
  notes <- character()
  if (!is.na(loc_val(locations$ResultsDiscussion, 1, 1))) {
    notes <- c(notes, "Results and Discussion merged")
  }
  if (!is.na(loc_val(locations$DiscussionConclusion, 1, 1))) {
    notes <- c(notes, "Discussion and Conclusions merged")
  }
  paste(notes, collapse = "; ")
}

section_quality_warnings <- function(intro_words,
                                     methods_words,
                                     results_words,
                                     discuss_words,
                                     conclude_words) {
  thresholds <- c(
    Introduction = 150,
    Methods = 100,
    Results = 100,
    Discussion = 100,
    Conclusion = 50
  )
  values <- c(
    Introduction = intro_words,
    Methods = methods_words,
    Results = results_words,
    Discussion = discuss_words,
    Conclusion = conclude_words
  )

  purrr::imap_chr(values, function(value, name) {
    if (!is.na(value) && value < thresholds[[name]]) {
      paste0(name, " below min_words (", value, ")")
    } else {
      NA_character_
    }
  }) |>
    stats::na.omit() |>
    as.character()
}

invalid_inferred_required_sections <- function(location_warnings,
                                               methods_words,
                                               results_words,
                                               min_words = c(Methods = 50, Results = 50)) {
  invalid <- character()

  if (any(stringr::str_detect(location_warnings, "^Methods inferred")) &&
      (is.na(methods_words) || methods_words < min_words[["Methods"]])) {
    invalid <- c(invalid, "Methods")
  }

  if (any(stringr::str_detect(location_warnings, "^Results")) &&
      (is.na(results_words) || results_words < min_words[["Results"]])) {
    invalid <- c(invalid, "Results")
  }

  invalid
}

count_keywords <- function(keyword_text) {
  if (is.na(keyword_text) || nchar(stringr::str_trim(keyword_text)) == 0) {
    return(NA_integer_)
  }

  keyword_text |>
    stringr::str_trim() |>
    stringr::str_split("[,;\\n]") |>
    unlist() |>
    stringr::str_trim() |>
    (\(x) x[nchar(x) > 0])() |>
    length()
}

clean_abstract_candidate <- function(text) {
  if (is.na(text)) return(NA_character_)

  lines <- stringr::str_split(text, "\n")[[1]]
  lines <- stringr::str_squish(lines)
  lines <- lines[nchar(lines) > 0]

  drop_line <- stringr::str_detect(lines, stringr::regex(
    paste(c(
      "^A\\s+B\\s+S\\s+T\\s+R\\s+A\\s+C\\s+T$",
      "^A\\s+R\\s+T\\s+I\\s+C\\s+L\\s+E\\s+I\\s+N\\s+F\\s+O\\s+A\\s+B\\s+S\\s+T\\s+R\\s+A\\s+C\\s+T$",
      "^Keywords?\\b",
      "^Article\\s+history\\b",
      "^Received\\b",
      "^Revised\\b",
      "^Accepted\\b",
      "^Available\\s+online\\b"
    ), collapse = "|"),
    ignore_case = TRUE
  ))

  paste(lines[!drop_line], collapse = " ")
}

count_abstract_words <- function(full_text, locations, end_idx) {
  abstract_start <- loc_val(locations$Abstract, 1, 2)
  if (is.na(abstract_start)) return(NA_integer_)

  intro_start <- loc_val(locations$Introduction, 1, 1)
  primary_end <- first_after(
    abstract_start,
    loc_val(locations$Keywords, 1, 1),
    intro_start,
    end_idx
  )
  primary_count <- count_words(clean_abstract_candidate(
    extract_between(full_text, abstract_start, primary_end)
  ))

  if (!is.na(primary_count) && primary_count >= 20) {
    return(primary_count)
  }

  fallback_end <- first_after(abstract_start, intro_start, end_idx)
  fallback_count <- count_words(clean_abstract_candidate(
    extract_between(full_text, abstract_start, fallback_end)
  ))

  if (!is.na(fallback_count) && (is.na(primary_count) || fallback_count >= primary_count)) {
    fallback_count
  } else {
    primary_count
  }
}

count_references <- function(ref_text) {
  if (is.na(ref_text)) return(NA_integer_)

  numbered <- stringr::str_count(ref_text, "(?m)^\\s*\\[?\\d{1,3}\\]?\\.?\\s+[[:upper:]]")
  author_year <- stringr::str_count(ref_text, "(?m)^\\s*\\p{Lu}[\\p{L}'-]+,\\s+\\p{Lu}")
  max(numbered, author_year)
}

count_numbered_items <- function(text, pattern) {
  matches <- stringr::str_extract_all(text, stringr::regex(pattern, ignore_case = TRUE))[[1]]
  if (length(matches) == 0) return(0L)

  nums <- matches |>
    stringr::str_extract("\\d+") |>
    as.integer()

  length(unique(stats::na.omit(nums)))
}

analyze_paper <- function(path, journal, profile) {
  pages_txt <- pdftools::pdf_text(path)
  full_text <- pages_txt |>
    purrr::map_chr(\(page) strip_page_noise(page, profile)) |>
    paste(collapse = "\n")

  raw_locations <- locate_sections(full_text, profile)
  end_idx <- min(
    c(loc_val(raw_locations$References, 1, 1), loc_val(raw_locations$Appendix, 1, 1), nchar(full_text)),
    na.rm = TRUE
  )
  locations <- trim_locations_to_body(raw_locations, end_idx)
  locations <- infer_missing_section_locations(full_text, locations, end_idx, profile)
  location_warnings <- attr(locations, "warnings") %||% character()

  section_result <- extract_section_words(full_text, locations, end_idx, profile)
  section_words <- section_result$words
  get_sec <- function(name) section_words[[name]] %||% NA_integer_

  intro_words <- get_sec("Introduction")
  methods_words <- get_sec("Methods")
  results_words <- get_sec("Results")
  discuss_words <- get_sec("Discussion")
  conclude_words <- get_sec("Conclusion")

  core_words <- sum(unlist(section_words), na.rm = TRUE)
  pct <- function(x) {
    if (is.na(x) || core_words == 0) return(NA_real_)
    round(x / core_words * 100, 1)
  }

  keyword_text <- extract_between(
    full_text,
    loc_val(locations$Keywords, 1, 2),
    first_non_missing(loc_val(locations$Introduction, 1, 1), end_idx)
  )
  reference_location <- raw_locations$References
  if (is.na(loc_val(reference_location, 1, 1))) {
    reference_location <- locate_last_reference_heading(full_text)
  }

  ref_text <- extract_between(
    full_text,
    loc_val(reference_location, 1, 2),
    first_after(
      loc_val(reference_location, 1, 2),
      loc_val(raw_locations$Appendix, 1, 1),
      nchar(full_text)
    )
  )

  fig_captions <- stringr::str_extract_all(
    full_text,
    stringr::regex("\\bFig(?:ure)?\\.?\\s*\\d+[A-Za-z]?[^\n]*", ignore_case = TRUE)
  )[[1]]
  caption_words <- count_words(paste(fig_captions, collapse = " "))

  missing_required <- c(
    if (is.na(intro_words)) "Introduction",
    if (is.na(methods_words)) "Methods",
    if (is.na(results_words)) "Results",
    invalid_inferred_required_sections(location_warnings, methods_words, results_words)
  ) |>
    unique()
  n_references <- count_references(ref_text)
  is_standard_structure <- length(missing_required) == 0
  section_warning_text <- paste(
    c(
      location_warnings,
      section_result$warnings,
      if (is.na(n_references)) "References missing" else character(),
      section_quality_warnings(
        intro_words,
        methods_words,
        results_words,
        discuss_words,
        conclude_words
      )
    ),
    collapse = ";"
  )

  tibble::tibble(
    FileName = basename(path),
    SourcePath = normalizePath(path, winslash = "/", mustWork = FALSE),
    Journal = journal,
    ProfileName = profile$name,
    TotalPages = length(pages_txt),
    LocatedSections = section_location_summary(locations, end_idx),
    MissingRequiredSections = paste(missing_required, collapse = ";"),
    CombinedSections = summarise_combined_sections(locations),
    SectionWarnings = section_warning_text,
    AbstractWords = count_abstract_words(full_text, locations, end_idx),
    NKeywords = count_keywords(keyword_text),
    IntroWords = intro_words,
    IntroPct = pct(intro_words),
    MethodsWords = methods_words,
    MethodsPct = pct(methods_words),
    ResultsWords = results_words,
    ResultsPct = pct(results_words),
    DiscussWords = discuss_words,
    DiscussPct = pct(discuss_words),
    ConcludeWords = conclude_words,
    ConcludePct = pct(conclude_words),
    CoreWords = core_words,
    NFigures = count_numbered_items(full_text, "\\bFig(?:ure)?\\.?\\s*\\d+[A-Za-z]?"),
    NTables = count_numbered_items(full_text, "\\bTable\\s*\\d+[A-Za-z]?"),
    CaptionDensityPct = if (!is.na(caption_words) && core_words > 0) {
      round(caption_words / core_words * 100, 1)
    } else {
      NA_real_
    },
    NReferences = n_references,
    RefPerPage = round(n_references / length(pages_txt), 1),
    HasAppendix = !is.na(loc_val(locations$Appendix, 1, 1)),
    IsStandardStructure = is_standard_structure,
    NeedsReview = !is_standard_structure || section_warning_text != "",
    ErrorMessage = NA_character_
  )
}

analyze_paper_safe <- function(path, journal, profile) {
  tryCatch(
    analyze_paper(path, journal, profile),
    error = function(e) {
      tibble::tibble(
        FileName = basename(path),
        SourcePath = normalizePath(path, winslash = "/", mustWork = FALSE),
        Journal = journal,
        ProfileName = profile$name,
        TotalPages = NA_integer_,
        LocatedSections = NA_character_,
        MissingRequiredSections = NA_character_,
        CombinedSections = NA_character_,
        SectionWarnings = NA_character_,
        AbstractWords = NA_integer_,
        NKeywords = NA_integer_,
        IntroWords = NA_integer_,
        IntroPct = NA_real_,
        MethodsWords = NA_integer_,
        MethodsPct = NA_real_,
        ResultsWords = NA_integer_,
        ResultsPct = NA_real_,
        DiscussWords = NA_integer_,
        DiscussPct = NA_real_,
        ConcludeWords = NA_integer_,
        ConcludePct = NA_real_,
        CoreWords = NA_integer_,
        NFigures = NA_integer_,
        NTables = NA_integer_,
        CaptionDensityPct = NA_real_,
        NReferences = NA_integer_,
        RefPerPage = NA_real_,
        HasAppendix = NA,
        IsStandardStructure = FALSE,
        NeedsReview = TRUE,
        ErrorMessage = e$message
      )
    }
  )
}

safe_stat <- function(x, fn) {
  values <- x[!is.na(x)]
  if (length(values) == 0) return(NA_real_)
  fn(values)
}

safe_sd <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) < 2) return(NA_real_)
  round(stats::sd(values), 1)
}

summarise_metrics <- function(results, group_cols = character()) {
  metric_cols <- c(
    "TotalPages", "AbstractWords", "IntroWords", "MethodsWords", "ResultsWords",
    "DiscussWords", "ConcludeWords", "CoreWords", "NFigures", "NTables", "NReferences"
  )

  standard <- results |>
    dplyr::filter(.data$IsStandardStructure)

  if (nrow(standard) == 0) {
    return(tibble::tibble())
  }

  if (length(group_cols) > 0) {
    standard <- standard |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  standard |>
    dplyr::summarise(
      NAnalyzed = dplyr::n(),
      dplyr::across(
        dplyr::all_of(metric_cols),
        list(
          NValid = \(x) sum(!is.na(x)),
          mean = \(x) round(safe_stat(x, mean), 1),
          median = \(x) safe_stat(x, stats::median),
          sd = \(x) safe_sd(x),
          min = \(x) safe_stat(x, min),
          max = \(x) safe_stat(x, max)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("__"),
      names_to = c("Metric", "Stat"),
      names_sep = "__"
    ) |>
    tidyr::pivot_wider(names_from = "Stat", values_from = "value") |>
    dplyr::relocate(dplyr::all_of(group_cols), .before = "Metric")
}

build_diagnostics <- function(results) {
  results |>
    dplyr::filter(.data$NeedsReview) |>
    dplyr::select(dplyr::all_of(c(
      "FileName",
      "Journal",
      "ProfileName",
      "IsStandardStructure",
      "NeedsReview",
      "MissingRequiredSections",
      "LocatedSections",
      "CombinedSections",
      "SectionWarnings",
      "ErrorMessage",
      "SourcePath"
    )))
}

is_probable_heading <- function(lines) {
  numbered_heading <- stringr::str_detect(
    lines,
    "^\\d+(\\.\\d+)*\\.?\\s+[[:upper:]][^;]{0,110}$"
  )

  known_heading <- stringr::str_detect(lines, stringr::regex(
    paste(c(
      "^Abstract\\b",
      "^A\\s+B\\s+S\\s+T\\s+R\\s+A\\s+C\\s+T\\b",
      "^A\\s+R\\s+T\\s+I\\s+C\\s+L\\s+E\\s+I\\s+N\\s+F\\s+O\\s+A\\s+B\\s+S\\s+T\\s+R\\s+A\\s+C\\s+T\\b",
      "^Keywords?\\b",
      "^Introduction\\b",
      "^Background\\b",
      "^Materials?\\b",
      "^Methods?\\b",
      "^Results?\\b",
      "^Discussion\\b",
      "^Conclusions?\\b",
      "^References\\b",
      "^Appendix\\b"
    ), collapse = "|"),
    ignore_case = TRUE
  ))

  topic_heading <- stringr::str_detect(lines, stringr::regex(
    "\\b(method|material|sample|data|analysis|result|assessment|discussion|conclusion|model|study|case|context|framework|protocol|experiment|chronolog|setting|background)\\b",
    ignore_case = TRUE
  )) &
    stringr::str_detect(lines, "^[[:upper:]][[:alnum:][:space:][:punct:]]+$") &
    stringr::str_count(lines, "\\b[[:alpha:]]+\\b") <= 12

  bad_fragment <- stringr::str_detect(lines, stringr::regex(
    paste(c(
      "^\\d+\\s+Page\\s+\\d+\\s+of\\s+\\d+",
      "^Extended\\s+author\\s+information\\b",
      "^Declaration\\s+of\\s+competing\\s+interest",
      "^Authors?\\s+and\\s+Affiliations\\b",
      "\\bUniversity\\b",
      "\\bInstitute\\b",
      "\\bDepartment\\b",
      "\\bhttps?://",
      "^Cal,Qz\\b",
      "^Clay,Qz\\b",
      "^Ti\\s+V\\s+Cr\\s+Ga\\s+Sr\\b",
      "^[[:upper:][:space:],]+$"
    ), collapse = "|"),
    ignore_case = TRUE
  ))

  (numbered_heading | known_heading | topic_heading) & !bad_fragment
}

extract_heading_candidates <- function(path, journal, max_line_chars = 120) {
  tryCatch({
    pages_txt <- pdftools::pdf_text(path)

    purrr::imap_dfr(pages_txt, function(page_text, page_number) {
      lines <- stringr::str_split(page_text, "\n")[[1]]
      lines <- stringr::str_squish(lines)
      lines <- lines[nchar(lines) > 0 & nchar(lines) <= max_line_chars]

      lines <- lines[is_probable_heading(lines)]
      lines <- lines[
        !stringr::str_detect(lines, "^\\d+$") &
          !stringr::str_detect(lines, stringr::regex("^https?://|^doi:|journal of|contents lists", ignore_case = TRUE))
      ]

      tibble::tibble(
        FileName = basename(path),
        Journal = journal,
        Page = page_number,
        CandidateHeading = lines
      )
    })
  }, error = function(e) {
    tibble::tibble(
      FileName = basename(path),
      Journal = journal,
      Page = NA_integer_,
      CandidateHeading = paste("ERROR:", e$message)
    )
  })
}

build_heading_candidates <- function(results) {
  review_results <- results |>
    dplyr::filter(.data$NeedsReview) |>
    dplyr::select(dplyr::all_of(c("SourcePath", "Journal")))

  if (nrow(review_results) == 0) {
    return(tibble::tibble(
      FileName = character(),
      Journal = character(),
      Page = integer(),
      CandidateHeading = character()
    ))
  }

  purrr::pmap_dfr(
    review_results,
    function(SourcePath, Journal) extract_heading_candidates(SourcePath, Journal)
  )
}

build_review_summary <- function(diagnostics, heading_candidates, max_headings = 18) {
  if (nrow(diagnostics) == 0) {
    return(tibble::tibble())
  }

  candidate_summary <- heading_candidates |>
    dplyr::distinct(.data$FileName, .data$CandidateHeading, .keep_all = TRUE) |>
    dplyr::group_by(.data$FileName) |>
    dplyr::summarise(
      CandidateHeadings = paste(utils::head(.data$CandidateHeading, max_headings), collapse = " | "),
      .groups = "drop"
    )

  diagnostics |>
    dplyr::left_join(candidate_summary, by = "FileName") |>
    dplyr::select(dplyr::all_of(c(
      "FileName",
      "Journal",
      "IsStandardStructure",
      "MissingRequiredSections",
      "SectionWarnings",
      "CandidateHeadings",
      "ErrorMessage",
      "SourcePath"
    )))
}
