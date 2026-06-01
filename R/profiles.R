`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

default_section_patterns <- function() {
  list(
    Abstract = c(
      "^\\s*Abstract\\b",
      "^\\s*A\\s+B\\s+S\\s+T\\s+R\\s+A\\s+C\\s+T\\b",
      "^\\s*A\\s+R\\s+T\\s+I\\s+C\\s+L\\s+E\\s+I\\s+N\\s+F\\s+O\\s+A\\s+B\\s+S\\s+T\\s+R\\s+A\\s+C\\s+T\\b"
    ),
    Keywords = c("^\\s*Keywords?\\b[:.]?"),
    Introduction = c(
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Introduction\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Background\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Background\\s+and\\s+Context\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Theoretical\\s+Background\\b"
    ),
    Methods = c(
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Materials?\\s+and\\s+Methods?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Methods?\\s+and\\s+Materials?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Data\\s+and\\s+Methods?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Samples?\\s+and\\s+Methods?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Analytical\\s+Methods?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Experimental\\s+Methods?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Methodological\\s+Approach\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Study\\s+Protocol\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Research\\s+Framework\\s+and\\s+Methodology\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Methods?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Methodology\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Experimental\\s+Design\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Materials?\\b",
      "^\\s*\\d+(\\.\\d+)*\\.?\\s+.*\\bMethods?\\b",
      "^\\s*\\d+(\\.\\d+)*\\.?\\s+.*\\bMethodology\\b"
    ),
    ResultsDiscussion = c(
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Results?\\s+and\\s+Discussion\\b"
    ),
    Results = c(
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Results?\\s+and\\s+Analys(e|i)s\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Results?\\s+and\\s+Assessment\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Analytical\\s+Results?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Results?\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Analysis\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Findings\\b",
      "^\\s*\\d+(\\.\\d+)*\\.?\\s+.*\\bResults?\\b"
    ),
    DiscussionConclusion = c(
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Discussion\\s+and\\s+Conclusions?(\\s+and\\s+Future\\s+Work)?\\b"
    ),
    Discussion = c("^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Discussion\\b"),
    Conclusion = c(
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Conclusions?\\s+and\\s+Future\\s+Work\\b",
      "^\\s*(\\d+(\\.\\d+)*\\.?\\s+)?Conclusions?\\b"
    ),
    References = c("^\\s*References\\b", "^\\s*Bibliography\\b"),
    Appendix = c("^\\s*Appendix\\b", "^\\s*Supplementary\\s+Material\\b")
  )
}

make_profile <- function(name,
                         header_patterns = character(),
                         footer_patterns = character(),
                         section_patterns = default_section_patterns(),
                         caption_drop_patterns = NULL,
                         max_section_chars = 100000) {
  list(
    name = name,
    header_patterns = header_patterns,
    footer_patterns = footer_patterns,
    section_patterns = section_patterns,
    caption_drop_patterns = caption_drop_patterns %||% c(
      "^\\s*Fig(?:ure)?\\.?\\s*\\d+[A-Za-z]?[.:]?.*$",
      "^\\s*Table\\s*\\d+[A-Za-z]?[.:]?.*$"
    ),
    max_section_chars = max_section_chars
  )
}

paper_profiles <- list(
  default = make_profile(
    name = "default",
    header_patterns = c(
      "^\\s*https?://doi\\.org/",
      "^\\s*doi:",
      "^\\s*Copyright\\b",
      "^\\s*Available\\s+online\\b"
    )
  ),
  JAMT_paper_sample = make_profile(
    name = "JAMT_paper_sample",
    header_patterns = c(
      "^\\s*https?://doi\\.org/",
      "J\\s+Arch\\s+Method\\s+Theory",
      "Journal\\s+of\\s+Archaeological\\s+Method\\s+and\\s+Theory"
    )
  ),
  JAS_paper_sample = make_profile(
    name = "JAS_paper_sample",
    header_patterns = c(
      "^\\s*https?://doi\\.org/",
      "Journal\\s+of\\s+Archaeological\\s+Science",
      "^\\s*Contents\\s+lists\\s+available\\s+at",
      "^\\s*Available\\s+online\\b"
    )
  )
)

get_profile <- function(profile_name = "default") {
  paper_profiles[[profile_name]] %||% paper_profiles$default
}
