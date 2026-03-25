install.packages(c("pdftools", "tidyverse", "here"))
library(pdftools)
library(tidyverse)
library(stringr)
library(here)

# --- 1. 设置路径 ---
pdf_folder <- here("JAMT_paper_sample")  # 若有其他期刊则更改路径
pdf_folder <- here("JAS_paper_sample")
files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# --- 2. 辅助函数 ---
count_words <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(NA_integer_)
  str_count(text, "[A-Za-z]+")
}

extract_between <- function(text, start_idx, end_idx) {
  if (is.na(start_idx) || is.na(end_idx) || start_idx > end_idx) return(NA_character_)
  str_sub(text, start_idx, end_idx)
}

loc_val <- function(mat, row = 1, col = 1) {
  if (is.null(mat) || all(is.na(mat))) return(NA_integer_)
  mat[row, col]
}

try_locate <- function(text, patterns) {
  for (pat in patterns) {
    m <- str_locate(text, regex(pat, ignore_case = TRUE))
    if (!is.na(m[1, 1])) return(m)
  }
  matrix(NA_integer_, nrow = 1, ncol = 2,
         dimnames = list(NULL, c("start", "end")))
}

dehydrate <- function(text) {
  if (is.na(text)) return(NA_character_)
  text |>
    str_replace_all("(?m)^\\s*(?i)Fig(?:ure)?\\.?\\s*\\d+[^\n]*\n", "") |>
    str_replace_all("(?m)^\\s*(?i)Table\\s*\\d+[^\n]*\n",            "")
}

# --- 3. 主函数 ---
clean_analyze_paper <- function(path) {
  tryCatch({
    
    # 3.1 读取并清理页眉页脚
    pages_txt <- pdf_text(path)
    
    full_text <- map_chr(pages_txt, function(p) {
      lines <- str_split(p, "\n")[[1]]
      lines <- lines[
        !str_detect(lines, "^\\s*\\d+\\s*$") &
          !str_detect(lines, "J Arch Method Theory") &
          !str_detect(lines, "https://doi.org/")
      ]
      paste(lines, collapse = "\n")
    }) |> paste(collapse = "\n")
    
    # 3.2 定位各节
    
    abstract_pos <- try_locate(full_text, c("\\nAbstract\\b"))
    
    keyword_pos  <- try_locate(full_text, c("\\nKeywords?[.:]?\\s"))
    
    intro_pos    <- try_locate(full_text, c(
      "\\n1\\.?\\s+Introduction\\b",
      "\\nIntroduction\\b",
      "\\nBackground\\b"          # 无标准引言标题的文章
    ))
    
    method_pos   <- try_locate(full_text, c(
      "\\nMaterials?\\s+and\\s+Methods?\\b",   # Materials and Methods / Material and Methods
      "\\n\\d\\.?\\s+Methods?\\b",             # 2. Methods
      "\\nMethods?\\b"                         # Methods
    ))
    
    # Results：优先检测合并节
    result_discuss_combined_pos <- try_locate(full_text, c(
      "\\nResults?\\s+and\\s+Discussion\\b"
    ))
    results_discuss_combined <- !is.na(loc_val(result_discuss_combined_pos, 1, 1))
    
    result_pos <- if (results_discuss_combined) {
      result_discuss_combined_pos
    } else {
      try_locate(full_text, c(
        "\\n\\d\\.?\\s+Results?\\b",
        "\\nResults?\\b",
        "\\nAnalysis\\b"  
      ))
    }
    
    # Discussion：优先检测合并节（Discussion and Conclusions）
    discuss_conclude_combined_pos <- try_locate(full_text, c(
      "\\nDiscussion\\s+and\\s+Conclusions?(?:\\s+and\\s+Future\\s+Work)?\\b"
    ))
    discuss_conclude_combined <- !is.na(loc_val(discuss_conclude_combined_pos, 1, 1))
    
    discuss_pos <- if (results_discuss_combined) {
      # Results 和 Discussion 已合并，Discussion 无独立起点
      matrix(NA_integer_, nrow = 1, ncol = 2, dimnames = list(NULL, c("start", "end")))
    } else if (discuss_conclude_combined) {
      discuss_conclude_combined_pos
    } else {
      try_locate(full_text, c(
        "\\n\\d\\.?\\s+Discussion\\b",
        "\\nDiscussion\\b"
      ))
    }
    
    conclude_pos <- if (discuss_conclude_combined) {
      # Discussion 和 Conclusion 已合并，Conclusion 无独立起点
      matrix(NA_integer_, nrow = 1, ncol = 2, dimnames = list(NULL, c("start", "end")))
    } else {
      try_locate(full_text, c(
        "\\nConclusions?\\s+and\\s+Future\\s+Work\\b",
        "\\n\\d\\.?\\s+Conclusions?\\b",
        "\\nConclusions?\\b",
        "\\nConclusion\\b"
      ))
    }
    
    ref_pos      <- try_locate(full_text, c("\\nReferences\\b", "\\nBibliography\\b"))
    appendix_pos <- try_locate(full_text, c("\\nAppendix\\b"))
    
    # 正文终点
    end_idx <- min(
      loc_val(ref_pos,      1, 1),
      loc_val(appendix_pos, 1, 1),
      nchar(full_text),
      na.rm = TRUE
    )
    
    # 3.3 合并节标注
    combined_note <- case_when(
      results_discuss_combined && discuss_conclude_combined ~
        "Results+Discussion merged; Discussion+Conclusions merged",
      results_discuss_combined  ~ "Results and Discussion merged",
      discuss_conclude_combined ~ "Discussion and Conclusions merged",
      TRUE ~ ""
    )
    
    # 3.4 各章节词数
    
    # 构建节名 → 起始位置
    section_starts <- list(
      Introduction = loc_val(intro_pos,    1, 2),
      Methods      = loc_val(method_pos,   1, 2),
      Results      = loc_val(result_pos,   1, 2),
      Discussion   = loc_val(discuss_pos,  1, 2),
      Conclusion   = loc_val(conclude_pos, 1, 2)
    )
    
    # 过滤未找到的节并按位置排序
    section_starts <- Filter(Negate(is.na), section_starts)
    section_starts <- section_starts[order(unlist(section_starts))]
    
    # 对每节截取文本并统计词数；超过30000字符视为定位异常
    MAX_SECTION_CHARS <- 30000
    section_words <- list()
    
    for (i in seq_along(section_starts)) {
      seg_start <- section_starts[[i]]
      seg_end   <- if (i < length(section_starts)) section_starts[[i + 1]] - 1 else end_idx
      
      if (is.na(seg_start) || is.na(seg_end)) {
        section_words[[names(section_starts)[i]]] <- NA_integer_
        next
      }
      
      if ((seg_end - seg_start) > MAX_SECTION_CHARS) {
        warning(paste(basename(path), "- 节", names(section_starts)[i],
                      "长度异常，已标记为 NA"))
        section_words[[names(section_starts)[i]]] <- NA_integer_
        next
      }
      
      seg_text <- extract_between(full_text, seg_start, seg_end) |> dehydrate()
      section_words[[names(section_starts)[i]]] <- count_words(seg_text)
    }
    
    get_sec <- function(name) section_words[[name]] %||% NA_integer_
    
    intro_words    <- get_sec("Introduction")
    methods_words  <- get_sec("Methods")
    results_words  <- get_sec("Results")
    discuss_words  <- get_sec("Discussion")
    conclude_words <- get_sec("Conclusion")
    
    total_core_words <- sum(unlist(section_words), na.rm = TRUE)
    
    pct <- function(x) {
      if (is.na(x) || total_core_words == 0) return(NA_real_)
      round(x / total_core_words * 100, 1)
    }
    
    # 3.5 文体与引用习惯
    # 摘要词数
    abstract_text <- extract_between(
      full_text,
      start_idx = loc_val(abstract_pos, 1, 2),
      end_idx   = coalesce(loc_val(keyword_pos, 1, 1), loc_val(intro_pos, 1, 1))
    )
    abstract_words <- count_words(abstract_text)
    
    # 关键词数量
    keyword_text <- extract_between(
      full_text,
      start_idx = loc_val(keyword_pos, 1, 2),
      end_idx   = loc_val(intro_pos, 1, 1)
    )
    n_keywords <- if (!is.na(keyword_text) && nchar(str_trim(keyword_text)) > 0) {
      keyword_text |>
        str_trim() |>
        str_split("[,;·•\n]") |>
        unlist() |>
        str_trim() |>
        (\(x) x[nchar(x) > 0])() |>
        length()
    } else NA_integer_
    
    # 参考文献条数
    ref_text <- extract_between(
      full_text,
      start_idx = loc_val(ref_pos, 1, 2),
      end_idx   = coalesce(loc_val(appendix_pos, 1, 1), nchar(full_text))
    )
    n_references <- if (!is.na(ref_text)) {
      numbered  <- str_count(ref_text, "(?m)^\\s*\\[?\\d{1,3}\\]?\\.?\\s+[A-Z]")
      author_yr <- str_count(ref_text, "(?m)^\\s*[A-Z][a-záéíóú]+,\\s+[A-Z]")
      max(numbered, author_yr)
    } else NA_integer_
    
    # 图注字数与密度
    fig_captions  <- str_extract_all(full_text, "(?i)Fig(?:ure)?\\.?\\s*\\d+[^\n]*")[[1]]
    caption_words <- count_words(paste(fig_captions, collapse = " "))
    caption_density <- if (!is.na(caption_words) && total_core_words > 0)
      round(caption_words / total_core_words * 100, 1) else NA_real_
    
    # 图表唯一数量
    fig_nums <- str_extract_all(full_text, "(?i)Fig(?:ure)?\\.?\\s?\\d+") |>
      unlist() |> str_extract("\\d+") |> as.integer()
    tab_nums <- str_extract_all(full_text, "(?i)Table\\s?\\d+") |>
      unlist() |> str_extract("\\d+") |> as.integer()
    
    # 3.6 汇总输出
    tibble(
      FileName          = basename(path),
      TotalPages        = length(pages_txt),
      CombinedSections  = combined_note,  
      
      # 摘要与关键词
      AbstractWords     = abstract_words,
      NKeywords         = n_keywords,
      
      # 各章节词数及占比
      IntroWords        = intro_words,
      IntroPct          = pct(intro_words),
      MethodsWords      = methods_words,
      MethodsPct        = pct(methods_words),
      ResultsWords      = results_words,
      ResultsPct        = pct(results_words),
      DiscussWords      = discuss_words,
      DiscussPct        = pct(discuss_words),
      ConcludeWords     = conclude_words,
      ConcludePct       = pct(conclude_words),
      CoreWords         = total_core_words,
      
      # 图表
      NFigures          = length(unique(fig_nums)),
      NTables           = length(unique(tab_nums)),
      CaptionDensityPct = caption_density,
      
      # 参考文献
      NReferences       = n_references,
      RefPerPage        = round(n_references / length(pages_txt), 1),
      
      HasAppendix       = !is.na(loc_val(appendix_pos, 1, 1))
    )
    
  }, error = function(e) {
    message(paste("Error:", basename(path), "-", e$message))
    NULL
  })
}

# --- 4. 批量执行 ---
results <- map(files, clean_analyze_paper) |> list_rbind()

# --- 5. 数据清洗 ---
results_cleaned <- results |>
  mutate(
    IsStandardStructure = !is.na(IntroWords) & 
      !is.na(MethodsWords) & 
      !is.na(ResultsWords)
  )

cat(paste0("\n数据清洗报告：\n",
           "- 成功读取样本：", nrow(results_cleaned), "\n",
           "- 结构规范样本：", sum(results_cleaned$IsStandardStructure), "\n",
           "- 被排除样本数：", sum(!results_cleaned$IsStandardStructure), "\n"))

# --- 6. 汇总输出 ---
final_summary <- results_cleaned |>
  filter(IsStandardStructure) |>  
  summarise(across(
    c(TotalPages, AbstractWords, IntroWords, MethodsWords, ResultsWords, 
      DiscussWords, ConcludeWords, CoreWords, 
      NFigures, NTables, NReferences),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd     = ~round(sd(.x, na.rm = TRUE), 1),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(), names_to = c("Metric", "Stat"), names_sep = "__") |>
  pivot_wider(names_from = Stat, values_from = value)

print(final_summary, n = Inf)

