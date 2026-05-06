#' Export GEE model summaries to a formatted Word document
#'
#' @param models A single geeglm model or a named list of geeglm models
#' @param filename Output filename (default: "GEE_summaries.docx")
#'
#' @return Invisibly returns the output file path
#' @export
#'
#' @examples
#' \dontrun{
#' # Single model
#' format_gee_to_docx(my_model)
#'
#' # Multiple models
#' models <- list("Model 1" = gee1, "Model 2" = gee2)
#' format_gee_to_docx(models, filename = "results.docx")
#' }

gee_to_docx <- function(models, filename = "GEE_summaries.docx") {
  
  # ── Handle single model input ──
  if (!is.list(models) || inherits(models, "geeglm")) {
    models <- list("Model" = models)
  }
  
  # ── If list has no names, auto-name them ──
  if (is.null(names(models))) {
    names(models) <- paste("Model", seq_along(models))
  }
  
  # ── Inner function: format one model into doc ──
  .format_one <- function(model, model_name, doc) {
    
    s <- summary(model)
    
    # 1. Capture full console output
    full_output <- capture.output(print(s))
    
    # 2. Extract coefficient table
    coef_df <- as.data.frame(s$coefficients) %>%
      tibble::rownames_to_column(var = "Term") %>%
      mutate(
        Stars = case_when(
          `Pr(>|W|)` < 0.001 ~ "***",
          `Pr(>|W|)` < 0.01  ~ "**",
          `Pr(>|W|)` < 0.05  ~ "*",
          `Pr(>|W|)` < 0.1   ~ ".",
          TRUE               ~ "\u00A0"
        ),
        `Pr(>|W|)` = case_when(
          `Pr(>|W|)` < 2e-16 ~ "< 2e-16",
          TRUE               ~ as.character(signif(`Pr(>|W|)`, 3))
        ),
        Estimate = round(Estimate, 6),
        Std.err  = round(Std.err,  6),
        Wald     = round(Wald,     4)
      )
    
    # 3. Monospace style
    mono_style <- fp_text(font.family = "Courier New", font.size = 9)
    
    # 4. Build flextable
    ft <- flextable(coef_df) %>%
      font(fontname = "Courier New", part = "all") %>%
      fontsize(size = 9, part = "all") %>%
      # bold(j = "Stars") %>%
      bg(~ Stars %in% c("***", "**", "*"), bg = "#EEEEEE") %>%
      set_header_labels(Stars = "Sig.") %>%
      align(j = -1, align = "right", part = "all") %>%
      align(j = 1,  align = "left",  part = "all") %>%
      border_remove() %>%
      hline_top(part = "header", border = fp_border(width = 1)) %>%
      hline_bottom(part = "header", border = fp_border(width = 1)) %>%
      hline_bottom(part = "body",   border = fp_border(width = 1)) %>%
      padding(padding.top    = 1, part = "all") %>%
      padding(padding.bottom = 1, part = "all") %>%
      padding(padding.left   = 3, part = "all") %>%
      padding(padding.right  = 3, part = "all") %>%
      autofit()
    
    # 5. Split console output into sections
    coef_start       <- grep("^\\s*Coefficients:", full_output)
    coef_end         <- grep("Signif\\. codes",    full_output)
    header_lines     <- full_output[1:(coef_start - 1)]
    coef_header_lines <- full_output[coef_start:(coef_start + 1)]
    footer_lines     <- full_output[coef_end:length(full_output)]
    
    # 6. Write to doc
    doc <- doc %>%
      body_add_par(model_name, style = "heading 1")
    
    for (line in header_lines) {
      doc <- doc %>% body_add_fpar(fpar(ftext(line, prop = mono_style)))
    }
    for (line in coef_header_lines) {
      doc <- doc %>% body_add_fpar(fpar(ftext(line, prop = mono_style)))
    }
    
    doc <- doc %>%
      body_add_flextable(ft) %>%
      body_add_par("")
    
    for (line in footer_lines) {
      doc <- doc %>% body_add_fpar(fpar(ftext(line, prop = mono_style)))
    }
    
    return(doc)
  }
  
  # ── Loop over all models ──
  doc <- read_docx()
  
  for (i in seq_along(models)) {
    name <- names(models)[i]
    doc  <- .format_one(models[[i]], name, doc)
    
    # Page break between models, not after last
    if (i < length(models)) {
      doc <- doc %>% body_add_break()
    }
  }
  
  # ── Save to working directory ──
  out_path <- file.path(getwd(), filename)
  print(doc, target = out_path)
  message("Saved to: ", out_path)
  invisible(out_path)
}




