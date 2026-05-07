#' Export GEE model summaries to a formatted Word document
#'
#' @param models A single geeglm model or a named list of geeglm models
#' @param filename Output filename (default: "GEE_summaries.docx")
#'
#' @return Invisibly returns the output file path
#' @importFrom officer read_docx body_add_par body_add_fpar body_add_break body_add_flextable fp_text fp_border fpar ftext
#' @importFrom flextable flextable font fontsize bold bg set_header_labels align border_remove hline_top hline_bottom padding autofit
#' @importFrom dplyr mutate case_when
#' @importFrom tibble rownames_to_column
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

format_gee_to_docx <- function(models, filename = "GEE_summaries.docx") {
  
  if (!is.list(models) || inherits(models, "geeglm")) {
    models <- list("Model" = models)
  }
  if (is.null(names(models))) {
    names(models) <- paste("Model", seq_along(models))
  }
  
  .format_one <- function(model, model_name, doc) {
    
    s           <- summary(model)
    full_output <- capture.output(print(s))
    
    coef_df <- as.data.frame(s$coefficients) %>%
      tibble::rownames_to_column(var = "Term") %>%
      dplyr::mutate(
        Stars = dplyr::case_when(
          `Pr(>|W|)` < 0.001 ~ "***",
          `Pr(>|W|)` < 0.01  ~ "**",
          `Pr(>|W|)` < 0.05  ~ "*",
          `Pr(>|W|)` < 0.1   ~ ".",
          TRUE               ~ "\u00A0"
        ),
        `Pr(>|W|)` = dplyr::case_when(
          `Pr(>|W|)` < 2e-16 ~ "< 2e-16",
          TRUE               ~ as.character(signif(`Pr(>|W|)`, 3))
        ),
        Estimate = round(Estimate, 6),
        Std.err  = round(Std.err,  6),
        Wald     = round(Wald,     4)
      )
    
    mono_style <- officer::fp_text(font.family = "Courier New", font.size = 9)
    
    ft <- flextable::flextable(coef_df) %>%
      flextable::font(fontname = "Courier New", part = "all") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::bold(j = "Stars") %>%
      flextable::bg(~ Stars %in% c("***", "**", "*"), bg = "#EEEEEE") %>%
      flextable::set_header_labels(Stars = "Sig.") %>%
      flextable::align(j = -1, align = "right", part = "all") %>%
      flextable::align(j = 1,  align = "left",  part = "all") %>%
      flextable::border_remove() %>%
      flextable::hline_top(part = "header", border = officer::fp_border(width = 1)) %>%
      flextable::hline_bottom(part = "header", border = officer::fp_border(width = 1)) %>%
      flextable::hline_bottom(part = "body",   border = officer::fp_border(width = 1)) %>%
      flextable::padding(padding.top = 1, padding.bottom = 1,
                         padding.left = 3, padding.right = 3, part = "all") %>%
      flextable::autofit()
    
    coef_start        <- grep("^\\s*Coefficients:", full_output)
    coef_end          <- grep("Signif\\. codes",    full_output)
    header_lines      <- full_output[1:(coef_start - 1)]
    coef_header_lines <- full_output[coef_start:(coef_start + 1)]
    footer_lines      <- full_output[coef_end:length(full_output)]
    
    doc <- doc %>%
      officer::body_add_par(model_name, style = "heading 1")
    
    for (line in c(header_lines, coef_header_lines)) {
      doc <- doc %>%
        officer::body_add_fpar(officer::fpar(officer::ftext(line, prop = mono_style)))
    }
    
    doc <- doc %>%
      officer::body_add_flextable(ft) %>%
      officer::body_add_par("")
    
    for (line in footer_lines) {
      doc <- doc %>%
        officer::body_add_fpar(officer::fpar(officer::ftext(line, prop = mono_style)))
    }
    
    return(doc)
  }
  
  doc <- officer::read_docx()
  
  for (i in seq_along(models)) {
    doc <- .format_one(models[[i]], names(models)[i], doc)
    if (i < length(models)) doc <- doc %>% officer::body_add_break()
  }
  
  out_path <- file.path(getwd(), filename)
  print(doc, target = out_path)
  message("Saved to: ", out_path)
  invisible(out_path)
}


