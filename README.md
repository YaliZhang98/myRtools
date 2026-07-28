### YALZHA customized R package 

gee_to_docx()
Print summary(geeglm_model) output to docs file

library(myRtools)
- Single model
`format_gee_to_docx(my_model)`

- Multiple models
`models <- list("Model 1" = gee1, "Model 2" = gee2)
 format_gee_to_docx(models, filename = "results.docx")`
