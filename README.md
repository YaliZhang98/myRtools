### YALZHA customized R package 

gee_to_docx()
Print summary(geeglm_model) output to docs file

`library(myRtools)`
- Single model \
`gee_to_docx(my_model)`

- Multiple models \
`models <- list("Model 1" = gee1, "Model 2" = gee2)` \
`gee_to_docx(models, filename = "results.docx")`
