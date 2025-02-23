library(readxl)
source("funs.R")

file_name <- "models_hyperparameters.xlsx"
sheets <- excel_sheets(file_name)
models_hyperparameters <- sapply(sheets, function(x){
	readxl::read_excel(file_name, sheet = x)
}, simplify = FALSE)

models_hyperparameters <- get_excel_param_all(models_hyperparameters)
default_params <- as.logical(models_hyperparameters$default)

#### ---- Random Forest -------------
rf_param <- models_hyperparameters$rf
num.trees <- rf_param$num.trees
regularization.factor <- rf_param$regularization.factor

if (default_params) {
   rf_tunegrid <- NULL
} else {
   mtry <- rf_param$mtry
   min.node.size <- rf_param$min.node.size
   splitrule <- rf_param$splitrule
   rf_tunegrid <- expand.grid(
      mtry = floor(seq(mtry[1], mtry[2], length.out=mtry[3]))
      , splitrule = splitrule
      , min.node.size = min.node.size
   )
}


#### ---- GBM -------------
gbm_param <- models_hyperparameters$gbm

if (default_params) {
  gbm_tunegrid <- NULL
} else {
  n.trees <- gbm_param$n.trees
  interaction.depth <- gbm_param$interaction.depth
  shrinkage <- gbm_param$shrinkage
  n.minobsinnode = gbm_param$n.minobsinnode
  gbm_tunegrid <- expand.grid(
    n.trees = n.trees
    , interaction.depth = interaction.depth
    , shrinkage = shrinkage
    , n.minobsinnode = n.minobsinnode
  )
}

#### ---- Lasso-------------
lasso_param <- models_hyperparameters$lasso

if (default_params) {
  lasso_tunegrid <- NULL
} else {
  alpha <- lasso_param$alpha
  lambda <- lasso_param$lambda
  lasso_tunegrid <- expand.grid(
    alpha = alpha
    , lambda= lambda
  )
}





