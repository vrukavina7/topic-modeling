rm(list=ls())
gc()

# Load gloabal data
models_results <- readRDS(file = 'modeling-output/models_results.RData')
vocabulary <- readRDS(file = 'modeling-output/vocabulary.RData')
lda_models <- readRDS(file = 'modeling-output/lda_models.RData')
json_viss <- readRDS(file = 'modeling-output/json_viss.RData')
optimalk <- readRDS(file = 'modeling-output/optimalk.RData')
hyperparams <- readRDS(file = 'modeling-output/hyperparams.RData')
optimal_hyperparams <- readRDS(
  file = 'modeling-output/optimal_hyperparams.RData'
)
