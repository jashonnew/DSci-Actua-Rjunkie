# devtools::install_github("Prometheus77/ucimlr")
# devtools::install_github("Prometheus77/actools")


## CREDIT TO AARON COOLEY, THIS AN ADAPTATION OF HIS ML TUTORIAL. SOME WAS ADDED AND REMOVED

pacman::p_load(mlr, mlrMBO, tidyverse, here, glmnet, caret)
here <- here::here()
here

######################################################################################################################################################################################################################
#PREPROCESSING - ONE HOT ENCODE and NORMALIZE FOR ALL PREDICTORS
######################################################################################################################################################################################################################
dat <- read_csv(here("derived_data/dat-targets.csv")) %>%
  group_by(Ukey) %>% 
  arrange(Ukey) %>% 
  top_n(1) %>% 
  ungroup() %>% 
  filter(MemberTypeExtra != "open loan - dl only") %>% 
  select(-Ukey,-City,-State,-Zip,-MemberDate,-LoanNumber,-OpenDate,-VehlMake,-VehlModel,-Dealer,-mei, -BuyRate, -Branch) %>% 
  mutate(MHOT = case_when(
    Gender == "M" ~ 1,
    TRUE ~ 0
  ),
  FHOT = case_when(
    Gender == "F" ~ 1,
    TRUE ~ 0  
  ),
  MarriHOT = case_when(
    JointMarried == "Y" ~ 1,
    TRUE ~ 0
  ),
  Targets = case_when(
    MemberTypeExtra == "converted member" ~ 1,
    MemberTypeExtra == "closed loan - never converted" ~ 0,
    T ~ 2
  )) %>% 
  select(-Gender,-JointMarried, -MemberTypeExtra) %>% 
  filter(!is.na(PMT), Targets != 2) #, Targets = parse_factor(Targets))

dattarg <- cbind("MarriHOT" = dat$MarriHOT,"FHOT" = dat$FHOT,"MHOT" = dat$MHOT,"Targets" = dat$Targets)
datU <- select(dat, Ukey)

dato <- mlr::normalizeFeatures(dat) %>% 
  #bind_cols(datU) %>% 
  select(-Targets,-MHOT,-FHOT,-MarriHOT) %>% 
  cbind(dattarg) %>% 
  mutate(Targets = as.factor(Targets),
         MarriHOT = as.factor(MarriHOT),
         MHOT = as.factor(MHOT),
         FHOT = as.factor(FHOT))

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# TASK CREATION - This defines the target and the data for the learners
#########################################################################################################
#########################################################################################################
#########################################################################################################

task <- makeClassifTask(data = dato, target = "Targets")
print(task)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Metric Creation - We will use Accuracy, Area under the curve, F1 score, and Logloss
#########################################################################################################
#########################################################################################################
#########################################################################################################
my_measures <- list(auc, logloss, f1, acc)

print(my_measures)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Learner List Creation - Logistic Regression, Lasso Regression, Ridge, Elastic Net, Decision Tree, Random Forest, 
# XG boost, Support Vector Machine, We will do probability predictions
#########################################################################################################
#########################################################################################################
#########################################################################################################

logistic <- makeLearner("classif.logreg") %>%
  makeDummyFeaturesWrapper()
lasso <- makeLearner("classif.LiblineaRL1LogReg") %>%
  makeDummyFeaturesWrapper()
ridge <- makeLearner("classif.LiblineaRL2LogReg") %>%
  makeDummyFeaturesWrapper()
elasticnet <- makeLearner("classif.glmnet") %>%
  makeDummyFeaturesWrapper()
decision_tree <- makeLearner("classif.rpart")
random_forest <- makeLearner("classif.randomForest")
xgb <- makeLearner("classif.xgboost") %>%
  makeDummyFeaturesWrapper()
svm <- makeLearner("classif.svm") %>%
  makeDummyFeaturesWrapper()
#deep_nn <- makeLearner("classif.saeDNN") %>%
#makeDummyFeaturesWrapper()

lrns <- list(logistic = logistic, 
             lasso = lasso, 
             ridge = ridge, 
             elasticnet = elasticnet, 
             decision_tree = decision_tree, 
             random_forest = random_forest, 
             xgb = xgb, 
             svm = svm)
lrns <- map(lrns, ~ setPredictType(.x, "prob"))
lrns <- map(lrns, ~ makePreprocWrapperCaret(.x, method = c("center", "scale")))
lrns <- map2(.x = lrns,
             .y = names(lrns),
             ~ setLearnerId(.x, .y))

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Baseline Results Creation - K-fold cross validation for each model, then we boxplot the performances to compare them
#########################################################################################################
#########################################################################################################
#########################################################################################################

baseline_result = benchmark(lrns, task, measures = my_measures) 

map(my_measures, ~ plotBMRBoxplots(baseline_result, measure = .x, pretty.names = FALSE))
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Lasso Tuning - Grid tuning, we then cross-validate it and plot it against the originals
#########################################################################################################
#########################################################################################################
#########################################################################################################

getParamSet("classif.LiblineaRL1LogReg")
params.lasso <- makeParamSet(
  makeDiscreteParam("cost", c(0.1, 1, 10, 100, 1000)),
  makeDiscreteParam("epsilon", c(0.001,0.005, 0.01,0.05, 0.1))
)
tr.lasso <- tuneParams(learner = lrns$lasso, 
                       task = task, 
                       resampling = cv5, 
                       measures = my_measures, 
                       par.set = params.lasso,
                       control = makeTuneControlGrid())
print(tr.lasso)
tr.lasso$opt.path$env$path %>%
  ggplot(aes(x = cost, y = epsilon)) +
  geom_raster(aes(fill = auc.test.mean)) +
  geom_text(aes(label = signif(auc.test.mean, 3)))
lrns$lasso.tuned <- setHyperPars(lrns$lasso, par.vals = tr.lasso$x) %>%
  setLearnerId("lasso.tuned")

bmr <- benchmark(list(lrns$lasso.tuned), task, measures = my_measures, resampling = cv10)

all_results <- mergeBenchmarkResults(list(baseline_result, bmr))

plotBMRBoxplots(all_results, pretty.names = FALSE)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# XGBoost  Random Tuning(Grid Tuning) we the cross-validate and plot against the previous models
#########################################################################################################
#########################################################################################################
#########################################################################################################

getParamSet("classif.xgboost")
params.xgboost <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 1),
  makeIntegerParam("max_depth", lower = 1, upper = 20),
  makeNumericParam("min_child_weight", lower = 0, upper = 100),
  makeNumericParam("subsample", lower = 0.1, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.1, upper = 1),
  makeIntegerParam("nrounds", lower = 10, upper = 1000)
)

tr_xgb_rand <- tuneParams(learner = lrns$xgb,
                          task = task, 
                          resampling = cv3,
                          measures = my_measures, 
                          par.set = params.xgboost,
                          control = makeTuneControlRandom())


lrns$xgb.tuned.rand <- setHyperPars(lrns$xgb, par.vals = tr_xgb_rand$x) %>%
  setLearnerId("xgb tuned random")
bmr <- benchmark(list(lrns$xgb.tuned.rand), task, measures = my_measures, resampling = cv10)

all_results <- mergeBenchmarkResults(list(all_results, bmr))

plotBMRBoxplots(all_results, pretty.names = FALSE)

plot_pd <- function(var, tune_result) {
  tune_result$opt.path$env$path %>%
    gather(metric, value, auc.test.mean, acc.test.mean, f1.test.mean, logloss.test.mean) %>%
    ggplot(aes_string(x = var, y = "value")) +
    geom_point() +
    geom_smooth() +
    geom_vline(xintercept = tune_result$x[[var]]) +
    facet_wrap(~ metric, scales = "free")
}

map(names(params.xgboost$pars), ~ plot_pd(.x, tr_xgb_rand))

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# XGBoost  MBO Tuning - MBO uses a targeted tuning which hunts for a better AUC, then added to boxplot
#########################################################################################################
#########################################################################################################
#########################################################################################################


mboc <- makeMBOControl(final.method = "best.predicted")
tr_xgb_mbo <- tuneParams(learner = lrns$xgb, 
                         task = task,
                         resampling = cv3,
                         measures = my_measures,
                         par.set = params.xgboost,
                         control = makeTuneControlMBO(budget = 50, mbo.control = mboc))

lrns$xgb.tuned.mbo <- setHyperPars(lrns$xgb, par.vals = tr_xgb_mbo$x) %>%
  setLearnerId("xgb tuned MBO")

bmr <- benchmark(list(lrns$xgb.tuned.mbo), task, measures = my_measures, resampling = cv10)

all_results <- mergeBenchmarkResults(list(baseline_result, bmr))

plotBMRBoxplots(all_results, pretty.names = FALSE)

map(names(params.xgboost$pars), ~ plot_pd(.x, tr_xgb_mbo))

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Elastic Net MBO tuning - same idea as above
#########################################################################################################
#########################################################################################################
#########################################################################################################

params.elast_net <- makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 1),
  makeNumericParam("s", lower = 0, upper = 1000),
  makeIntegerParam("nlambda", lower = 1, upper = 100),
  makeNumericParam("thresh", lower = 0, upper = 1000),
  makeIntegerParam("dfmax", lower = 0, upper = 1000),
  makeIntegerParam("pmax", lower = 0, upper = 1000),
  makeIntegerParam("maxit", lower = 1, upper = 1000)
)

tr_ENET_mbo <- tuneParams(learner = lrns$elasticnet, 
                          task = task,
                          resampling = cv3,
                          measures = my_measures,
                          par.set = params.elast_net,
                          control = makeTuneControlMBO(budget = 50, mbo.control = mboc))

lrns$Enet.tuned.mbo <- setHyperPars(lrns$elasticnet, par.vals = tr_ENET_mbo$x) %>%
  setLearnerId("Enet tuned MBO")

bmr <- benchmark(list(lrns$Enet.tuned.mbo), task, measures = my_measures, resampling = cv10)

all_results <- mergeBenchmarkResults(list(all_results, bmr))

plotBMRBoxplots(all_results, pretty.names = FALSE)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# SVM MBO tuning - same idea as above
#########################################################################################################
#########################################################################################################
#########################################################################################################

params.svm <- makeParamSet(
  makeNumericParam("nu", lower = -100, upper = 100),
  makeNumericParam("gamma", lower = 0, upper = 100),
  makeIntegerParam("degree", lower = 1, upper = 100),
  makeNumericParam("cost", lower = 0, upper = 100)
)

tr_SVM_mbo <- tuneParams(learner = lrns$svm, 
                         task = task,
                         resampling = cv3,
                         measures = my_measures,
                         par.set = params.svm,
                         control = makeTuneControlMBO(budget = 50, mbo.control = mboc))

lrns$SVM.tuned.mbo <- setHyperPars(lrns$svm, par.vals = tr_SVM_mbo$x) %>%
  setLearnerId("SVM tuned MBO")

bmr <- benchmark(list(lrns$Enet.tuned.mbo), task, measures = my_measures, resampling = cv10)

all_results <- mergeBenchmarkResults(list(all_results, bmr))

plotBMRBoxplots(all_results, pretty.names = FALSE)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Rand Forest MBO tuning - Same as above
#########################################################################################################
#########################################################################################################
#########################################################################################################

params.rando <- makeParamSet(
  makeIntegerParam("ntree", lower = 1, upper = 1000),
  makeIntegerParam("mtry", lower = 1, upper = 100),
  makeIntegerParam("nodesize", lower = 1, upper = 100),
  makeIntegerParam("maxnodes", lower = 1, upper = 100)
)

tr_rando_mbo <- tuneParams(learner = lrns$random_forest, 
                           task = task,
                           resampling = cv3,
                           measures = my_measures,
                           par.set = params.rando,
                           control = makeTuneControlMBO(budget = 50, mbo.control = mboc))

lrns$random_for.tuned.mbo <- setHyperPars(lrns$random_forest, par.vals = tr_rando_mbo$x) %>%
  setLearnerId("R_forest tuned MBO")

bmr <- benchmark(list(lrns$random_for.tuned.mbo), task, measures = my_measures, resampling = cv10)

all_results <- mergeBenchmarkResults(list(all_results, bmr))

plotBMRBoxplots(all_results, pretty.names = FALSE)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Ensembles, all base and tuned learners are ensembled. stack log_reg performs the best, we plot the boxplots
# once more, train the final stack on all of the training data and then use it to get predictions on our test data.
#########################################################################################################
#########################################################################################################
#########################################################################################################

ensemble1 <- makeStackedLearner(base.learners = lrns,
                                predict.type = "prob",
                                method = "hill.climb")

ensemble2 <- makeStackedLearner(base.learners = lrns,
                                predict.type = "prob",
                                method = "average") %>%
  setLearnerId("stack_average")
ensemble3 <- makeStackedLearner(base.learners = lrns,
                                super.learner = "classif.logreg",
                                predict.type = "prob",
                                method = "stack.nocv") %>%
  setLearnerId("stack_logreg.nocv")
ensemble4 <- makeStackedLearner(base.learners = lrns,
                                super.learner = "classif.logreg",
                                predict.type = "prob",
                                method = "stack.cv",
                                resampling = cv5) %>%
  setLearnerId("stack_logreg.cv5")
ensemble5 <- makeStackedLearner(base.learners = lrns,
                                super.learner = "classif.LiblineaRL1LogReg",
                                predict.type = "prob",
                                method = "stack.nocv") %>%
  setLearnerId("stack_lasso.nocv")
ensemble6 <- makeStackedLearner(base.learners = lrns,
                                super.learner = "classif.LiblineaRL1LogReg",
                                predict.type = "prob",
                                method = "stack.cv",
                                resampling = cv5) %>%
  setLearnerId("stack_lasso.cv5")
stack_results <- benchmark(list(ensemble1, ensemble2, ensemble3, ensemble4, ensemble5, ensemble6),
                           task, cv10, list(auc, acc, f1, logloss))


final_result <- mergeBenchmarkResults(list(all_results, stack_results))
print(final_result)
plotBMRBoxplots(final_result, pretty.names = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tvp <- generateThreshVsPerfData(final_result, list(fpr, tpr))
plotROCCurves(tvp, list(fpr, tpr), facet.learner = TRUE)


final_model <- mlr::train(ensemble2, task)

newdata.pred <-  predict(final_model, newdata = dat_predict) %>% as_tibble() %>% cbind(dat_predict.con) 

write_csv(newdata.pred,here("derived_data/preds_final.csv"))
write_rds(final_model,here("derived_data/final_model.rds"))
