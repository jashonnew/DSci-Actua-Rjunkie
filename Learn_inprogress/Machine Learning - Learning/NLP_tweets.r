pacman::p_load(tidyverse,tidytext, ngram, textdata, sentimentr, GGally, mlr, mlrMBO)

train <- read_csv("nlp-getting-started/train.csv") %>%
  mutate(char_count = nchar(text),
         word_count = sapply(strsplit(text, " "), length),
         hashtag_count = str_count(text, "#"),
         number_count = str_count(text, "[0-9]"),
         capital_count = str_count(text, "[A-Z]"),
         contains_link = as.character(case_when(str_count(text, "http") >= 1 ~ 1,
                                   TRUE                         ~ 0)),
         breaking = as.character(str_count(text, "BREAKING")))

b <- sentiment(train$text)
d <- b %>% 
  group_by(element_id) %>% 
  summarize(sent_score = mean(sentiment))

targs <- train$target
train <- train %>% 
  cbind(sent_score = d$sent_score) %>% 
  select(-id,-keyword,-location,-text,-target) %>% 
  normalizeFeatures() %>% 
  cbind(targs) %>% 
  mutate(contains_link = as.numeric(contains_link), targs = as.factor(targs), breaking = as.numeric(breaking))

task <- makeClassifTask(data = train, target = "targs")
print(task)

#train %>% 
  #select(-text, -keyword, -location) %>% 
  #ggpairs(aes(col = as.character(target)))

my_measures <- list(auc, logloss, f1, acc)
print(my_measures)

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

baseline_result = benchmark(lrns, task, measures = my_measures) 
map(my_measures, ~ plotBMRBoxplots(baseline_result, measure = .x, pretty.names = FALSE))


params.xgboost <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 1),
  makeIntegerParam("max_depth", lower = 1, upper = 20),
  makeNumericParam("min_child_weight", lower = 0, upper = 100),
  makeNumericParam("subsample", lower = 0.1, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.1, upper = 1),
  makeIntegerParam("nrounds", lower = 10, upper = 1000)
)


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