pacman::p_load(arules)

data("Groceries")
rules <- apriori(Groceries, parameter = list(supp = 0.0005, conf = 0.5))
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15))
inspect(frequentItems)
itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency")

rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
rules_supp <- sort(rules, by = "support", decreasing = TRUE)
rules_conf <- sort(rules, by = c("confidence","support"), decreasing = TRUE)

inspect(head(rules_lift, 5))
inspect(head(rules_supp, 5))
inspect(head(rules_conf, 5))

