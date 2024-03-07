# connect to database
library('RODBC')
conn <- odbcConnect("LocalDSN",uid="sa",pwd="123456")

# package for analytic
#install.packages('arules') 
#install.packages('arulesViz')
library('arules')
library('arulesViz')

### import data table
order_detail <- sqlQuery(conn, "SELECT * FROM Order_Detail_Clean WHERE Sale_type = 'order'")

### Convert data frame to transaction matrix
transaction_list <- list()

# Loop to create a list of unique transactions throughout the dataset
for (i in unique(order_detail$Order_ID)) {
  tlist <- unique(order_detail[order_detail$Order_ID == i, "Product_type", drop = FALSE]$Product_type)
  if (length(tlist) > 0) {
    transaction_list[[length(transaction_list) + 1]] <- as.character(tlist)
  }
}

# Convert the list of transactions to a transaction matrix
transactions <- as(transaction_list, "transactions")
class(transactions)

### Display the transaction matrix
inspect(transactions)
summary(transactions)

### test with support minium = 0.005

# iteration 1: 1-itemset
itemsets1 <- apriori(transactions, parameter = list(minlen=1, maxlen=1, support=0.005, 
                                                    target="frequent itemsets"))
summary(itemsets1) 

inspect(head(sort(itemsets1,by="support"),10))

# iteration 2: 2-itemset
itemsets2 <- apriori(transactions, parameter = list(minlen=2, maxlen=2, support=0.005, 
                                                    target="frequent itemsets"))
summary(itemsets2)
inspect(sort(itemsets2,by="support"))

# iteration 3: 3-itemset
itemsets3 <- apriori(transactions, parameter = list(minlen=3, maxlen=3, support=0.005, 
                                                    target="frequent itemsets"))
summary(itemsets3)
inspect(sort(itemsets3,by="support"))

### test with support minium = 0.002
itemsets1 <- apriori(transactions, parameter = list(minlen=1, maxlen=1, support=0.002, 
                                                    target="frequent itemsets"))
summary(itemsets1) 

inspect(head(sort(itemsets1,by="support"),10))

# iteration 2: 2-itemset
itemsets2 <- apriori(transactions, parameter = list(minlen=2, maxlen=2, support=0.002, 
                                                    target="frequent itemsets"))
summary(itemsets2)
inspect(head(sort(itemsets2,by="support"),10))

# iteration 3: 3-itemset
itemsets3 <- apriori(transactions, parameter = list(minlen=3, maxlen=3, support=0.002, 
                                                    target="frequent itemsets"))
summary(itemsets3)
inspect(head(sort(itemsets3,by="support"),10))

### built rules let confidence = 0.1, and support = 0.002
rules <- apriori(transactions, parameter = list(support=0.002, confidence=0.2,
                                                target="rules"))
summary(rules)
plot(rules)
#lift
inspect(head(sort(rules,by="lift"),10))

#rules with 2-itemsets
rules2 <- apriori(transactions, parameter = list(support=0.002, confidence=0.2,minlen=2, maxlen=2,
                                                target="rules"))
inspect(head(sort(rules2,by="lift"),10))

#rules with 3-itemsets
rules3 <- apriori(transactions, parameter = list(support=0.002, confidence=0.2,minlen=3, maxlen=3,
                                                 target="rules"))
inspect(head(sort(rules3,by="lift"),10))

### visualization
plot(rules, method = "grouped")
plot(rules@quality)

####
confidenceRules <- rules[quality(rules)$confidence>0.2]
plot(confidenceRules, method="matrix", measure = c("lift","confidence"))

highLiftRules <- head(sort(rules, by="lift"),5)
plot(highLiftRules, method="graph", control = list(type="items"))
