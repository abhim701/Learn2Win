# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules
options(digits=2)
inspect(rules[1:25])
#rules<-sort(rules, by="confidence", decreasing=TRUE)
#rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=2))


# Redundancies
# Sometimes, rules will repeat. Redundancy indicates that one item might be a given. As an analyst you can elect to drop the item from the dataset. Alternatively, you can remove redundant rules generated.
# 
# We can eliminate these repeated rules using the follow snippet of code:
#   
# subset.matrix <- is.subset(rules, rules)
# subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# redundant <- colSums(subset.matrix, na.rm=T) >= 1
# rules.pruned <- rules[!redundant]
# rules<-rules.pruned

# Now that we know how to generate rules, limit the output, lets say we wanted to target items to generate rules. There are two types of targets we might be interested in that are illustrated with an example of "whole milk":
#   
#   What are customers likely to buy before buying whole milk



rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# What are customers likely to buy if they purchase whole milk?
# Likewise, we can set the left hand side to be "whole milk" and find its antecedents.
# Note the following:
#   
#   We set the confidence to 0.15 since we get no rules with 0.8
# We set a minimum length of 2 to avoid empty left hand side items

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)

