install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

moody = read.csv("/Users/shazeghi/Downloads/moody2022_new.csv")

#rpart(formula,method,data,control,subset,weights)
tree1 = rpart(GRADE~SCORE,moody)
tree1
rpart.plot(tree1)
printcp(tree1)

probA = nrow(moody[moody$SCORE >= 80,])/nrow(moody)
probA

tree2 = rpart(GRADE~SCORE + TEXTING_IN_CLASS,moody)
rpart.plot(tree2)
#more predictors usually means more branches, higher predictive accuracy

#tree3 uses all columns as predictors for grade
tree3 = rpart(GRADE~.,moody,method = "anova")
rpart.plot(tree3)

Realestate = read.csv("/Users/shazeghi/Downloads/RealEstate.csv")
View(Realestate)
dev.off()
table(Realestate$Bathrooms)
RestateTree = rpart(Price~Bathrooms,data = Realestate,minsplit = 20)
rpart.plot(RestateTree)
