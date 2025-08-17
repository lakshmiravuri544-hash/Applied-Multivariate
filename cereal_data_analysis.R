#Reading the file
cereals.data <- read.table("cereals1-1.txt", header = T)
head(cereals.data)
summary(cereals.data)

#Standardize the data
Z <- scale(cereals.data[,-c(1,2)], scale = T)
cor(Z)

#Importing Libraries
library(ggplot2)
library(psych)

#(a) Are there notable associations/relationships between some of the variables?
#Correlation plot
library(corrplot)
corrplot(cor(cereals.data[,-c(1,2)]), method  = "color")

# creating the scatter plot and correlation matrix
pairs.panels(cereals.data[,-c(1,2)],
             method = "pearson", #correlation method
             hist.col = "red",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
) 

#From the correlation matrix and correlation plot, there exists a strong positive linear relationship between potassium and fiber.
#From the matrix medium positive linear relationship between sugar and calories, potassium and protein, sodium and carbohydrates, fiber and protein.
#And there exists a negative association between sugar and protein, fat and carbohydrates, sugar and carbohydrates.

#(b) Is there a way to graphically represent the raw data for the 43 cereals and draw conclusions about the data set from such a graph?
#Draw a star plot and set the argument draw.segments to TRUE to get segments instead of stars.
stars(cereals.data[,-c(1,2)], draw.segments=T, key.loc = c(15.5,1.5), labels = cereals.data[,1], main = "Star Plot of cereals data")

#From the star plot, it is noticed as the cereals related to the raisin products and cheerios only having the all the variables.
#From the plot it is also observed that puffed rice has only carbohydrates, whereas puffed wheat has only carbohydrates, potassium, protein and sodium.

#(c)What are the basic underlying groups that the cereals form? Can you plot the data in a small number of dimensions, showing the group separation of the cereals?
#Performing the Clustering techniques to group the cereals
#Calculating pairwise Euclidean distances between the (standardized) objects:
dist.cereal <- dist(x = Z, method = "euclidean")

# Performing complete linkage clustering:
gob.complete.link <- hclust(dist.cereal, method='complete')

# Plotting the complete linkage dendrogram:
plot(gob.complete.link, ylab="Distance", labels = cereals.data$Brand)
data.frame(obs.cluster = gob.complete.link$merge, gob.complete.link$height)
abline(h = 3.50, lty = "dashed", lwd = 2)
abline(h = 5.25, lty = "dashed", lwd = 2)
abline(h = 6.5, lty = "dashed", lwd = 2)
save.cl<-rect.hclust(gob.complete.link, k = 7, border = "red")
save.cl

#From the complete clustering method, there are total number of 7 cluster groups of cereals are formed.
#Group-1 consists of Allbran. Group-2 has Quaker oat meal. Group-3 consists of Frosted mini wheats, puffed rice and puffed wheat.
#Group-4 contains  cheerios and special K. 
#Group-5 consists of Nutri grain wheat, multi grain cheerios, total whole grain, cheaties, kix, total corn flakes, product19, crispix and corn flakes and rice krispies.
#Group-7 has the all the raisin cereal products including cracklin oat bran, life, honey nut cheerios and fruitful bran, myueslix crispy blend.
#Group-6 has all other cereals remaining.

#Plotting the clusters in the smaller number of dimensions

library(rgl)
PCA.CA.plot<-function(data.set, cluster.results, numb.clust, plot.title, cor.use = TRUE, inches = 0.5) {
  
  clusters<-cutree(tree = cluster.results, k = numb.clust)
  
  #PC scores
  pca.cor<-princomp(x = data.set, cor = cor.use, scores = FALSE)
  pca.cor$scale<-apply(X = data.set, MARGIN = 2, FUN = sd)
  score.cor<-predict(pca.cor, newdata = data.set)
  
  #Scatter plot of first two PCs
  win.graph()
  par(pty = "s")
  common.limits<-c(min(score.cor[,1:2]), max(score.cor[,1:2]))
  plot(x = score.cor[,1], y = score.cor[,2], xlab = "PC #1", ylab = "PC #2",
       main = paste("PCs with", plot.title, "and", numb.clust, "clusters"),
       xlim = common.limits, ylim = common.limits, panel.first = grid(col = "lightgray", lty = "dotted"),
       col = clusters, pch = clusters)
  abline(h = 0)
  abline(v = 0)
  text(x = score.cor[,1], y = score.cor[,2]+0.2)
  
  #Bubble plot of first three PCs
  win.graph()
  par(pty = "s")
  PC3.positive<-score.cor[,3] - min(score.cor[,3])  #Bubble needs to contain all values > 0
  col.symbol<-ifelse(test = score.cor[,3]>0, yes = "red", no = "blue")
  symbols(x = score.cor[,1], y = score.cor[,2], circles = PC3.positive,
          xlab = "PC #1", ylab = "PC #2", main = paste("PCs with", plot.title, "and", numb.clust, "clusters"), inches = inches,
          xlim = common.limits, ylim = common.limits, panel.first = grid(col = "lightgray", lty = "dotted"),
          fg = col.symbol)
  text(x = score.cor[,1], y = score.cor[,2], col = clusters)
  abline(h = 0)
  abline(v = 0)
}

PCA.CA.plot(data.set = cereals.data[,-c(1,2)], cluster.results = gob.complete.link, numb.clust = 7,
            plot.title = "Complete linkage dendogram")

#(d)Can you use the dissimilarities among the cereals to plot the cereals on a 2-dimensional map? If so, how would you characterize those two dimensions?
# Calculate the dissimilarities(distance)
dist.cereal <- dist(Z, method = "euclidean")
d <- as.matrix(dist.cereal)
plot(d)
cereal_types <- cereals.data$Company
cereal_colors <- c("red", "blue", "yellow")
points(d, pch = 19, col = cereal_colors)
text(d, labels = cereals.data$Brand, cex = 0.5, pos = 2)

#From the plot of the distance matrix, it is noticed as the Allbran, Puffed rice, Puffed wheat, special K, Cheerios and Quaker oat meal has the larger distance, which has the huge dissimilarity

#(e) Would you say the three companies are the same in terms of their average values for the eight variables? 
#Are there interesting differences between the three companies’ cereals that can be displayed using the variables (or combinations of variables) in the data set?
#Mean of variables
data <- as.matrix(cereals.data[,-c(1,2)])
company <- cereals.data$Company

# group means
aggregate(data, by = list(company), FUN = mean)


# manova
out <- manova(data ~ company)
summary(out, test = "Wilks")
summary.aov(out)

#From the summary, it seems that p-value for variables Calories, Fat, Sodium and Carbohydrates is too small. 
#This indicates the larger difference of group means between them.

# Difference between "General mills" and "Kellogg's"
summary(manova(data ~ company, data = cereals.data, 
               subset = company %in% c("G", "K")))

# Difference between "General mills" and "Quaker" companies
summary(manova(data ~ company, data = cereals.data, 
               subset = company %in% c("G", "Q")))

# Difference between "Kellogg's" and "Quaker" company
summary(manova(data ~ company, data = cereals.data, 
               subset = company %in% c("K", "Q")))

#From the difference between the cereals companies, it is observed that the p-value is too small for  companies General Mills and Kellogg's
#General Mills and Quaker oats companies which suggests the true mean difference is too large.

#Pairwise Comparison of each variable
library(emmeans)
# number of variables
p <- 8
# Create a list to store the results
pair.lst <- vector("list", p)
# name the list according to variabls (for convenience)
names(pair.lst) <- colnames(data)
# run emmeans for each variable to estimate the group means etc
for(j in 1:p){
  wts <- rep(0, p)
  wts[j] <- 1
  pair.lst[[j]] <- emmeans(out, "company", weights=wts)
}

pair.lst

#Adjust alpha with new alpha
# number of groups
g <- 3
# old significance level
alpha <- 0.05
# number of comparison
nc <- p * g * (g-1) / 2
# new significance level
alphanew <- 0.05 / nc

# obtain the contrasts first for Calories
cont <- contrast(pair.lst$Calories, "pairwise")
# pair-wise differences for Calories
ca <- confint(cont, level=1-alphanew, adj="none")
ca
#plot of pair wise difference for Calories
plot(ca)

# obtain the contrasts first for Protein
cont <- contrast(pair.lst$Protein, "pairwise")
# pair-wise differences for Protein
pr <- confint(cont, level=1-alphanew, adj="none")
pr
#plot of pair wise difference for Protein
plot(pr)

# obtain the contrasts first for Fat
cont <- contrast(pair.lst$Fat, "pairwise")
# pair-wise differences for Fat
fat <- confint(cont, level=1-alphanew, adj="none")
fat
#plot of pair wise difference for Fat
plot(fat)

# obtain the contrasts first for Sodium
cont <- contrast(pair.lst$Sodium, "pairwise")
# pair-wise differences for Sodium
so <- confint(cont, level=1-alphanew, adj="none")
so
#plot of pair wise difference for Sodium
plot(so)

# obtain the contrasts first for Fiber
cont <- contrast(pair.lst$Fiber, "pairwise")
# pair-wise differences for fiber
fi <- confint(cont, level=1-alphanew, adj="none")
fi
#plot of pair wise difference for Fiber
plot(fi)

# obtain the contrasts first for Carbohydrates
cont <- contrast(pair.lst$Carbohydrates, "pairwise")
# pair-wise differences for Carbohydrates
carbs <- confint(cont, level=1-alphanew, adj="none")
carbs
#plot of pair wise difference for Carbohydrates
plot(carbs)

# obtain the contrasts first for Sugar
cont <- contrast(pair.lst$Sugar, "pairwise")
# pair-wise differences for sugar
sugar <- confint(cont, level=1-alphanew, adj="none")
sugar
#plot of pair wise difference for sugar
plot(sugar)

# obtain the contrasts first for Potassium
cont <- contrast(pair.lst$Potassium, "pairwise")
# pair-wise differences for Potassium
pot <- confint(cont, level=1-alphanew, adj="none")
pot
#plot of pair wise difference for Potassium
plot(pot)

#(f) What are any other potentially interesting aspects of the data set?
#Check for univariate outliers
#Univariate outliers
library(univOutl)
apply(Z,2,boxB)

#From the univariate outlier test, it is observed as the no outliers found in the Sugar and Fat.
#where as the other variables detected the outliers.

#Check for univariate normality
apply(Z,2,shapiro.test)

#The cereals data is not univariate normal as we have strong evidence that shapiro test rejects null hypothesis.

#Check for multivariate normality
library(MVN)
# Perform Royston's test and create a chi-square plot 
mvn(Z, mvnTest = "royston", multivariatePlot = "qq")

#The data is not normal.

#Performing Multi Linear regression analysis
mod.fit<-lm(formula = Calories ~ Protein + Fat + Sodium + Fiber + Carbohydrates + Sugar + Potassium, data = cereals.data)
summary(mod.fit)

#From the MLR model, it is noticed as the sodium, fiber and potassium are not significant and doesn't have impact on the Calories.

mod.fit1<-lm(formula = Calories ~ Protein + Fat + Carbohydrates + Sugar, data = cereals.data)
summary(mod.fit1)

#After modifying the above non-significant variables, 

#Performing Linear Discriminant Analysis(LDA)
DA1<-lda(formula = Company ~ Calories + Protein + Fat + Sodium + Fiber + Carbohydrates + Sugar + Potassium, data = cereals.data, CV = T)
DA1

# Accuracy
summarize.class<-function(original, classify) {
  class.table<-table(original, classify)
  numb<-rowSums(class.table)
  prop<-round(class.table/numb,4)
  overall<-round(sum(diag(class.table))/sum(class.table),4)
  list(class.table = class.table, prop = prop, overall.correct = overall)
}

summarize.class(original = cereals.data$Company, classify = DA1$class)

#From the LDA results, it is observed as the accuracy of the model is 0.72(i.e., it predicts 72% of correct categorization of cereals belongs to the group of companies)

#Performing T-Test on the cereals data
# Split the data by companies
gm_data <- subset(cereals.data, Company == "G")
kellogg_data <- subset(cereals.data, Company == "K")
quaker_data <- subset(cereals.data, Company == "Q")

#Compare the true mean difference by Companies
t.test(gm_data[,-c(1,2)], alternative = c("two.sided"), mu = 0,
       conf.level = 0.95)

t.test(kellogg_data[,-c(1,2)], alternative = c("two.sided"), mu = 0,
       conf.level = 0.95)

t.test(quaker_data[,-c(1,2)], alternative = c("two.sided"), mu = 0,
       conf.level = 0.95)

#From the t-test results of individual companies. it is noticed as the p-value is less than 0.05, which states it rejects null hypothesis.
#results in true mean is not equal to zero.


##Factor Analysis
library(psych)
#Determining the Number of factors
fa.parallel(Z, fm = "ml", fa = "fa", n.iter = 100)
scree(Z)

#From the parallel analysis and scree plot suggests the number of factors sufficient are 2.

#Factors using Varimax rotation 
fa.varimax <- fa(r = Z, nfactors = 5, 
                 n.obs = nrow(Z), fm = "ml",
                 rotate = "varimax",
                 scores = "regression")

print(fa.varimax$loadings)

#From the factor analysis, Factor-1 interprets about the linear relationship among the number of calories is affected upon the amount of sugar includes in the cereals.
#Factor-2 suggests about the carbohydrates and sodium, as presence of carbohydrates effects the sodium levels in the cereals.
#Factor-3 represents among the Fat levels of the cereals. Similarly, Factor-4 is highly related among the Potassium and Sodium of the cereals.
#Factor-5 gives the priority about the protein levels of the cereals of the group of companies.