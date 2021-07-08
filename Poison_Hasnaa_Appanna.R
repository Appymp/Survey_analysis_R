library ("readxl")
poison<- read_excel("poison.xlsx")
poison <- as.data.frame(unclass(poison)) #read categorical columns as factors

######## Univariate analyses #############
summary(poison)


#####Quantitative variables
# Compute the variance for all quantitative variables
sapply(poison[,2:3], var)

# Calculate the mean of age in dataset by Sex
aggregate(poison$Age, by=list(poison$Sex), mean)

# Boxplot of age of sick people
#boxplot(temp$Février, ylab="Température de février (en °C)", col="pink")

# Adding a symbol at the position of the mean
#points(mean(temp$Février),pch=4)


#####Qualitative variables
# Number of observations and frequencies for all variables
for (names in colnames(poison[,4:ncol(poison)])) {
  print(names)
  t <- table(poison[,names])
  print(t)
  print(round(prop.table(t)*100,2))
}

# Bar chart 
par (mfrow=c(2,2))
for (names in colnames(poison[,4:ncol(poison)])) {
  plot(poison[,names], col="lightgreen", ylab="Effectif", main=paste(names))
}

# Pie chart
for (names in colnames(poison[,4:ncol(poison)])) {
  pie(table(poison[,names]), col=c("chocolate","chocolate4","chocolate1","chocolate2","chocolate3"), main=paste(names))
}
par(mfrow = c(1,1))


######## Bivariate analyses #############
#####Between two quantitative variables

# Scatter plot of Age and Time
##Time values which have magnitude only if person is sick. So maybe a function of duration of sickness.
plot(poison$Age,poison$Time, pch=20, col="blue", xlab="Age", ylab="Time")

# Correlation coefficient of Age and Time
cor(poison$Age,poison$Time)


#####Between two qualitative variables
# Contingency table
## Sickness vs symptoms
for (name in colnames(poison[,6:10])) {
  t<-table(poison$Sick,poison[,name])
  print(t)
}

## Sickness vs gender
t<-table(poison$Sick,poison$Sex)
print (t)

## Let us check what food corresponds to being sick
## Each person eats many foods so this level of analysis does not give us the full picture
food_cols<-c('Potato','Fish','Mayo','Courgette','Cheese','Icecream')
for (name in food_cols) {
  t<-table(poison$Sick,poison[,name])
  print(t)
}

# In percentage of total number of observations
for (name in food_cols) {
  t<-table(poison$Sick,poison[,name])
  print(round(prop.table(t)*100,2))
}

# # In percentage of line total
for (name in food_cols) {
  t<-table(poison$Sick,poison[,name])
  print(round(prop.table(t,1),2))
}

# # In percentage of column total
for (name in food_cols) {
  t<-table(poison$Sick,poison[,name])
  print(round(prop.table(t,2),2))
}


#################### Multivariate analyzes ################
# Libraries needed
library(FactoMineR)
library(explor)

# Launch MCA
poison_drop_Id<-poison[,2:ncol(poison)] #Drop ID column since it is not being used
poison.mca <- MCA(poison_drop_Id, quali.sup=3:4, quanti.sup=1:2, ncp=5, graph=T) #specify the additional variables

# To know all the types of results that can be obtained
poison.mca

# Eigenvalue graph
barplot(poison.mca$eig[,2], names.arg=1:nrow(poison.mca$eig))

# Graphic with all elements (illegible)
plot(poison.mca)
# Graphic with individuals and additional variable
plot.MCA(poison.mca,invisible=c("var"))
# Graphic with only individuals
plot.MCA(poison.mca,invisible=c("var","quali.sup"))
# Graphic with modalities colored by variable
plot(poison.mca,habillage="quali", invisible="ind")
# Graphic of the 10 individuals with the greatest contribution on axes 1 and 2
plot(poison.mca,habillage="quali", invisible="var", select="contrib 10")  
# Graphic of the 10 modalities with the greatest contribution on axes 1 and 2
plot(poison.mca,habillage="quali", invisible="ind", selectMod="contrib 10")
# Graphic of the 10 modalities + 10 individuals with the greatest contribution on axes 1 and 2
plot(poison.mca,habillage="quali", select="contrib 10", selectMod="contrib 10") 

"""
#Order of plots in notepad explanation:
#From the first command
##bar chart 
##Correlation between variables and principal dimensions
plot(poison.mca)
#Coordinates of variable categories
plot(poison.mca, invisible=c("ind","quali.sup"))
# Quality representation of variable categories (cos2)
round(poison.mca$var$cos2,2)  # 100% per modality
#Variable categories with colors graded by cos2 (Color for a variable)
plot(poison.mca,habillage="cos2", invisible=c("ind","quali.sup"))
#Contribution of variable categories to the dimensions
round(poison.mca$var$contrib,2)
sort(poison.mca$var$contrib[,1],decreasing=TRUE) #contribution on dim1
sort(poison.mca$var$contrib[,2],decreasing=TRUE) #contribution on dim2
#Variable categories with colors graded by contrib (Color for a category)
plot(poison.mca,habillage="contrib", invisible=c("ind","quali.sup"))
"""

# One graph per variable, to observe the positions of each modality on the axes
plotellipses(poison.mca)

# Contribution on axes
round(poison.mca$var$contrib,2)
sort(poison.mca$var$contrib[,1],decreasing=TRUE)  # 100% par axe

# Representation of modalities (cos2)
round(poison.mca$var$cos2,2)  # 100% per modality

# Results for individuals
poison.mca$ind$coord
poison.mca$ind$contrib
poison.mca$ind$cos2

# Choice of the number of axes to keep for classification
# Kaiser rule
poison.mca$eig[,1] > sum(poison.mca$eig[,1])/nrow(poison.mca$eig)



################ Clustering (CAH)
# Relaunch the MCA with the number of axes (ncp) that we want to keep for the hierarchical clustering
poison.mca <- MCA(poison_drop_Id, quali.sup=3:4, quanti.sup=1:2, ncp=5, graph=F)
poison.hcpc <- HCPC(poison.mca,method="ward",nb.clust=3)
# nb.clust=3

# Characterization of classes by qualitative variables
#poison.hcpc$desc.var$category$'1'
poison.hcpc$desc.var$category


# Formatting the characterization of class 1
Classe1 <- poison.hcpc$desc.var$category$'1'
rownames(Classe1)
tt <- rownames(Classe1)
strsplit(tt, split="=")
tt2 <- do.call("rbind",strsplit(tt, split="="))[,2]
tt2 <- rownames(Classe1)
Classe1

Classe2 <- poison.hcpc$desc.var$category$'2'
rownames(Classe2)
tt <- rownames(Classe2)
strsplit(tt, split="=")
tt2 <- do.call("rbind",strsplit(tt, split="="))[,2]
tt2 <- rownames(Classe2)
Classe2

Classe3 <- poison.hcpc$desc.var$category$'3'
rownames(Classe3)
tt <- rownames(Classe3)
strsplit(tt, split="=")
tt2 <- do.call("rbind",strsplit(tt, split="="))[,2]
tt2 <- rownames(Classe3) 
Classe3

# Number of individuals by class
table(poison.hcpc$data.clust[,"clust"])


