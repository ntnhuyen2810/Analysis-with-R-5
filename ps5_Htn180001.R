library(data.table)
library(plm)
install.packages("partykit")
library(partykit)

n <- 500        #create 500 records for each group      
set.seed(75080) #set seed = 75080

z   <- rnorm(n) #create 500 random values for z
w   <- rnorm(n) #create 500 random values for w
x   <- 5*z + 50 #create 500 values for income (x)
y   <- -100*z+ 1100 + 50*w #create 500 values for SAT (y)
y   <- 10*round(y/10) #round up value of Y
y   <- ifelse(y<200,200,y) #set value of Y if less than 200 to be 200 - the min of SAT, 
y   <- ifelse(y>1600,1600,y) #set value of Y if > 1600 to be 1600 - the max of SAT,
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n)) #create data  for group 1

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n)) #create data  for group 2

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n)) #create data  for group 3

#merge 3 data sets together
dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)

#Q2
dtable$group <- as.factor(dtable$group)
dtable$iid <- 1:1500
pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+group-1,data=dtable)
model1 <- lm(sat~income,data=dtable[group==1])
model2 <- lm(sat~income,data=dtable[group==2])
model3 <- lm(sat~income,data=dtable[group==3])
summary(pooled)
summary(within)
summary(model1)
summary(model2)
summary(model3)

#Q3
tree <- ctree(sat~income,data=dtable)
plot(tree)
tree1 <- ctree(sat~group,data=dtable)
plot(tree1)
tree2 <- ctree(sat~income+group,data=dtable)
plot(tree2)

#Q4
glmtree(sat~income+group,data=dtable)

#Q5
## Optimal number of clusters
install.packages("factoextra")
library(factoextra)   
fviz_nbclust(dtable[,2:3], kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

## Correct means
kmodel1 <- kmeans(dtable[,2:3],centers=3,nstart=10)
kmodel1$centers

#Q6
model <- hclust(dist(dtable[,2:3]))
plot(model)
rect.hclust(model,k=7,border="red")
rect.hclust(model,k=6,border="purple")
rect.hclust(model,k=5,border="blue")
rect.hclust(model,k=4,border="green")
rect.hclust(model,k=3,border="yellow")
rect.hclust(model,k=2,border="orange")
summary(cutree(model,k=3))

wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}

wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss))
  wss
}

res <- sapply(seq.int(1, 20), wrap, h = model, x = dtable[,2:3])
plot(seq_along(res[1:20]), res[1:20], type = "o", pch = 19, xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#best number of cluster is 4

#Q7
newdtable <- cbind(dtable, clusterNum = kmodel1$cluster)
newdtable$hier <-cutree(hclust(dist(dtable[,2:3])),k=4)
newdtable$clusterNum <- as.factor(newdtable$clusterNum)
newdtable$hier <- as.factor(newdtable$hier)
pooled1 <- lm(sat~income,data=newdtable)
within1 <- lm(sat~income+clusterNum-1,data=newdtable)
within2 <- lm(sat~income+hier-1,data=newdtable)

summary(pooled1)
summary(within1)
summary(within2)

#Q8
kmodel2 <- kmeans(newdtable[,3:3],centers=3,nstart=10)
fviz_nbclust(newdtable[,3:3], kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#best number of cluster is 3
newdtable <- cbind(newdtable, clusterNumIncome = kmodel2$cluster)
newdtable$clusterNumIncome <- as.factor(newdtable$clusterNumIncome)
pooled3 <- lm(sat~income,data=newdtable)
within3 <- lm(sat~income+clusterNumIncome-1,data=newdtable)
summary(pooled3)
summary(within3)

#Q9
kmodel3 <- kmeans(scale(newdtable[,2:3]),centers=3,nstart=10)
fviz_nbclust(scale(newdtable[,2:3]), kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#best number of cluster is 3
newdtable <- cbind(newdtable, clusterNumScale = kmodel3$cluster)
newdtable$clusterNumScale <- as.factor(newdtable$clusterNumScale)
pooled4 <- lm(sat~income,data=newdtable)
within4 <- lm(sat~income+clusterNumScale-1,data=newdtable)
summary(pooled4)
summary(within4)
