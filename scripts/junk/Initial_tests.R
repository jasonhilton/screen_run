library(tibble)
library(ggplot2)
library(magrittr)

set.seed(1434)


# x in [0,1]^k
k <- 2
N <- 100
x <- seq(0,1, 0.05)

X <- as.matrix(expand.grid(replicate(k, x, simplify = F)))



test_function <- function(X, B){
  cbind(1, X, X[,1] * X[,2], X[,1]*X[,1]*X[,1]*X[,1], 
        sin(X[,2]*4*pi)) %*% as.matrix(B)
}


true_beta <- c(3,-0.5,0.25, 2, -1/10,1)
Y <- test_function(X, true_beta)


get_true_function <- function(true_beta){
  function(X){
    test_function(X, true_beta)
  }
  
}

true_function  <- get_true_function(true_beta)

D <- tibble(X1=X[,1], X2=X[,2], Y=Y[,1])
ggplot(D, aes(x=X1, y=X2, fill=Y)) + geom_tile() + scale_fill_viridis_c()


D1<-lhs::maximinLHS(n=400,k=2)
Y<- test_function(D, c(3,-0.5,0.25, 2, -1/10,1))

library(DiceKriging)
mod1 <- km(design=tibble(Var1=D[,1],Var2=D[,2]),response=Y)


preds1 <- predict(mod1, X,type="SK")

preds1_df <- as_tibble(cbind(X, Y=preds1$mean))
ggplot(preds1_df, aes(x=Var1,y=Y)) + geom_line() + facet_wrap(~Var2) +
  geom_point(aes(y=true_function(X)))


ggplot(preds1_df, aes(x=Var2,y=Y)) + geom_line() + facet_wrap(~Var1) +
  geom_point(aes(y=true_function(X)))

D2<-lhs::maximinLHS(n=400,k=10)
D3<-lhs::maximinLHS(n=400,k=10,method = "build", dup=4)
#D4<-lhs::maximinLHS(n=400,k=10,method = "iterative")  #wow sup-er slow
#D5<-lhs::maximinLHS(n=400,k=10,method = "iterative", eps=0.005)
D4 <-NA
D5 <- NA
D6<-lhs::improvedLHS(n=400,k=10)
D7<-lhs::improvedLHS(n=400,k=10, dup=4)
D8<-lhs::geneticLHS(n=400,k=10)

tibble(methods =c("Lower D",
                  "Maximin build",
                  "Maximin build, dup=4",
                  "Maximin iterative",
                  "Maximin iterative eps=0.005",
                  "Improved",
                  "Improved dup=4",
                  "Genetic"),
       "Min Dist"=c(min(dist(D1[,1:2])),
                    min(dist(D2[,1:2])),
                    min(dist(D3[,1:2])),
                    NA,
                    NA,
                    min(dist(D6[,1:2])),
                    min(dist(D7[,1:2])),
                    min(dist(D8[,1:2]))))

plot(D1[,1], D1[,2])
points(D2[,1], D2[,2], pch=15, col="red")


Y<- test_function(D, c(3,-0.5,0.25, 2, -1/10,1))


install.packages("sensitivity")
library(sensitivity)
x <- morris(model = morris.fun, factors = 20, r = 4,
            design = list(type = "oat", levels = 5, grid.jump = 3))
print(x)
plot(x)





true_function <- get_true_function(true_beta)
Y <- true_function(D3)

# levels is the number - it should be even  
x <- morris(model = true_function, factors = 10, r = 10,
            design = list(type = "oat", levels = 10, grid.jump = 2))
print(x)
plot(x)
plot(x$X[,1], x$X[,2])




library(DiceKriging)
library(dplyr)
D3_df <- as_tibble(D3)
mod2 <- km(design=D3_df[,1:2],response=Y,nugget = 0.001)

mod3 <- km(design=D3_df,response=Y,nugget = 0.001)


pred2<-predict(mod2, newdata=as_tibble(X), type="SK", checkNames=F)
pred3<-predict(mod3, 
               newdata=as_tibble(cbind(X,matrix(0.5, 441, 8))),
               checkNames=F,
               type="SK")


tibble(Vpred3$mean

