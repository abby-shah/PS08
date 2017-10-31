#SYED ABBAS SHAH

library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)

# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3)
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic
runtime



# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data
train <- read_csv("~/Downloads/train.csv")

# YOOGE!
dim(train)


# knn modeling ------------------------------------------------------------
model_formula <- as.formula(Device ~ X + Y + Z)

# Values to use:
n_values <- c(10, 100, 500, 1000,5000 ,10000, 50000, 100000, 500000)
k_values <- c(2, 3, 4, 5, 6, 7, 8,9,10)


#SETTING UP

#Let's make a function to do most of the work for us.

helpful.fn<-function(data,size, k){
  
n<- slice(data,1:size)
tic()
model_knn <- caret::knn3(model_formula, n , k = k)  # calculates the time it takes to fit the model
timer_info <- toc()
runtime<- timer_info$toc- timer_info$tic
#a<-as.data.frame(size,k,runtime)
return(runtime)
}

#First, let's calculate Run Time values for the N and K vectors given above.

n<- c()
K <- c()
runtime<- c()
i=0
set.seed(20)
for(size in n_values){
  for(k in k_values){
    n[i] <- size
    K[i] <- k
    runtime[i] <- helpful.fn(data=train,size=size,k=k)
    i=i+1
  }
}
runtime_dataframe<-data.frame(n,K,runtime)
plot1 <- ggplot(runtime_dataframe, aes(x=n , y=runtime, col=K, group=K)) +
  geom_point() + geom_line() + theme_minimal() 
plot1 + labs(title = "Run Time over Sample Size (N) and Number of Neighbours (K)", x= "N", y= "Run Time")
ggplot(runtime_dataframe, aes(x=K , y=runtime, col=n, group=n)) +
  geom_point() + geom_line() + theme_minimal() 

#More values

#I will now iterate over a larger domain of N and K to get more values of Run Time, 
#to see if I missed any trends in my previous analysis.

n<- c()
K <- c()
runtime<-c()
i=0
set.seed(33)
for(size in seq(10,100000, by=20000)){
  for(k in seq(1,500, by= 20)){
    n[i] <- size
    K[i] <- k
    runtime[i] <- helpful.fn(data=train,size=size,k=k)
    i=i+1
  }
}
runtime_dataframe<-data.frame(n,K,runtime)

plot2 <- ggplot(runtime_dataframe, aes(x=n , y=runtime, col=K, group=K)) +
  geom_point() + geom_line() + theme_minimal() + scale_fill_brewer(palette="Dark2")
plot2<-plot2 + labs(title = "Run Time over Sample Size (N) and Number of Neighbours (K)", x= "N", y= "Run Time") 

ggplotly(runtime_plot)
plot3<- ggplot(runtime_dataframe, aes(x=K , y=runtime, col=n, group=n)) +
  geom_point() + geom_line() + theme_minimal() 
plot3<- plot3 + labs(title = "Run Time over Sample Size (N) and Number of Neighbours (K)", x= "N", y= "Run Time")




ggsave(plot2, filename="Abbas Shah.png", width=16, height = 9)
ggsave(plot3,filename=" Abbas Shah2.png", width=16, height=9)




# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3


#Clearly, we can see that increasing N increases the total runtime. However, the relationship between Run Time and K 
#appears to be constant for the same values of N, as demonstrated by Plot 3.
#Thus, I would hypothesize that the relationship is:
#O(n) - as number of N increases, the Runtime seems to increase linearly
#O(c) - as K increases, the Runtime does not seem to increase i.e. it's constant.
#For Number of predictors, I surmise that the relationship will be O(d log d ) - it'll take longer as a more complex model has to be fit.  

#So time complexity can be written as O(n * d log d)

