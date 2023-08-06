df = data.frame(
  col1=c("this", "is", "a", 'vector', ' of','strings'),
  col2 = 1:6,
  col3 =letters[1:6],
  stringsAsFactors = FALSE
)

df[,2]
df$col2

#broadcasting

a <- 1:3
b <- 1:9
a+b

#ufunc

#que acepte vectores. 
str(df)
names(df)
head(df)
nrow(df)
ncol(df)


is.na(df)
colSums(is.na(df))


df_2 <- rbind(df, c("!",NA,NA))
df_2

df_2[!is.na(df_2$col3),]

#----*Funciones*----

find_mean = function(x){
  return(sum(x)/length(x))
}
vec <- 1:10
find_mean(vec)
find_mean(data.frame(col1 = 1:10, col2=5:14))


data <- data.frame(
  a =1:10,
  b = sample(c("GT", "US", "CA"), size = 10, replace = TRUE)
)


find_sample <- function(x){
  for_index<-sample(1:nrow(x), size=10, replace = FALSE)
  new_df <- x[for_index,]
  return(new_df)
}

find_sample(data)

#undersampling/ oversampling
#hyperparametros

lapply(list, function)



