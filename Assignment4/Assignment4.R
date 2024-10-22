# Alex Gibbons and Lauren Kawecki
library(RMySQL)

height_data <- read.table("C:\\Users\\Kelly\\Desktop\\MCSP24\\CMPT363 - Data Mining\\tab_sep_data.txt",header=TRUE,sep="\t")
weight_data <- read.table("C:\\Users\\Kelly\\Desktop\\MCSP24\\CMPT363 - Data Mining\\comma_sep_data.txt",header=TRUE,sep=",")


mean_height <- mean(height_data$height, na.rm=TRUE)
for(i in 1:nrow(height_data)) {
  if(is.na(height_data[i,2])) {
    height_data[i,2] <- mean_height
  }
}


mean_weight <- mean(weight_data$weight, na.rm=TRUE)
for(i in 1:nrow(weight_data)) {
  if(is.na(weight_data[i,2])) {
    weight_data[i,2] <- mean_weight
  }
}

student_measurements <- merge(x=weight_data,y=height_data, by="id", all=TRUE)

mydb <- dbConnect(MySQL(),user="root",password="root",host="localhost")
dbSendQuery(mydb, "DROP DATABASE IF EXISTS student_data;")
dbSendQuery(mydb, "CREATE DATABASE student_data;")
dbSendQuery(mydb, "USE student_data;")
dbSendQuery(mydb, "CREATE TABLE measurements(id INT, weight INT, height INT);")
query <- "INSERT INTO measurements VALUES"
query <- paste0(query, paste(sprintf("('%f','%f','%f')",student_measurements$id,student_measurements$weight,student_measurements$height),collapse=","))
dbSendQuery(mydb, query)
query_results <- fetch(dbSendQuery(mydb, "SELECT id,weight,height FROM measurements;"))
print(query_results)


