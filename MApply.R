library(microbenchmark)
library(ggplot2)

#using mapply to find sum of respective elements of sub_1, sub_2 and sub_3
demo_list <- list(sub_1 = c(1:10), sub_2 = c(11:20), sub_3 = c(21:30))
mapply_function <- function()
{
  results_mapply <- mapply(sum, demo_list$sub_1, demo_list$sub_2, demo_list$sub_3)
}


#Equivalent for loop
loop_function <- function()
{
  temp <- 1
  results_loop <- list()
  for(i in 1:length(demo_list[[temp]]))
  {
    results_loop<- c(results_loop,(demo_list$sub_1[i] + demo_list$sub_2[i] + demo_list$sub_3[i]))
  }
}
loop_function()

microbenchmark(mapply_function(),loop_function())


# Performance graph for mapply
performance_mapply <- data.frame(Replications = character(), Elapsed_Time = numeric())
rep <- c(100,500,1000,1500,2000)
for(i in rep){
  tmp <- benchmark(mapply(sum, demo_list$sub_1, demo_list$sub_2, demo_list$sub_3))[3]
  performance_mapply <- rbind(performance_mapply, data.frame(as.character(i), tmp))
}
names(performance_mapply) <- c("replications","elapsed_time")

# Performance graph for loop
performance_for <- data.frame(Replications = character(), Elapsed_Time = numeric())
rep <- c(100,500,1000,1500,2000)
for(i in rep){
  tmp <- benchmark(loop_function(), replications = i)[3]
  performance_for <- rbind(performance_for, data.frame(as.character(i), tmp))
}
names(performance_for) <- c("replications","elapsed_time_for")

performance_final <- cbind(performance_mapply, performance_for$elapsed_time_for)
names(performance_final)[3] <- "elapsed_time_for"


#plot for the performance comparison
ggplot(performance_final, aes(x = as.integer(replications))) + 
  geom_line(aes(y = elapsed_time, colour = "mapply_time")) + 
  geom_line(aes(y = elapsed_time_for, colour = "loop_time"))
