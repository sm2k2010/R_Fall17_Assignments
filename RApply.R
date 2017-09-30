library("microbenchmark")
#Creating a demo list
demo_list <- list(1:5,list(6:9),10:12,list(13:20))

#Applying rapply to find squares of each element
rapply_function <- function()
{
  list_rapply <- rapply(demo_list, function(demo_list){demo_list^2})
}

#Using loops to do the same evaluations

list_loop<- list()
loop_function <- function()
{
  for(i in 1:length(demo_list))
  {
    for(j in 1:length(demo_list[[i]]))
    {
      if(class(demo_list[[i]][j]) == "list")
      {
        subList <- demo_list[[i]][j]
        for(k in 1:length(subList[[1]]))
          list_loop <- c(list_loop,subList[[1]][k]^2)
      }
      else
      {
        list_loop <- c(list_loop,demo_list[[i]][j]^2)
      }
    }
  }
}

#Benchmarking both the functions
microbenchmark(rapply_function(),loop_function())

# Performance graph for rapply
performance_rapply <- data.frame(Replications = character(), Elapsed_Time = numeric())
rep <- c(100,500,1000,1500,2000)
for(i in rep){
  tmp <- benchmark(rapply(demo_list, function(demo_list){demo_list^2}))[3]
  performance_rapply <- rbind(performance_rapply, data.frame(as.character(i), tmp))
}
names(performance_rapply) <- c("replications","elapsed_time")

# Performance graph for loop
performance_for <- data.frame(Replications = character(), Elapsed_Time = numeric())
rep <- c(100,500,1000,1500,2000)
for(i in rep){
  tmp <- benchmark(loop_function(), replications = i)[3]
  performance_for <- rbind(performance_for, data.frame(as.character(i), tmp))
}
names(performance_for) <- c("replications","loop_time")

performance_final <- cbind(performance_rapply, performance_for$loop_time)
names(performance_final)[3] <- "loop_time"


#plot for the performance comparison
ggplot(performance_final, aes(x = as.integer(replications))) + 
  geom_line(aes(y = elapsed_time, colour = "rapply_time")) + 
  geom_line(aes(y = loop_time, colour = "loop_time"))
