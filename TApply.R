# Creating a demo data_frame, assigning student_id and randoming marks, and adding labels, based on year
df_demo <- data.frame(student_id = 1:10000, marks = rnorm(10000, mean = 60, sd = 10),
                      year = gl(4, 2500,
                      labels = c("2017","2016","2015","2014")))

#Assigning grades according to marks obtained
  for(i in seq_along(df_demo$student_id))
  {
    if(df_demo$marks[i] > 90)
    {
      df_demo$grade[i] <- "O"
    }
    else if(df_demo$marks[i] > 80)
    {
      df_demo$grade[i] <- "E"
    }
    else if(df_demo$marks[i] > 75)
    {
      df_demo$grade[i] <- "A"
    }
    else if(df_demo$marks[i] > 70)
    {
      df_demo$grade[i] <- "B"
    }
    else if(df_demo$marks[i] > 65)
    {
      df_demo$grade[i] <- "C"
    }
    else if(df_demo$marks[i] > 60)
    {
      df_demo$grade[i] <- "D"
    }
    else
    {
      df_demo$grade[i] <- "F"
    }
  }

#Using tapply
tapply_function <- function()
{
  tapply(df_demo$marks, df_demo$year, mean)
}

#Using loop
loop_function <- function()
{
  for(i in unique(df_demo$year))
  {
    c(mean(df_demo[which(df_demo$year == i),"marks"]),i)
  }
}
loop_function()
tapply_function()

#benchmarking
microbenchmark(tapply_function(),loop_function())

# Performance graph for tapply
performance_tapply <- data.frame(Replications = character(), Elapsed_Time = numeric())
rep <- c(100,500,1000,1500,2000)
for(i in rep){
  tmp <- benchmark(tapply(df_demo$marks, df_demo$year, mean))[3]
  performance_tapply <- rbind(performance_tapply, data.frame(as.character(i), tmp))
}
names(performance_tapply) <- c("replications","elapsed_time")

# Performance graph for loop
performance_for <- data.frame(Replications = character(), Elapsed_Time = numeric())
rep <- c(100,500,1000,1500,2000)
for(i in rep){
  tmp <- benchmark(loop_function(), replications = i)[3]
  performance_for <- rbind(performance_for, data.frame(as.character(i), tmp))
}
names(performance_for) <- c("replications","elapsed_time_for")

performance_final <- cbind(performance_tapply, performance_for$elapsed_time_for)
names(performance_final)[3] <- "elapsed_time_for"


#plot for the performance comparison
ggplot(performance_final, aes(x = as.integer(replications))) + 
  geom_line(aes(y = elapsed_time, colour = "tapply_time")) + 
  geom_line(aes(y = elapsed_time_for, colour = "loop_time"))