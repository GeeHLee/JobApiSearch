library(data.table)
library(lubridate)
library(stringr)
source(paste0(getwd(),"/function.R"))

today <- Sys.time()
post_all <- c("assistante+marketing", "assistante+chef+projet")
sort_by <- "date"

for(post in post_all){
  filename <- paste0("old_jobs_", post, ".rds")
  jobs_1 <- telecharge_database(post, "date")
  jobs_2 <- telecharge_database(post, "relevance")
  jobs_2 <- jobs_2[as.numeric(today - date) <= 14, ]
  jobs <- unique(rbind(jobs_1, jobs_2))
  
  
  jobs[indeed == TRUE, redirection := url]
  jobs[indeed == FALSE, redirection:=lapply(.SD, redirection_url), by = "url", .SDcols="url"]
  jobs[,url:=NULL]
  jobs$page <- unlist(lapply(jobs$redirection, page_url))
  jobs[,page:=ifelse(page=="Error", redirection, page)]
  
  if(filename %in% ls()==TRUE){
    old_jobs <- readRDS(filename)
    new_jobs <- jobs[date > max(old_jobs$date)]
    jobs <- rbind(new_jobs, old_jobs)
    saveRDS(jobs, file=filename)
  }else{
    saveRDS(jobs, file=filename)
  }
}


for(post in post_all){
  sendNotification(post, "m.seksaoui@gmail.com")
}
