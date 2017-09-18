redirection_url <- function(old_url){
  library(rvest)
  library(stringr)
  
  web <- read_html(old_url)
  content_html <- web %>% html_nodes("body") %>%html_nodes("table#job-content") %>% 
    html_nodes("td.snip") %>% html_node("div#bvjl") %>% html_node("a")
  
  content_str <- as.character(content_html) 
  start <- str_locate(content_str, "href=")[,2] + 2
  end <- str_locate(content_str, "target")[,1] - 3
  redirection <- paste0("www.indeed.fr", str_sub(content_str, start, end))
  
  return(redirection)
}

page_url <- function(x){
  library(httr)
  
  page <- tryCatch(GET(x)$url, error = function(x){return("Error")})
  return(page)
}

telecharge_database <- function(post, sort_by){
  library(jobbR)
  library(data.table)
  
  jobs <- jobSearch(publisher="2246321477645649", post, country = "fr", location = "paris", limit = 25, sort = sort_by)
  jobs <- as.data.table(jobs)
  jobs <- jobs[, c("query","results.jobtitle", "results.company", "results.date","results.snippet", "results.url", "results.formattedLocationFull"
                   ,"results.formattedRelativeTime", "results.indeedApply")]
  colnames(jobs) <- c("searchKey","title", "company", "date", "description", "url", "location", "relativetime", "indeed")
  jobs$date <- dmy_hms(str_sub(jobs$date, 6, 25))
  
  return(jobs)
}

sendNotification <- function(post, recipents){
  library(mailR)
  library(htmlTable)
  library(data.table)
  
  filename <- paste0("old_jobs_", post, ".rds")
  data <- as.data.table(readRDS(filename))
  data[, c("searchKey", "indeed", "redirection"):=NULL]
  data[order(-rank(date))]
  html_data <- htmlTable(data)
  subject <- toupper(paste0("jobs for ", str_replace(post, "\\+", " ")))
  
  sender <- "liheng87@gmail.com"
  send.mail(from = sender,
            to = recipents, 
            subject = subject, 
            body = html_data,
            html = T,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "liheng87@gmail.com", passwd = "liheng840726", ssl = T),
            authenticate = T,
            send = T)
}
