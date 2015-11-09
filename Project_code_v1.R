#///////////////////////////////
#////                       ////
#////      PROJECT SDS      ////
#////                       ////
#///////////////////////////////

#### Libraries ----
library(Rfacebook)
library(rvest)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

#
# #### Get posts from facebook
# token <- "from fb.com"
#
# face_data <- getPage("nytimes", token, n = 5)
#
# write.csv(face_data, "~/Desktop/face_data.csv", row.names = FALSE)


#### Read data from file ----
#face_data <- read_csv("~/Desktop/face_data.csv")
face_data <- read_csv("https://raw.githubusercontent.com/clausvestergaard/Project_SDS/master/face_data_151109.csv")


#### Matrice with links ----
link_ny <- data.frame(matrix(face_data$link))
names(link_ny) <- c("link")
link_ny$link <- as.character(link_ny$link)


#### Scrape NY Times ----
css.selector = "p"


d <- matrix(nrow=nrow(link_ny),ncol=2)

for(row in 1:nrow(link_ny)){
#for(row in 1:1){

  print(paste("Processing ",row,"/",nrow(link_ny),sep=""))

  web <- read_html(link_ny[row,1]) %>%
    html_nodes(css=css.selector) %>%
    html_text

  a <- matrix(web, nrow=length(web), ncol=1, byrow=TRUE)
  b <- data.frame(a)

  b$laengde <-nchar(as.character(b$a))
  b<-b[(b$laengde>3),]


  # Add text and link to matrix(d)
  d[row,1] <- paste(b$a, collapse=" ")
  d[row,2] <- link_ny[row,1]

  sleep <- 1
  print(paste("--- Pause - resuming in ",sleep," sec. ---", sep=""))
  Sys.sleep(sleep)

}


print("Done scraping!")

#### Combine matrix d and face_data ----
d <- data.frame(d)
names(d) <- c("Txt", "link")
d$Txt <- as.character(d$Txt)
d$link <- as.array(d$link)

my_data <- full_join(face_data, d, by="link")


#### Descriptive data on the article ----
my_data$Article_length <- nchar(my_data$Txt)
my_data$Article_count_dots <- str_count(my_data$Txt,"\\.")

#### Find length of word ----
my_data_2 <- my_data
my_data_2$Txt <- gsub("\\.","",my_data$Txt)
my_data_2$Txt <- gsub(",","",my_data$Txt)
my_data_2$Txt <- gsub(" ","€#",my_data$Txt)

e <- matrix(nrow=nrow(my_data),ncol=2)


for(row in 1:nrow(my_data)){
  c <- data.frame(str_split(my_data_2$Txt[row],pattern = "€#")) #Split string when €# and make into a new dataframe.
  names(c)=c("words")
  c$words <- as.character(c$words)

  #remove words shorther than or equal to 6 letters
  c<- data.frame(c[nchar(c$words) >6 , ])
  c <- matrix(nrow(c),nrow=1,ncol=2)
  c[1,2] <- my_data$link[row]

  e[row,1] <- c[1,1]
  e[row,2] <- c[1,2]

}

#Join the data frames
e <- data.frame(e)
names(e) <- c("num_long_words","link")
my_data <- full_join(my_data, e, by="link")


#Remove old variables and values
rm(a,b,c,d,e,my_data_2,link_ny,row,sleep,web,css.selector,face_data,token)


#### GRAPHS ----
p <- ggplot(data = my_data, aes(x=likes_count, y=Article_length)) +
      geom_line()
p
