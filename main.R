library("XML")
library("dplyr")


WindowsPhoneBadges <- xmlParse("WindowsPhoneBadges.xml")
WindowsPhoneBadges <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhoneBadges)), xmlAttrs, simplify = FALSE))

WindowsPhoneComments <- xmlParse("WindowsPhoneComments.xml")
WindowsPhoneComments <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhoneComments)), xmlAttrs, simplify = FALSE))

WindowsPhonePostHistory <- xmlParse("WindowsPhonePostHistory.xml")
WindowsPhonePostHistory <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhonePostHistory)), xmlAttrs, simplify = FALSE))

WindowsPhonePostLinks <- xmlParse("WindowsPhonePostLinks.xml")
WindowsPhonePostLinks <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhonePostLinks)), xmlAttrs, simplify = FALSE))

WindowsPhonePosts <- xmlParse("WindowsPhonePosts.xml")
WindowsPhonePosts <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhonePosts)), xmlAttrs, simplify = FALSE))

WindowsPhoneTags <- xmlParse("WindowsPhoneTags.xml")
WindowsPhoneTags <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhoneTags)), xmlAttrs, simplify = FALSE))

WindowsPhoneUsers <- xmlParse("WindowsPhoneUsers.xml")
WindowsPhoneUsers <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhoneUsers)), xmlAttrs, simplify = FALSE))

WindowsPhoneVotes <- xmlParse("WindowsPhoneVotes.xml")
WindowsPhoneVotes <- bind_rows(sapply(xmlChildren(xmlRoot(WindowsPhoneVotes)), xmlAttrs, simplify = FALSE))


AndroidBadges <- xmlParse("AndroidBadges.xml")
AndroidBadges <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidBadges)), xmlAttrs, simplify = FALSE))

AndroidComments <- xmlParse("AndroidComments.xml")
AndroidComments <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidComments)), xmlAttrs, simplify = FALSE))

AndroidPostHistory <- xmlParse("AndroidPostHistory.xml")
AndroidPostHistory <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidPostHistory)), xmlAttrs, simplify = FALSE))

AndroidPostLinks <- xmlParse("AndroidPostLinks.xml")
AndroidPostLinks <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidPostLinks)), xmlAttrs, simplify = FALSE))

AndroidPosts <- xmlParse("AndroidPosts.xml")
AndroidPosts <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidPosts)), xmlAttrs, simplify = FALSE))

AndroidTags <- xmlParse("AndroidTags.xml")
AndroidTags <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidTags)), xmlAttrs, simplify = FALSE))

AndroidUsers <- xmlParse("AndroidUsers.xml")
AndroidUsers <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidUsers)), xmlAttrs, simplify = FALSE))

AndroidVotes <- xmlParse("AndroidVotes.xml")
AndroidVotes <- bind_rows(sapply(xmlChildren(xmlRoot(AndroidVotes)), xmlAttrs, simplify = FALSE))


AppleBadges <- xmlParse("AppleBadges.xml")
AppleBadges <- bind_rows(sapply(xmlChildren(xmlRoot(AppleBadges)), xmlAttrs, simplify = FALSE))

AppleComments <- xmlParse("AppleComments.xml")
AppleComments <- bind_rows(sapply(xmlChildren(xmlRoot(AppleComments)), xmlAttrs, simplify = FALSE))

ApplePostHistory <- xmlParse("ApplePostHistory.xml")
ApplePostHistory <- bind_rows(sapply(xmlChildren(xmlRoot(ApplePostHistory)), xmlAttrs, simplify = FALSE))

ApplePostLinks <- xmlParse("ApplePostLinks.xml")
ApplePostLinks <- bind_rows(sapply(xmlChildren(xmlRoot(ApplePostLinks)), xmlAttrs, simplify = FALSE))

ApplePosts <- xmlParse("ApplePosts.xml")
ApplePosts <- bind_rows(sapply(xmlChildren(xmlRoot(ApplePosts)), xmlAttrs, simplify = FALSE))

AppleTags <- xmlParse("AppleTags.xml")
AppleTags <- bind_rows(sapply(xmlChildren(xmlRoot(AppleTags)), xmlAttrs, simplify = FALSE))

AppleUsers <- xmlParse("AppleUsers.xml")
AppleUsers <- bind_rows(sapply(xmlChildren(xmlRoot(AppleUsers)), xmlAttrs, simplify = FALSE))

AppleVotes <- xmlParse("AppleVotes.xml")
AppleVotes <- bind_rows(sapply(xmlChildren(xmlRoot(AppleVotes)), xmlAttrs, simplify = FALSE))




pytanie1<-function(Posts){
  wyn <- Posts %>% select(PostTypeId, Id, CreationDate) %>% mutate(date = substr(CreationDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>% 
    mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y"))%>% select(PostTypeId, Id, month, year) %>% filter(PostTypeId==1) %>%
    group_by(month, year) %>% tally() %>% as.data.frame() %>% arrange(year, month)
  head(wyn, 1000)
}







pytanie2<-function(Users){
  wyn <- Users %>% select(Id, CreationDate) %>% mutate(date = substr(CreationDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>% 
    mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y"))%>% select(Id, month, year) %>%
    group_by(month, year) %>% tally() %>% as.data.frame() %>% arrange(year, month)
  head(wyn, 1000)
}







pytanie3<-function(Users, Posts, Comments){
  Users<- rename(Users, UserId = Id, AccCreatDate = CreationDate)
  Posts<- rename(Posts, PostId = Id, PostCreatDate = CreationDate)
  Comments<- rename(Comments, CommentId = Id, CommentCreatDate = CreationDate)
  
  
  
  wyn1 <- Users %>% left_join(Posts, by = c("UserId" = "OwnerUserId")) %>% select(UserId, PostCreatDate, PostId) %>% mutate(date = substr(PostCreatDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>% mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y")) %>%  mutate(PostId2 = as.integer(PostId)) %>% select( month, year, PostId2)  %>% as.data.frame() %>% arrange(PostId2, year, month) %>% group_by(month, year)%>% tally()%>% mutate(monthYear = paste(month, year, sep = " ")) %>% as.data.frame() 

  wyn2 <- Users %>% left_join(Comments, by = c("UserId" = "UserId")) %>% select(UserId, CommentCreatDate, CommentId) %>% mutate(date = substr(CommentCreatDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>% mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y")) %>%  mutate(CommentId2 = as.integer(CommentId)) %>% select( month, year, CommentId2)  %>% as.data.frame() %>% arrange(CommentId2, year, month)%>% group_by(month, year)%>% tally()%>% mutate(monthYear = paste(month, year, sep = " "))%>% as.data.frame()
  
  
  wyn3<- wyn1 %>% full_join(wyn2, by = c("monthYear" = "monthYear"))
  
  temp <- is.na(wyn3$n.y)
  wyn3$n.y[temp] <- 0
  
  temp <- is.na(wyn3$n.x)
  wyn3$n.x[temp] <- 0
  wyn3 <- wyn3 %>% mutate (aktywnosc = (wyn3$n.x + wyn3$n.y)) %>% select (monthYear, aktywnosc)
  

  
  
  head(wyn3, length(wyn3$aktywnosc)-1)

}


pytanie4 <-function(Users){
  
  temp <- is.na(Users$UpVotes)
  Users<-Users[!temp, ]
  wyn1 <- Users %>% select(Id, CreationDate, UpVotes) %>% mutate(date = substr(CreationDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>% mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y")) %>% select( month, year, Id, UpVotes)  %>% group_by(month, year, UpVotes)%>% tally()%>% as.data.frame() 
  
  print (wyn1)
 
  
  
}







write.csv(pytanie1(ApplePosts), file = "ApplePyt1.csv", row.names = FALSE)
write.csv(pytanie1(WindowsPhonePosts), file = "WindowsPhonePyt1.csv", row.names = FALSE)
write.csv(pytanie1(AndroidPosts), file = "AndroidPyt1.csv", row.names = FALSE)


write.csv(pytanie2(ApplePosts), file = "ApplePyt2.csv", row.names = FALSE)
write.csv(pytanie2(WindowsPhonePosts), file = "WindowsPhonePyt2.csv", row.names = FALSE)
write.csv(pytanie2(AndroidPosts), file = "AndroidPyt2.csv", row.names = FALSE)


write.csv(pytanie3(WindowsPhoneUsers, WindowsPhonePosts, WindowsPhoneComments), file = "WindowsPhonePyt3.csv", row.names = FALSE)
write.csv(pytanie3(AppleUsers, ApplePosts, AppleComments), file = "ApplePyt3.csv", row.names = FALSE)
write.csv(pytanie3(AndroidUsers, AndroidPosts, AndroidComments), file = "AndroidPyt3.csv", row.names = FALSE)


write.csv(pytanie4(WindowsPhoneUsers), file = "WindowsPhonePyt4.csv", row.names = FALSE)
write.csv(pytanie4(AppleUsers), file = "ApplePyt4.csv", row.names = FALSE)
write.csv(pytanie4(AndroidUsers), file = "AndroidPyt4.csv", row.names = FALSE)






