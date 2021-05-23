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


pytanie1(ApplePosts)
pytanie1(WindowsPhonePosts)
pytanie1(AndroidPosts)




pytanie2<-function(Users){
  wyn <- Users %>% select(Id, CreationDate) %>% mutate(date = substr(CreationDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>% 
    mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y"))%>% select(Id, month, year) %>%
    group_by(month, year) %>% tally() %>% as.data.frame() %>% arrange(year, month)
  head(wyn, 1000)
}



pytanie2(ApplePosts)
pytanie2(WindowsPhonePosts)
pytanie2(AndroidPosts)



pytanie3<-function(Users, Posts, Comments){
  Users<- rename(Users, UserId = Id, AccCreatDate = CreationDate)
  Posts<- rename(Posts, PostId = Id, PostCreatDate = CreationDate)
  wyn1 <- Users %>% left_join(Posts, by = c("UserId" = "OwnerUserId")) %>% select(UserId, PostCreatDate, PostId) %>% mutate(date = substr(PostCreatDate, 1, 10)) %>% mutate(date2 = as.Date(date, "%Y-%m-%d")) %>%
    mutate(month = format(date2, "%m"))%>% mutate(year = format(date2, "%Y")) %>%  mutate(PostId2 = as.integer(PostId)) %>% select(UserId, month, year, PostId2)  %>% as.data.frame() %>% arrange(PostId2, year, month) 
  
  wyn1 

}

pytanie3(WindowsPhoneUsers, WindowsPhonePosts, WindowsPhoneComments)
