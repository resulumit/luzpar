library(rio)
library(tidyverse)

df <- import("utility/mock_mps.xlsx", which = "Sheet1") %>% 
        mutate(extension = as.character(extension))


for (i in 1:length(df$name_url)) {
        
        
mp_since_rand <- sample(c(2020, 2015, 2010, 2005, 2000), 1)
term <- ceiling((as.numeric(format(Sys.Date(), "%Y")) - mp_since_rand) / 5)
mp_term_rand <- case_when(term == 1 ~ "first",
                     term == 2 ~ "second",
                     term == 3 ~ "third",
                     term == 4 ~ "fourth",
                     term == 5 ~ "fifth")
fp_since <- mp_since_rand + sample(0:(2021-mp_since_rand), 1)
sp_since <- mp_since_rand + sample(0:(2021-mp_since_rand), 1)

mp_attended_num <- sample(23:65, 1)
mp_spoke_num <- sample(7:46, 1)

parties <- unique(df$party)[sample(1:length(unique(df$party)))]
other_parties <- parties[parties != df$party[i] & 
                         parties != "Independent"]

first_vote_num <- sample(20138:36234, 1)
second_vote_num <- floor(first_vote_num * sample(75:98, 1) / 100)
third_vote_num <- floor(second_vote_num * sample(75:98, 1) / 100)
fourth_vote_num <- floor(third_vote_num * sample(75:98, 1) / 100)

electorate <- first_vote_num + second_vote_num + third_vote_num + fourth_vote_num

first_share_num <- round(first_vote_num * 100 / electorate, 1)
second_share_num <- round(second_vote_num * 100 / electorate, 1)
third_share_num <- round(third_vote_num * 100 / electorate, 1)
fourth_share_num <- round(fourth_vote_num * 100 / electorate, 1)

mp_file <- paste0("content/members/", df$name_url[i], ".html")
mp_text <- read_lines("utility/mp_text.html") %>% 
        str_replace_all("mp_title", df$title[i]) %>% 
        str_replace_all("mp_name", df$name[i]) %>% 
        str_replace_all("mp_surname", df$surname[i]) %>% 
        str_replace_all("mp_subject_low", tolower(df$subject[i])) %>%
        str_replace_all("mp_subject_high", df$subject[i]) %>%
        str_replace_all("mp_object", df$object[i]) %>%
        str_replace_all("mp_profession", df$profession[i]) %>% 
        str_replace_all("mp_constituency", df$constituency[i]) %>%
        str_replace_all("constituency_url", df$constituency_url[i]) %>% 
        str_replace_all("mp_party", df$party[i]) %>% 
        str_replace_all("mp_region", df$region[i]) %>% 
        str_replace_all("mp_contact", sample(c("e-mail", "telephone"))) %>% 
        str_replace_all("mp_email", df$email[i]) %>% 
        str_replace_all("mp_phone", df$extension[i]) %>% 
        str_replace_all("mp_website", df$website[i]) %>%
        str_replace_all("mp_since", as.character(mp_since_rand)) %>%
        str_replace_all("mp_term", mp_term_rand) %>% 
        str_replace_all("first_committee", df$firstcom[i]) %>% 
        str_replace_all("first_position", df$firstcom_pos[i]) %>% 
        str_replace_all("first_since", as.character(fp_since)) %>% 
        str_replace_all("second_committee", df$secondcom[i]) %>% 
        str_replace_all("second_position", df$secondcom_pos[i]) %>% 
        str_replace_all("second_since", as.character(fp_since)) %>%
        str_replace_all("mp_attended", as.character(mp_attended_num)) %>%
        str_replace_all("mp_spoke", as.character(mp_spoke_num)) %>%
        str_replace_all("first_party", df$party[i]) %>%
        str_replace_all("first_vote", as.character(format(first_vote_num, big.mark = ","))) %>%
        str_replace_all("first_share", paste0(as.character(first_share_num), "%")) %>%
        str_replace_all("second_party", other_parties[1]) %>%
        str_replace_all("second_vote", as.character(format(second_vote_num, big.mark = ","))) %>%
        str_replace_all("second_share", paste0(as.character(second_share_num), "%")) %>%
        str_replace_all("third_party", other_parties[2]) %>%
        str_replace_all("third_vote", as.character(format(third_vote_num, big.mark = ","))) %>%
        str_replace_all("third_share", paste0(as.character(third_share_num), "%")) %>%
        str_replace_all("fourth_party", other_parties[3]) %>%
        str_replace_all("fourth_vote", as.character(format(fourth_vote_num, big.mark = ","))) %>%
        str_replace_all("fourth_share", paste0(as.character(fourth_share_num), "%"))
        

file.create(mp_file)
writeLines(text = mp_text, con = mp_file, useBytes = TRUE)



# constituency ------------------------------------------------------------

constituency_file <- paste0("content/constituencies/", df$constituency_url[i], ".html")
constituency_text <- read_lines("utility/constituency_text.html") %>% 
        str_replace_all("constituency_name", df$constituency[i]) %>% 
        str_replace_all("constituency_region", df$region[i]) %>% 
        str_replace_all("mp_title", df$title[i]) %>% 
        str_replace_all("mp_name", df$name[i]) %>% 
        str_replace_all("mp_surname", df$surname[i]) %>% 
        str_replace_all("mp_url", df$name_url[i]) %>% 
        str_replace_all("mp_party", df$party[i]) %>% 
        str_replace_all("mp_since", as.character(mp_since_rand)) %>%
        str_replace_all("first_party", df$party[i]) %>%
        str_replace_all("first_vote", as.character(format(first_vote_num, big.mark = ","))) %>%
        str_replace_all("first_share", paste0(as.character(first_share_num), "%")) %>%
        str_replace_all("second_party", other_parties[1]) %>%
        str_replace_all("second_vote", as.character(format(second_vote_num, big.mark = ","))) %>%
        str_replace_all("second_share", paste0(as.character(second_share_num), "%")) %>%
        str_replace_all("third_party", other_parties[2]) %>%
        str_replace_all("third_vote", as.character(format(third_vote_num, big.mark = ","))) %>%
        str_replace_all("third_share", paste0(as.character(third_share_num), "%")) %>%
        str_replace_all("fourth_party", other_parties[3]) %>%
        str_replace_all("fourth_vote", as.character(format(fourth_vote_num, big.mark = ","))) %>%
        str_replace_all("fourth_share", paste0(as.character(fourth_share_num), "%"))

file.create(constituency_file)
writeLines(text = constituency_text, con = constituency_file, useBytes = TRUE)  

}
