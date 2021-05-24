library(rvest)
library(purrr)
library(stringr)
library(dplyr)

try_numeric <- function(vec) {
    func <- purrr::quietly(as.numeric)
    res <- func(vec)
    if (length(res[['warnings']]) > 0) { 
        vec
    } else {
        res[['result']]
    }
}

# 2021 ----
euro2021 <- read_html('https://en.wikipedia.org/wiki/Eurovision_Song_Contest_2021')

tables2021 <- euro2021 %>% html_elements('table')
table_captions2021 <- tables2021 %>% html_element('caption') %>% html_text()
voting_table_idx2021 <- str_which(table_captions2021, 'voting results')
voting_tables2021 <- tables2021[voting_table_idx2021] %>% html_table()

cleaned_voting_tables2021 <- vector('list', length(voting_tables2021))
names(cleaned_voting_tables2021) <- c('SF1_Jury','SF1_Tele', 'SF2_Jury','SF2_Tele', 'Final_Jury','Final_Tele')
for (i in seq_along(voting_tables2021)) {
    df <- voting_tables2021[[i]]
    names(df) <- as.character(df[1,])
    names(df)[3] <- 'Total score'
    names(df)[2] <- 'Country'
    startrow <- ifelse(i == 2, 4, 3)
    cleaned_voting_tables2021[[i]] <- df[startrow:nrow(df), 2:ncol(df)] %>% 
        map_df(.f = function(x) str_replace(x, pattern = "^$", replacement = '0')) %>%
        purrr::map_df(.f = try_numeric) 
}

tidy_voting_tables2021 <- vector('list', length(cleaned_voting_tables2021))
names(tidy_voting_tables2021) <- names(cleaned_voting_tables2021)
for (i in seq_along(cleaned_voting_tables2021)) {
tidy_voting_tables2021[[i]] <- cleaned_voting_tables2021[[i]] %>%
    select(-ends_with('score')) %>%
    tidyr::pivot_longer(cols = c(everything(), -Country), names_to = 'AwardedFrom', values_to = 'Points') %>%
    rename(AwardedTo = Country) %>%
    mutate(Points = ifelse(AwardedFrom == AwardedTo, NA, Points)
           )
}

all_votes2021 <- bind_rows(tidy_voting_tables2021, .id = 'VotingSet')

# 2019 ----
euro2019 <- read_html('https://en.wikipedia.org/wiki/Eurovision_Song_Contest_2019')

tables2019 <- euro2019 %>% html_elements('table')
table_captions2019 <- tables2019 %>% html_element('caption') %>% html_text()
voting_table_idx2019 <- str_which(table_captions2019, 'voting results')
voting_tables2019 <- tables2019[voting_table_idx2019] %>% html_table()

cleaned_voting_tables2019 <- vector('list', length(voting_tables2019))
names(cleaned_voting_tables2019) <- c('SF1_Jury','SF1_Tele', 'SF2_Jury','SF2_Tele', 'Final_Jury','Final_Tele')
for (i in seq_along(voting_tables2019)) {
    df <- voting_tables2019[[i]]
    names(df) <- as.character(df[1,])
    names(df)[3] <- 'Total score'
    names(df)[2] <- 'Country'
    startrow <- 3
    cleaned_voting_tables2019[[i]] <- df[startrow:nrow(df), 2:ncol(df)] %>% 
        map_df(.f = function(x) str_replace(x, pattern = "^$", replacement = '0')) %>%
        purrr::map_df(.f = try_numeric) 
}

tidy_voting_tables2019 <- vector('list', length(cleaned_voting_tables2019))
names(tidy_voting_tables2019) <- names(cleaned_voting_tables2019)
for (i in seq_along(cleaned_voting_tables2019)) {
    tidy_voting_tables2019[[i]] <- cleaned_voting_tables2019[[i]] %>%
        select(-ends_with('score')) %>%
        tidyr::pivot_longer(cols = c(everything(), -Country), names_to = 'AwardedFrom', values_to = 'Points') %>%
        rename(AwardedTo = Country) %>%
        mutate(Points = ifelse(AwardedFrom == AwardedTo, NA, Points)
        )
}

all_votes2019 <- bind_rows(tidy_voting_tables2019, .id = 'VotingSet')

# 2018 ----
euro2018 <- read_html('https://en.wikipedia.org/wiki/Eurovision_Song_Contest_2018')

tables2018 <- euro2018 %>% html_elements('table')
table_captions2018 <- tables2018 %>% html_element('caption') %>% html_text()
voting_table_idx2018 <- str_which(table_captions2018, 'voting results')
voting_tables2018 <- tables2018[voting_table_idx2018] %>% html_table()

cleaned_voting_tables2018 <- vector('list', length(voting_tables2018))
names(cleaned_voting_tables2018) <- c('SF1_Jury','SF1_Tele', 'SF2_Jury','SF2_Tele', 'Final_Jury','Final_Tele')
for (i in seq_along(voting_tables2018)) {
    df <- voting_tables2018[[i]]
    names(df) <- as.character(df[1,])
    names(df)[3] <- 'Total score'
    names(df)[2] <- 'Country'
    startrow <- 3
    cleaned_voting_tables2018[[i]] <- df[startrow:nrow(df), 2:ncol(df)] %>% 
        map_df(.f = function(x) str_replace(x, pattern = "^$", replacement = '0')) %>%
        purrr::map_df(.f = try_numeric) 
}

tidy_voting_tables2018 <- vector('list', length(cleaned_voting_tables2018))
names(tidy_voting_tables2018) <- names(cleaned_voting_tables2018)
for (i in seq_along(cleaned_voting_tables2018)) {
    tidy_voting_tables2018[[i]] <- cleaned_voting_tables2018[[i]] %>%
        select(-ends_with('score')) %>%
        tidyr::pivot_longer(cols = c(everything(), -Country), names_to = 'AwardedFrom', values_to = 'Points') %>%
        rename(AwardedTo = Country) %>%
        mutate(Points = ifelse(AwardedFrom == AwardedTo, NA, Points)
        )
}

all_votes2018 <- bind_rows(tidy_voting_tables2018, .id = 'VotingSet')

# 2017 ----
euro2017 <- read_html('https://en.wikipedia.org/wiki/Eurovision_Song_Contest_2017')

tables2017 <- euro2017 %>% html_elements('table')
table_captions2017 <- tables2017 %>% html_element('caption') %>% html_text()
voting_table_idx2017 <- str_which(table_captions2017, 'voting results')
voting_tables2017 <- tables2017[voting_table_idx2017] %>% html_table()

cleaned_voting_tables2017 <- vector('list', length(voting_tables2017))
names(cleaned_voting_tables2017) <- c('SF1_Jury','SF1_Tele', 'SF2_Jury','SF2_Tele', 'Final_Jury','Final_Tele')
for (i in seq_along(voting_tables2017)) {
    df <- voting_tables2017[[i]]
    names(df) <- as.character(df[1,])
    names(df)[3] <- 'Total score'
    names(df)[2] <- 'Country'
    startrow <- 3
    cleaned_voting_tables2017[[i]] <- df[startrow:nrow(df), 2:ncol(df)] %>% 
        map_df(.f = function(x) str_replace(x, pattern = "^$", replacement = '0')) %>%
        purrr::map_df(.f = try_numeric) 
}

tidy_voting_tables2017 <- vector('list', length(cleaned_voting_tables2017))
names(tidy_voting_tables2017) <- names(cleaned_voting_tables2017)
for (i in seq_along(cleaned_voting_tables2017)) {
    tidy_voting_tables2017[[i]] <- cleaned_voting_tables2017[[i]] %>%
        select(-ends_with('score')) %>%
        tidyr::pivot_longer(cols = c(everything(), -Country), names_to = 'AwardedFrom', values_to = 'Points') %>%
        rename(AwardedTo = Country) %>%
        mutate(Points = ifelse(AwardedFrom == AwardedTo, NA, Points)
        )
}

all_votes2017 <- bind_rows(tidy_voting_tables2017, .id = 'VotingSet')

# 2016 ----
euro2016 <- read_html('https://en.wikipedia.org/wiki/Eurovision_Song_Contest_2016')

tables2016 <- euro2016 %>% html_elements('table')
table_captions2016 <- tables2016 %>% html_element('caption') %>% html_text()
voting_table_idx2016 <- str_which(table_captions2016, 'voting results')
voting_tables2016 <- tables2016[voting_table_idx2016] %>% html_table()

cleaned_voting_tables2016 <- vector('list', length(voting_tables2016))
names(cleaned_voting_tables2016) <- c('SF1_Jury','SF1_Tele', 'SF2_Jury','SF2_Tele', 'Final_Jury','Final_Tele')
for (i in seq_along(voting_tables2016)) {
    df <- voting_tables2016[[i]]
    names(df) <- as.character(df[1,])
    names(df)[3] <- 'Total score'
    names(df)[2] <- 'Country'
    startrow <- 3
    cleaned_voting_tables2016[[i]] <- df[startrow:nrow(df), 2:ncol(df)] %>% 
        map_df(.f = function(x) str_replace(x, pattern = "^$", replacement = '0')) %>%
        purrr::map_df(.f = try_numeric) 
}

tidy_voting_tables2016 <- vector('list', length(cleaned_voting_tables2016))
names(tidy_voting_tables2016) <- names(cleaned_voting_tables2016)
for (i in seq_along(cleaned_voting_tables2016)) {
    tidy_voting_tables2016[[i]] <- cleaned_voting_tables2016[[i]] %>%
        select(-ends_with('score')) %>%
        tidyr::pivot_longer(cols = c(everything(), -Country), names_to = 'AwardedFrom', values_to = 'Points') %>%
        rename(AwardedTo = Country) %>%
        mutate(Points = ifelse(AwardedFrom == AwardedTo, NA, Points)
        )
}

all_votes2016 <- bind_rows(tidy_voting_tables2016, .id = 'VotingSet')

# Merging all ----
all_votes <- list(`2021` = all_votes2021,
                  `2019` = all_votes2019,
                  `2018` = all_votes2018,
                  `2017` = all_votes2017,
                  `2016` = all_votes2016) %>% 
    bind_rows(.id = 'Year')

readr::write_delim(all_votes, file.path('data','all_votes.csv'), delim = ';')
