library(dplyr)

raw <- read_reddit_stream("data-raw/RC_2008_01_1000.json")

RC_2008_01_1000 <- raw %>%
    rm_attrs(c("id", "author", "subreddit", "body"), negative=TRUE)

save(RC_2008_01_1000, file="data/RC_2008_01_1000.rda")
