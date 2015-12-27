# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 4/28/2015
# Date updated:

###############################################################################
## SCRIPT OVERVIEW

# goal: Supporting document for the NOAA data analysis. 
#
#       I am separating my on-the-fly work from the Rmd I am preparing.

# sketch of script
# - load libraries
# - function definitions
# - gather and arrange NOAA data
# - review and clean NOAA data
#
# - old material (repurpose or drop)

# key documentation
# - NOAA storm database site: http://www.ncdc.noaa.gov/stormevents/
# - short summary of database objective and some key limitations:
#   http://www.ncdc.noaa.gov/stormevents/details.jsp
# - long discussion of key storm events and some other details:
#   http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
# - information about the database CSVs:
#   http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/README
# - field/column descriptions for each table:
#   http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx

###############################################################################
## load libraries

setwd("C:/Users/BCW/Dropbox/Brian/github/NOAA_analysis/")

library(rvest)      # getting the links to the source data files
library(stringr)    # for cleaning EVTYPE character strings
library(dplyr)      # for dataset reformatting
library(tidyr)      # for dataset reformatting
library(lubridate)  # for date interactions
library(ggplot2)    # for making our data visualizations

###############################################################################
## function definitions

get_flavor_match <- function(file_name, flavor_set) {
    test_set <- c()
    
    for(index in 1:length(flavor_set)) {
        current_test <- grepl(flavor_set[index], file_name)
        test_set <- c(test_set, current_test)
    }
    
    flavor_match <- flavor_set[test_set]
    
    return(flavor_match)
}

download_noaa_data <- function(source_url, file_names, flavor_set) {
    download_dir <- "./noaa_collection"
    
    if(download_dir %in% list.dirs()) {
        stop("Please remove the 'noaa_collection' folder from the working",
             "directory or change the working directory. This function uses",
             "that folder name.")
    }
    
    dir.create(download_dir)
    
    download_path_set <- c()
    
    for(index in 1:length(file_names)) {
        current_name <- file_names[index]
        target_url <- paste0(source_url, current_name)
        download_path <- paste0(download_dir, "/",
                                flavor_set[[index]], index, 
                                ".csv.gz")
        
        download.file(target_url, download_path)
        download_path_set <- c(download_path_set, download_path)
        
        message("File ", index, " of ", length(file_names))
        message("")
    }
    
    return(download_path_set)
}

make_up_paths <- function(flavor_matches) {
    base_path <- "./noaa_collection"
    path_set <- c()
    
    for(index in 1:length(flavor_matches)) {
        current_flavor <- flavor_matches[[index]]
        current_path <- paste0(base_path, "/",
                               current_flavor, index,
                               ".csv.gz")
        
        path_set <- c(path_set, current_path)
    }
    
    return(path_set)
}

import_noaa_data <- function(download_path_set, flavor_matches) {
    noaa_collection <- list()
    
    for(index in 1:length(download_path_set)) {
        current_path <- download_path_set[[index]]
        current_flavor <- flavor_matches[[index]]
        
        current_import <- read.csv(current_path, stringsAsFactors = FALSE)
        
        noaa_collection[[current_flavor]][[index]] <- current_import
        
        message("File ", index, " of ", length(download_path_set))
    }
    
    # this solution results in null values being inserted into the sublists
    # when there is a gap between the index value and the values already
    # in the list (e.g., noaa_collection[["locations"]][[150]] when nothing
    # yet exists in that sublist); we remove the null values
    for(index in 1:length(noaa_collection)) {
        noaa_collection[[index]] <- Filter(Negate(is.null), 
                                           noaa_collection[[index]])
    }
    
    return(noaa_collection)
}

column_check <- function(sublist) {
    first_col_names <- names(sublist[[1]])
    test <- lapply(sublist, function(x) identical(names(x), first_col_names))
    return(unlist(test))
}

# http://stackoverflow.com/questions/12323693/is-there-a-more-elegant-way-to-convert-two-digit-years-to-four-digit-years-with
fix_year <- function(date_times, split_year) {
    extracted_years <- year(date_times)
    two_digits <- extracted_years %% 100
    correct_years <- ifelse(two_digits > split_year,
                            1900 + two_digits,
                            2000 + two_digits)
    year(date_times) <- correct_years
    
    return(date_times)
}

###############################################################################
## gather and arrange NOAA data

# specify the URL where the dataset pieces are available
source_url <- "http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# parse the page with rvest
source_doc <- html(source_url)

# the names of the dataset pieces - a whole bunch of separate, zipped .csv
# files - are located in a table in the page body; we use rvest to extract
# this table into an R data frame object
body_table <- source_doc %>% html_nodes("table") %>% .[[1]] %>% html_table()

# the Name column has the file names that we want to download along with
# some other junk - we just want the zipped file names; these can be 
# distinguished because they have ".gz" in them
file_name_index <- grepl(".gz", body_table$Name)
file_names <- body_table$Name[file_name_index]

# the files we want come in three flavors - details, fatalities, and locations
# we want to collect like files together first, which means we want to be able
# to identify each file with it's "flavor"
flavor_set <- c("details", "fatalities", "locations")

flavor_matches <- sapply(file_names, 
                         function(x) get_flavor_match(x, flavor_set))

# decide which type of import is currently appropriate - fresh download,
# simply importing current csvs, or importing initial .Rdata object
import_type <- "rds"

if(import_type == "full") {
    # there are three flavors of noaa data: details, fatalities, locations
    # we are going to download all the files and then read each flavor 
    # collection into its own list
    download_path_set <- get_noaa_data(source_url, file_names, flavor_matches)
} else if(import_type == "csv") {
    # alternative for if files already downloaded...
    download_path_set <- make_up_paths(flavor_matches)
} else if(import_type == "rds") {
    # alternative for if we already have completed the initial data grab,
    # import, and organization and saved it as an .rds file
    load("./initial_noaa_data.rds")
}

if(import_type == "full" | import_type == "csv") {
    # now we read in each file and collect it by its type, getting back a list
    # of the three types, each filled with their respective imports (note that
    # R allows us to read .gz files without explicit decompression)
    noaa_collection <- import_noaa_data(download_path_set, flavor_matches)
    
    # we want to combine the datasets within each sublist; we start by making 
    # sure they have the same columns
    lapply(noaa_collection, column_check)
    
    # they do, so we merge within each sublist
    noaa_collection <- lapply(noaa_collection, function(x) do.call(rbind, x))
}

# at this point, we essentially have a database with three tables that can be
# joined on the shared key "EVENT_ID"

# for our purpose, the "details" table can be thought of as the primary table
# of interest - every row the table is supposed to represent a unique storm
# event (the observation of a storm impacting a particular region); a storm
# itself may be part of multiple events and is identified by EPISODE_ID

# the "locations" table offers additional location variables for storms and
# storm events

# the "fatalities" table links individual deaths to storm events and provides
# additional information about the deaths

# key information: what makes a storm (EPISODE_ID)?
# The occurrence of storms and other significant weather phenomena having 
# sufficient intensity to cause loss of life, injuries, significant property 
# damage, and/or disruption to commerce.  Rare, unusual, weather phenomena that 
# generate media attention, such as snow flurries in South Florida or the San 
# Diego coastal area; and Other significant meteorological events, such as 
# record maximum or minimum temperatures or precipitation that occur in 
# connection with another event.

# storm event (EVENT_ID)?
# single, small part that goes into a specific storm episode

###############################################################################
## reviewing and cleaning

# as noted, the "details" table is central to our interests, so we'll start
# there..
details <- noaa_collection$details
nrow(details)

# the following "details" variables are central to our interests and so will get
# some immediate attention: storms (EPISODE_ID), storm events (EVENT_ID), event
# types (EVTYPE)

# we start with EVENT_ID since every row in the table is supposed to be a
# single storm event, making it the primary key of the table
# EVENT_ID
# - NAs?
summary(details$EVENT_ID)
# - what's up with the NAs? KINDA EMPTY/USELESS
na_index <- is.na(details$EVENT_ID)
na_rows <- details[na_index,]
# - are our observed IDs unique? LOOK LIKE IT - ONLY NON-UNIQUE ARE THE NAs
nrow(details)
length(unique(details$EVENT_ID))
nrow(details[!na_index, ])
length(unique(details$EVENT_ID[!na_index]))
# - how are EVENT_IDs ordered? i.e., sequentially such that we can infer event
#   order of occurence from the EVENT_ID? MAYBE ACROSS LIMITED CHUNKS OF TIME
#   BUT EVENT DATE/TIME IS PROBABLY A BETTER APPROACH
ggplot(details, aes(x = BEGIN_YEARMONTH, y = EVENT_ID)) + 
    geom_point() + 
    geom_line() +
    xlim(195000, 201500)
# - how are events distributed over time? # GRADUAL INCREASE OVER TIME BUT A
#   HUGE SPIKE IN 1996 WHEN THE NWS STANDARDS GO INTO EFFECT ON THE DATA;
#   WILL PROBABLY WANT TO REVISIT THIS CHART AFTER CLEANING EVENT TYPES TO
#   EXPLORE HOW THE MOST COMMON EVENT SUBSET IS DISTRIBUTED OVER TIME...; BUT
#   THIS DOES SUGGEST THAT INFERENCES ABOUT EVENTS PRIOR TO 1996 ARE RELATIVELY
#   LIMITED AND SUGGESTS 1996+ MAY BE A RICHER WINDOW TO STUDY (IT ALSO MAY BE
#   WHEN EPISODES START BEING TRACKED); STILL A GROWTH IN OBSERVED EVENTS AFTER
#   THIS WINDOW BUT UNCLEAR IF THAT IS RELATED TO IMPROVED REPORTING
details$bdt <- dmy_hms(details$BEGIN_DATE_TIME)
head(details$BEGIN_DATE_TIME)
head(details$bdt)
details$bdt <- fix_year(details$bdt, 49)
ggplot(details, aes(x = year(bdt))) +
    geom_histogram(binwidth = 1) +
    xlim(1949, 2016) +
    geom_vline(aes(xintercept = 1996))

# EPISODE_ID
# - NAs? OMG SO MANY
summary(details$EPISODE_ID)
# - how often are EPISODE_IDs repeated? (presumably because of multiple storm
#   events) # MOST EPISODE IDS ARE USED JUST A SINGLE TIME BUT THE RANGE IS
#   NOTABLE
nrow(details)
sum(!is.na(details$EPISODE_ID))
length(unique(details$EPISODE_ID))
episode_table <- table(details$EPISODE_ID)
head(episode_table)
summary(as.vector(episode_table))
sd(episode_table)
# - given the large number of NAs... when did we start seeing episode IDs used?
#   is it related to the 1996 standards? also, are they assigned sequentially
#   (time ~ ID)? LOOKS LIKE A 1996 THING, SO NOT REALLY USEFUL PRIOR... ALSO,
#   PROBABLY CAN'T RELY ON THEM TO BE SEQUENTIAL EXCEPT IN LIMITED WINDOWS
#   AND SEEMS LIKE THERE MAY BE RISK OF EPISODE_IDs BEING REUSED TO REFERENCE
#   DIFFERENT STORMS ENTIRELY (GIVEN OVERLAP IN VERTICAL RANGE)
ggplot(details, aes(x = BEGIN_YEARMONTH, y = EPISODE_ID)) +
    geom_point() +
    xlim(195000, 201500) +
    geom_vline(aes(xintercept = 199600))

# - any EPISODE_ID NAs after 1996? NOT MANY - MAYBE OUR SAME NAs FROM EARLIER
#   SHOULD REVISIT IF WE CLEAN OUT THOSE NAs AND SEE IF WE NEED TO CLEAN OUT
#   ANY MORE
index_1996 <- year(details$bdt) >= 1996
length(details$EPISODE_ID)
length(details$EPISODE_ID[index_1996])
summary(details$EPISODE_ID)
summary(details$EPISODE_ID[index_1996])
summary(details$EPISODE_ID[index_1996 & !na_index])

# - any signs that EPISODE_IDs have been reused incorrectly? (i.e., the same ID
#   seems to have been used for different storms) WE CAN SEE THAT THE EPISODE
#   IDS ARE NOT SEQUENTIAL NOR IS THERE A CONSISTENT PATTERN TO THE DIFFERENCES
#   AMONG EPISODE IDS...
unique_episodes <- unique(details$EPISODE_ID[index_1996 & !na_index])
length(unique_episodes)
length(episode_table[episode_table > 1])
length(episode_table[episode_table > 1]) / length(unique_episodes)
# unique_repeated <- episode_table[episode_table > 1]
# repeated_sequences <- lapply(names(unique_repeated), function(x) 
#     grep(paste0("^", x, "$"), details$EPISODE_ID[index_1996 & !na_index])
# )
# get_episode_diff <- function(episode_vector) {
#     diffs <- c()
#     
#     for(index in 2:length(episode_vector)) {
#         current_diff <- episode_vector[index] - episode_vector[index - 1]
#         diffs <- c(diffs, current_diff)
#     }
#     
#     diffs <- c(NA, diffs)
#     
#     return(diffs)
# }
# episode_na_index <- is.na(details$EPISODE_ID)
# natural_episode_sequence <- details$EPISODE_ID[!episode_na_index]
# offset_episode_sequence <- c(NA, natural_episode_sequence[-length(natural_episode_sequence)])
# head(natural_episode_sequence)
# head(offset_episode_sequence)
# natural_offset_contrast <- offset_episode_sequence - natural_episode_sequence
# head(natural_offset_contrast)
# length(table(natural_offset_contrast))
# 
# natural_episode_event_ids <- details$EVENT_ID[!episode_na_index]
# natural_collection <- data.frame(episode = natural_episode_sequence,
#                                  event   = natural_episode_event_ids)
# ordered_collection <- arrange(natural_collection, episode)

## MORE WORK HERE: NEED TO EXPLORE HOW TO MAKE THIS EFFICIENT - TAKES FOREV...

# So, at this point...
# - virtually all (except for our 3 random NAs) rows are properly represented
#   by an event ID
# - there was a large increase in total event observations in 1996, along with
#   enrichment of the data by the addition of episode IDs and a diversity of
#   of new event types... this increase in event types (possibly along with
#   improvements in the data gatherinr stream) are the likely best explanation
#   of the increase in observations (rather than, say, an actual increase in
#   storm events of such magnitude if any increase at all)

# It's time to inspect the EVENT_TYPE variable to assess what shape it's in.
# Understanding and cleaning EVENT_ID, EPISODE_ID, and EVENT_TYPE should put
# further exploration of the data on fairly firm foundation - it will allow us
# to explore differences among the event types with some confidence that our
# events are uniquely identified, give us a clear sense of the time periods
# our analyses have to be sensitive to, and allow us to (for 1996+) also 
# roll-up by episode if we like. Once we clean these core variables, we should
# be ready to start exploring our data from the perspective of particular
# questions - cleaning as needed.

# EVENT_TYPE
# - how many event types are observed in the data?
length(unique(details$EVENT_TYPE))
# - are any of them equivalent to NAs (not as obvious when character values)
#   and, if yes, how many? WE CAN SEE THERE IS ONE BLANK ("") VALUE. THERE IS
#   ALSO AN "Other" CATEGORY THAT MAY BE UNHELPFUL. THE BLANK VALUE OCCURS A
#   FAMILIAR NUMBER OF TIMES (3) AND COME FROM OUR NAs FROM ABOVE. ONLY 1 
#   "OTHER" VALUE OCCURS.
unique(details$EVENT_TYPE)
table(details$EVENT_TYPE)
table(details$EVENT_TYPE[!na_index])
# - how many of the event types are used very often (i.e., definitely don't
#   seem like one-time mistakes)? 59/72 MORE THAN ONCE, 52/72 MORE THAN FIVE
#   TIMES... NOT MANY MORE THAN THE 48 OFFICIAL EVENT TYPES, WHICH IS PRETTY
#   SOLID GIVEN THAT THERE ARE MORE THAN 1M EVENTS IN THE TABLE
length(unique(details$EVENT_TYPE))
code_freq_table <- sort(table(details$EVENT_TYPE), decreasing = TRUE)
head(code_freq_table)
more_than_one <- sum(code_freq_table > 1)/length(code_freq_table)
more_than_one * length(unique(details$EVENT_TYPE))
more_than_five <- sum(code_freq_table > 5)/length(code_freq_table)
more_than_five * length(unique(details$EVENT_TYPE))

# first cleaning: dropping our strange event/episode NAs... DROPPING THREE
# WEIRD EVENTS GETS READ OF ALL EVENT_ID NAs and ALL EPISODE_ID NAs AFTER
# 1996; IT ALSO CLEARS OUT ONE OF THE NON-COMPLIANT EVENT_TYPEs (THE BLANK
# ONE)
details <- details[!na_index, ]
summary(details$EVENT_ID)
summary(details$EPISODE_ID[index_1996[!na_index]])
length(unique(details$EVENT_TYPE))

# exploring the
# non-matching event type names to see if they should/can be converted to
# valid names or if there are truly unique event types being captured that
# should be represented even if they fall outside of the official set

# define the official event names with consistent formatting
official_event_names <- c(
    "astronomical low tide", 
    "avalanche",
    "blizzard",
    "coastal flood",
    "cold/wind chill",
    "debris flow",
    "dense fog",
    "dense smoke",
    "drought",
    "dust devil",
    "dust storm",
    "excessive heat",
    "extreme cold/wind chill",
    "flash flood",
    "flood",
    "frost/freeze",
    "funnel cloud",
    "freezing fog",
    "hail",
    "heat",
    "heavy rain",
    "heavy snow",
    "high surf",
    "high wind",
    "hurricane (typhoon)",
    "ice storm",
    "lake-effect snow",
    "lakeshore flood",
    "lightning",
    "marine hail",
    "marine high wind",
    "marine strong wind",
    "marine thunderstorm wind",
    "rip current",
    "seiche",
    "sleet",
    "storm surge/tide",
    "strong wind",
    "thunderstorm wind",
    "tornado",
    "tropical depression",
    "tropical storm",
    "tsunami",
    "volcanic ash",
    "waterspout",
    "wildfire",
    "winter storm",
    "winter weather"
)

# a silly reason why we may have unique events may be simple differences in
# case (e.g., R will treat "Thunderstorms" as unique from "thunderstorms"); we
# resolve this and make our EVENT_TYPEs compliant with our official set
# formatting (all lower case)
details$EVENT_TYPE <- tolower(details$EVENT_TYPE)
length(unique(details$EVENT_TYPE))

# now we do our first couple of checks:
# - how many of our official events types occur at all? ALL OF THEM OCCUR AT
#   LEAST ONCE
# - what events types remain once we account for the official event types and
#   how frequent are these unofficial types? THE TYPES THAT REMAIN APPEAR TO 
#   MOSTLY BE COMBINATIONS OF EVENT TYPES - SOMETHING NOT ALLOWED IN THE EVENT
#   TYPE RULES; FORTUNATELY, THE NUMBER OF UNOFFICIAL EVENT TYPES SEEMS TO BE
#   NEAR 0 EXCEPT FOR "landslide" AND ONLY A HANDFUL - SUCH AS "landslide"
#   APPEAR TO ACTUALLY BE UNIQUE
length(official_event_names)
sum(official_event_names %in% unique(details$EVENT_TYPE))
length(unique(details$EVENT_TYPE))
sum(unique(details$EVENT_TYPE) %in% official_event_names)
unofficial_type_index <- !(unique(details$EVENT_TYPE) %in% official_event_names)
unofficial_types <- unique(details$EVENT_TYPE)[unofficial_type_index]
unofficial_types
unofficial_row_index <- details$EVENT_TYPE %in% unofficial_types
sort(table(details$EVENT_TYPE[unofficial_row_index]), decreasing = TRUE)

# the easiest unofficial type to fix are the thunderstorm wind hybrids - we
# can simply find all the event names that start with "thunderstorm wind" and
# then have other stuff and flip these to thunderstorm (respecting the fact that
# only a single event type is allowed and thunderstorm wind is stated first)
unofficial_types[grepl("^thunderstorm\\swind.", unofficial_types)]
thunderstorm_correction_index <- grepl("^thunderstorm\\swind.", 
                                       details$EVENT_TYPE)
details$EVENT_TYPE[thunderstorm_correction_index] <- "thunderstorm wind"

# quick review of what unofficial types are left... NOT TOO MANY! ONE BY ONE
# REVIEW SEEMS REASONABLE
unofficial_type_index <- !(unique(details$EVENT_TYPE) %in% official_event_names)
unofficial_types <- unique(details$EVENT_TYPE)[unofficial_type_index]
unofficial_types

# for each remaining unofficial type, we suggest a replacement; then we loop
# over the unofficial type and replacement lists, correcting matches to a type
# with the replacement name; where there is a hybrid, we use the first event
# type; there are a few exceptions to this process... landslides look legit,
# northern lights look legit (if silly in context), "other" is garbage,
# not sure what to do with sneakerwave (could be a few things:
# http://en.wikipedia.org/wiki/Sneaker_wave), 
type_corrections <- c("tornado", "hail", "hail", "tornado", "heavy snow", 
                      "high wind", NA, "northern lights", "landslide", NA,
                      "hurricane (typhoon)", "dense fog", "tropical storm",
                      "lightning")
for(index in 1:length(unofficial_types)) {
    current_regex <- paste0("^", unofficial_types[index], "$")
    correction_index <- grepl(current_regex, details$EVENT_TYPE)
    details$EVENT_TYPE[correction_index] <- type_corrections[index]
}

# quick review of what unofficial types are left... JUST THE TWO WE DECIDED TO
# KEEP PLUS A SILLY NA
length(unique(details$EVENT_TYPE))
unofficial_type_index <- !(unique(details$EVENT_TYPE) %in% official_event_names)
unofficial_types <- unique(details$EVENT_TYPE)[unofficial_type_index]
unofficial_types

# drop our new NAs
clean_index <- !is.na(details$EVENT_TYPE)
details <- details[clean_index, ]

# now that we have official (or reasonable unofficial) event types... EVENT_TYPE
# seems fair to treat as "clean"; we already did some treatment of EVENT_ID and
# EPISODE_ID (dropped rows missing an EVENT_ID and verified that all 1996+
# rows have EPISODE_IDs); the only things to verify are that (a) EVENT_ID has
# no duplicates and (b) there are no signs that EPISODE_ID has been re-used
sum(duplicated(details$EVENT_ID))

# now we check EPISODE_ID... ~40% OF EPISODE IDs REPEAT
length(details$EPISODE_ID)
length(unique(details$EPISODE_ID))
episode_table <- table(details$EPISODE_ID)
length(episode_table[episode_table > 1])
length(episode_table[episode_table > 1])/length(episode_table)

# grab the collection of EPISODE_IDs that repeat
repeating_eps <- episode_table[episode_table > 1]

system.time({
    repeat_candidates <- c()
    
    for(index in 1:100) {
        current_episode <- names(repeating_eps)[index]
        
        matching_index <- details$EPISODE_ID %in% current_episode
        
        matching_starts <- details$BEGIN_YEARMONTH[matching_index]
        
        unusual_starts <- length(unique(matching_starts)) > 1
        
        if(unusual_starts) {
            repeat_candidates <- c(repeat_candidates, current_episode)
        }
    }
})
length(repeating_eps)

###############################################################################
## old material


# we start off with some simple summary statistics: ignoring event type, what
# are typical fatality rates per weather event?
summary(storms$FATALITIES)
sd(storms$FATALITIES)

# injury rates?
summary(storms$INJURIES)
sd(storms$INJURIES)

# first we add two new factors to our dataset to group our events by number
# of fatalities or injuries
storms$FAT_FACTOR <- sapply(storms$FATALITIES, function(x) 
    if(x == 0) {
        return("None")
    } else if (x > 5) {
        return("Five or More")
    } else {
        return("One to Five")
    }
)

storms$INJ_FACTOR <- sapply(storms$INJURIES, function(x) 
    if(x == 0) {
        return("None")
    } else if (x > 5) {
        return("Five or More")
    } else {
        return("One to Five")
    }
)

# now we turn these into proper, ordered factors
storms$FAT_FACTOR <- factor(storms$FAT_FACTOR, 
                            levels = c("None", "One to Five", "Five or More"),
                            labels = c("None", "One to Five", "Five or More")
)

storms$INJ_FACTOR <- factor(storms$INJ_FACTOR, 
                            levels = c("None", "One to Five", "Five or More"),
                            labels = c("None", "One to Five", "Five or More")
)

# we make a simplified dataset and melt/tweak it for nice plotting
fi_counts <- storms[c("FAT_FACTOR", "INJ_FACTOR")]

fi_counts <- gather(fi_counts, key = "Harm_Type", value = "Harm_Volume")
fi_counts$Harm_Type <- factor(fi_counts$Harm_Type, 
                              levels = c("FAT_FACTOR", "INJ_FACTOR"),
                              labels = c("Fatalities", "Injuries")
)
fi_counts$Harm_Volume <- factor(fi_counts$Harm_Volume,
                                levels = c("None", "One to Five", 
                                           "Five or More"),
                                labels = c("None", "One to Five", 
                                           "Five or More")
)

# now we check out the results in graphical form
ggplot(fi_counts, aes(x = Harm_Volume, fill = Harm_Type)) + 
    geom_bar(position = "dodge") +
    ylab("Frequency") +
    xlab("Number of Persons Experiencing Harm") +
    scale_fill_discrete(name = "Type of Harm")

# and tabular form (counts)
table(fi_counts)

storms$EVTYPE <- factor(storms$EVTYPE, 
                        levels = unique(storms$EVTYPE),
                        labels = unique(storms$EVTYPE))
most_inj_overall <- sort(xtabs(INJURIES ~ EVTYPE, data = storms), 
                         decreasing = TRUE)
most_fat_overall <- sort(xtabs(FATALITIES ~ EVTYPE, data = storms), 
                         decreasing = TRUE)

# largest single sources of injuries?
head(most_inj_overall)

# of fatalities
head(most_fat_overall)

# compliant symbols
compliant_symbols <- c("K", "M", "B")

# correct the most obvious cause for mismatch (case)
storms$PROPDMGEXP <- toupper(storms$PROPDMGEXP)
storms$CROPDMGEXP <- toupper(storms$CROPDMGEXP)

# assess compliance
sum(storms$PROPDMGEXP %in% compliant_symbols) / length(storms$PROPDMGEXP)
sum(storms$CROPDMGEXP %in% compliant_symbols) / length(storms$CROPDMGEXP)

###############################################################################
## END OF SCRIPT
###############################################################################