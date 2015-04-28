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
#
# - old material (repurpose or drop)

###############################################################################
## load libraries

library(rvest)      # getting the links to the source data files
library(stringr)    # for cleaning EVTYPE character strings
library(dplyr)      # for dataset reformatting
library(tidyr)      # for dataset reformatting
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

# there are three flavors of noaa data: details, fatalities, locations
# we are going to download all the files and then read each flavor collection
# into its own list
download_path_set <- get_noaa_data(source_url, file_names, flavor_matches)

# alternative for if files already downloaded...
download_path_set <- make_up_paths(flavor_matches)

# now we read in each file and collect it by its type, getting back a list
# of the three types, each filled with their respective imports (note that
# R allows us to read .gz files without explicit decompression)
noaa_collection <- import_noaa_data(download_path_set, flavor_matches)

# we want to combine the datasets within each sublist; we start by making sure
# they have the same columns
lapply(noaa_collection, column_check)

# they do, so we merge within each sublist
noaa_collection <- lapply(noaa_collection, function(x) do.call(rbind, x))

## I AM HERE

###############################################################################
## old material

# make capitalization conistent (all lowercase)
storms$EVTYPE <- tolower(storms$EVTYPE)

# drop any non-letter symbols - replacing with a single space to avoid 
# collapsing any words together if a symbol was being used as a separator
storms$EVTYPE <- gsub("[^a-zA-Z]", " ", storms$EVTYPE)

# remove leading/trailing whitespace
storms$EVTYPE <- str_trim(storms$EVTYPE)

# remove any extra spaces between words (no more than 1 space between words
# allowed)
storms$EVTYPE <- gsub(" +", " ", storms$EVTYPE)

# check unique strings
ev_length <- length(unique(storms$EVTYPE))
ev_length

# make a table of how often all event codes were used, sorted most to least
code_freq_table <- sort(table(storms$EVTYPE), decreasing = TRUE)
head(code_freq_table)

# what proportion of codes are used more than once?
more_than_one <- sum(code_freq_table > 1)/length(code_freq_table)
more_than_one

# more than 5 times?
more_than_five <- sum(code_freq_table > 5)/length(code_freq_table)
more_than_five

# define the event names permitted in NOAA documentation
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

# apply the same corrections to these names as we did to the event names
# above (since some of these have symbols in them)
official_event_names <- tolower(official_event_names)
official_event_names <- gsub("[^a-zA-Z]", " ", official_event_names)
official_event_names <- str_trim(official_event_names)
official_event_names <- gsub(" +", " ", official_event_names)

# sort the official names from most to least complex (based on name length)
official_event_names <- data.frame(names = official_event_names,
                                   complexity = nchar(official_event_names),
                                   stringsAsFactors = FALSE)
official_event_names <- arrange(official_event_names, desc(complexity))

# assess what proportion of event names are an exact match with an official 
# event name
compliance_before_clean <- sum(storms$EVTYPE %in% official_event_names$names) /
    length(storms$EVTYPE)
compliance_before_clean

# assess if there are any patterns to the non-compliant names
non_compliant_index <- storms$EVTYPE %in% official_event_names$names
non_compliant_events <- storms$EVTYPE[non_compliant_index]
non_compliant_summary <- sort(table(non_compliant_events), decreasing = TRUE)

head(non_compliant_summary, 20)

# our inspection raises a common set of recurring issues that can be fixeed
# via string subsitution; we define a list of fixes to make
# NOTE: we only need to include fixes that will prevent our PARTIAL
# match appr
sub_list <- list(
    # most common alternate spelling issues
    list(pattern = "tstm", replacement = "thunderstorm", 
         fixed = TRUE),
    list(pattern = "flooding", replacement = "flood", 
         fixed = TRUE),
    # a touch trickier - a common error is to make a name plural (e.g., wind to 
    # winds and cloud to clouds); conveniently, NONE of the official names are
    # plural and only "debris flow" has an "s" at the end of any word... more
    # conveniently, "debris" occurs only once in our data and is used
    # incorrectly... so we simply drop any "s" that occurs at the end of a word
    list(pattern = "s\\b", replacement = "", 
         fixed = FALSE),
    # make explicit changes to commonly occuring categories that are either
    # incorrect or which are combinations (documentation states that only name
    # can be assigned); the first combination member was retained
    list(pattern = "urban/sml stream fld", replacement = "flood", 
         fixed = TRUE),
    list(pattern = "wild/forest fire", replacement = "wildfire", 
         fixed = TRUE),
    list(pattern = "winter weather/mix", replacement = "winter weather", 
         fixed = TRUE),
    list(pattern = "thunderstorm wind/hail", replacement = "thunderstorm wind", 
         fixed = TRUE),
    list(pattern = "^extreme cold$", replacement = "extreme cold wind chill", 
         fixed = FALSE),
    list(pattern = "flood/flash flood", replacement = "flash flood", 
         fixed = TRUE),
    list(pattern = "urban flood", replacement = "flood", 
         fixed = TRUE),
    list(pattern = "^storm surge$", replacement = "storm surge tide", 
         fixed = FALSE),
    list(pattern = "^hurricane$", replacement = "hurricane typhoon", 
         fixed = FALSE)
)

# now we loop over our sub_list with gsub to implement the string substitutions
for(index in 1:length(sub_list)) {
    current_fix <- sub_list[[index]]
    storms$EVTYPE <- gsub(current_fix$pattern, 
                          current_fix$replacement, 
                          storms$EVTYPE,
                          fixed = current_fix$fixed)
}

rm(index, current_fix)

# assess our rate of compliance again
compliance_after_clean <- sum(storms$EVTYPE %in% official_event_names$names) /
    length(storms$EVTYPE)
compliance_after_clean

# now we duplicate our EVTYPES column (in case we want to recover any more
# non-compliant names later)
storms$EVTYPE_NC_PREFIX <- storms$EVTYPE

# any remaining non-compliant names are now dropped and replaced with "non-
# compliant"
storms$EVTYPE[!(storms$EVTYPE %in% official_event_names$names)] <- 
    "non-compliant"

# finally, we verify that we now only have official event names in our data
# (plus the new "non-compliant" event type and minus "debris flow" which
# never occurs in our data)
length(unique(storms$EVTYPE))

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