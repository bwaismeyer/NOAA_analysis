# Using the NOAA Storm Database to Explore the Impact of Weather Events on US Public Health and Economy
Brian Waismeyer  
Thursday, April 23, 2015  
## Where I'm At...
* Free-flow analysis work
* Rewrite openinging sections
* Transfer and add narrative to key sections as complete them in free-flow
* Replace outline with TOC

## Outline

1. Introduction
    * Context
    * Objectives
    * Constraints
2. Primary Data Processing
    * Gathering and Arranging
    * Cleaning
3. Analytic Processing
    * Objective
    * Objective
4. Results
    * Objective
    * Objective
5. Summary

## Synopsis
This is an exploratory analysis of an open database provided by the the U.S. 
National Oceanic and Atmospheric Administration (NOAA). It is a re-envisioning
and expansion of work I did to fulfill requirements of a course on 
[Reproducible Research](https://www.coursera.org/course/repdata) on Coursera.

The database used contains data about adverse weather events in the United 
States along with estimates of how these storms impacted the human environment. 
More information about the database can be found 
[here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

I completed this analysis to satisfy both my personal curiousity and to gain
experience exploring and buildings models from large, complex data.

My overarching analytic objective was pretty simple: Develop an understanding
of the risks to public health and economy posed by adverse weather events.

I broke this objective down as follows.

1. The dataset has some messy and inconsistent coding issues. I want to capture
   where and how much error is present, correct what I can, and understand what
   can't be corrected.
2. Describe and visualize the patterns of event occurrence.
3. Describe and visualize the broad, overall relationship between adverse 
   weather events and assessed public health outcomes.
4. Describe and visualize the broad, overall relationship between adverse 
   weather events and assessed economic cost.

This analysis will conducted in alignment with reproducible data principles. All
stages of the analysis - from dataset processing to analysis - will be conveyed
as clearly as possible.

## Key Dataset Limitations
There are notable limits on what events were recorded up to the start of 1996:
http://www.ncdc.noaa.gov/stormevents/details.jsp. Certain events will be
overrepresented purely because they have been documented for a much larger
window of time.

## Load Supporting Resources
This are R packages selected for my work or scripts containing functions I
wrote to support this project. Any custom scripts will be available at the
[GitHub repository for this project](https://github.com/bwaismeyer/NOAA_analysis).

## Data Processing


### Getting the Data

### Reviewing the Data
We take a quick look at the data structure, both to understand it and to insure
it loaded correctly.

### Tidying the Data
Our main interest is going to be the event type variable and
how it relates to public health and economic variables. So, we start by making
it a proper factor. This ends up requiring a few steps due to inconsistent
formatting.

We discover several cases where there are either leading/trailing whitespace, where there are multiple versions of the
same string but with different capitalization, where numbers have been added, etc.

The NOAA documentation provides no reason to believe differences in 
capitalization or whitespace are meaningful and does not permit name variations.
We attempt to clean out obvious incorrect features.

The NOAA documentation lists 48 unique event types. We have **var**.

To figure out what is going on, we can inspect how often specific codes are
actually used in the data.

It looks like only a small proportion of the current names were used very often. 
A proper cleaning would require digging through the list and identifying 
mispellings, alternate arrangements, etc. 

For the purposes of this analysis, we implemented a very simple cleaning:

* Contrast existing event names against the official NOAA event names.
* Check to see if there are any common patterns in the non-compliant event
  names that can be corrected via simple subsitutions (e.g., "tstm" being used
  as shorthand for "thunderstorm").
* Convert any events that have a partial match to an official name to an 
  official name. Events will be matched in the order they occur in our official
  name list, which has been arranged from most to least complex event name 
  (e.g., if an event was named "flood/flash flood" it would be converted to the 
  official name "flood"; if an event is named "thunderstorm wind/hail" it will 
  match "thunderstorm wind" as that event name is higher in our sorted list).
* Any remaining events with non-compliant names are retained but re-named 
  "non-compliant" so that we can assess whether important events are hiding
  in the mess.

We improved event name compliance from **var** to 
**var**. The post cleaning compliance is acceptable.

## Results
This section will present analyses attemping to address our two key questions.

### What types of storm events are most harmful to US population health?
For this analysis, we restricted "public health" to measurements of immediate
harm to persons. So, for example, fatality and injury data were considered
variables of interest but crop and property damage (which could have longer
term consequences for public health) were not.

We address the question first by simply exploring frequencies of fatalities
(**var**) and injuries (**var**) overall.

Given that most storms appear to have 0 injuries or fatalities, we take a 
simple approach to grouping our events to get a clear take on how often
fatalities and injuries occur at all

We can see that, overall, injuries and fatalities are quite rare during tracked
storm events.

Still, our question is focused on **which** storm event has the largest impact on
public health. We can answer this in three ways.

1. __Which type of storm event has the highest number of fatalities/injuries?__
   This confounds event type frequency and event type danger but is probably
   more useful for budgeting for future, unknown event periods.

2. __Which type of storm event has the highest rate of fatalities/injuries?__ 
   This tells which event is type is historically associated with the most
   harm when it occurs. This would be useful for choosing how to allocate
   resources among concurrent events where measurements of harm have not     
   yet been collected or for calculating worst-case scenario allocation.
   
Although an important question, it was not addressed for the time being in this
analysis.

### What types of events have the most impact on US economy?
For this analysis, we focus on the direct estimates of cost (**var** and 
**var**) included in the dataset. It is unclear if the measures have been 
adjusted for inflation. We will ignoreinflation for this analysis.

Per NOAA documentation, the cost estimates were recorded as follows:

> Estimates should be rounded to three significant digits, followed by an 
  alphabetical character signifying the magnitude of the number, i.e., 1.55B for
  $1,550,000,000. Alphabetical characters used to signify magnitude include “K” 
  for thousands, “M” for millions, and “B” for billions. 

So we first want to clean our magnitude variables and then combine these with
our cost estimate variables to get the actual cost amounts.

Compliance is relatively poor for both property and crop magnitude variables.
Due to time considerations, this analysis simply excludes any non-compliant
cases from the next part of the analysis.
