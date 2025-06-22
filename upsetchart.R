# install.packages("rJava")
# install.packages("UpSetR")
# install.packages("tidyverse")
# install.packages("venneuler")
# install.packages("grid")



library(dplyr)
library(rJava)
library(UpSetR)
library(tidyverse)
library(venneuler)
library(grid)



rawSets <- read.csv(
  file = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/sets/seniorTransportation.csv",
  header = TRUE, sep = ",", stringsAsFactors = FALSE
)




rawSets[is.na(rawSets)] <- 0

# Rename the columns for easier display
sets <- rawSets %>%
  rename(TTC = ttcTransportation, Walk = walkTransportation, Drive = driveTransportation, Cycle = cycleTransportation, Taxi = taxiTransportation, `Community Ride` = communityRideTransportation, `Wheel Trans` = wheelTransTransportation, Friends = friendsTransportation)


# Prep the data for a Venn diagram
vennSets <- sets %>%
  gather(transportation, binary,6:13) %>% # take all binary mappings and convert to be a the set indicator
  filter(binary == 1) %>% # only include set matches
  select(ID, transportation) %>% # only include ID and set category
  mutate(transportation = factor(transportation)) # set the transportation column as a factor


v <- venneuler(data.frame(vennSets))

#Note that if you need to move around the labels so that they are not overlapping, you can use the new line breaks like the example below.
# v$labels <- c("TTC", "Walk", "Drive", "Cycle\n\n\n", "\nTaxi", "Community Ride", "Wheel Trans", "Friends")

par(cex = 0.7) 
plot(v, main = "Modes of Senior Transportation (Toronto 2017 Survey)", cex.main = 1.5)
grid.text(
  "@littlemissdata",
  x = 0.52,
  y = 0.15,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

Little Miss Data


Set Analysis: A face off between Venn diagrams and UpSet plots
APR
30
Set Analysis: A face off between Venn diagrams and UpSet plots
DATASCIENCE, R, VISUALIZATION, TUTORIAL, STEAM, 2019
Venn Diagrams and UpSet Plots 

It’s time for me to come clean about something;  I think Venn diagrams are fun!  Yes that’s right, I like them.  They’re pretty, they’re often funny, and they convey the straight forward overlap between one or two sets somewhat easily.  Because I like making nerd comedy graphs, I considered sharing with y’all how to create Venn diagrams in R.  But I couldn’t do that in good conscience without showing an alternative for larger and more complex set analysis.  A few weeks ago, when I saw Matthew Hendrickson and Mara Averick’s excitement over the UpSetR plot, I knew what I should do.

Folks, what you are about to witness is a set analysis face off!  We will be pairing off Venn diagrams and UpSet plots in a variety of scenarios for a true battle royale.  Winner takes all and is able to claim the prize of set analysis master.  

Venn Diagrams and Upset plots in R
Working Environment

For this tutorial, we are going to be using R as our programming language.   The entire code is hosted in my github repo, and you can also copy and paste to follow along below.  If you are looking to understand your options for an R working environment, I recommend that you can check out IBM Watson Studio to run hosted R notebooks, or RStudio.  
Round 1: Tiny and Fun Set Intersections

Kind folks, this is our warm up.  In this round, we will be creating some fun and simple set intersections.  Specifically, we will just be creating a very important graph which describes why I love Twitter.

To get started, we are going to install and load the packages required for this tutorial.  If you do not already have the packages installed, please uncomment the install.packages() commands by removing the hashtag(#).
  
  Install and Load Packages
  # install.packages("rJava")
  # install.packages("UpSetR")
  # install.packages("tidyverse")
  # install.packages("venneuler")
  # install.packages("grid")
  
  library(rJava)
  library(UpSetR)
  library(tidyverse)
  library(venneuler)
  library(grid)
  Format the data
  
  We will create a basic list which specifies the values of each of the circles and their overlap.  
  # Set the chart data
  expressionInput <- c(`#rstats` = 5, memes = 5, `#rstats&memes` = 3)
  Create a Venn diagram
  
  To create a simple Venn diagram, you can just pass in the list with the specified set and overlap values into the venneuler() function.  The remaining code is just formatting to set the font size, title and subtitle.
  Venn Diagram using vennular in R
  # Create the Venn diagram
  # note on set up for java v11 jdk (v12 does not work with this)
  myExpVenn <- venneuler(expressionInput)
  par(cex=1.2)
  plot(myExpVenn, main = "Why I Love Twitter")
  grid.text(
    "@littlemissdata",
    x = 0.52,
    y = 0.2,
    gp = gpar(
      fontsize = 10,
      fontface = 3
    )
  )
  Create an UpSet Plot
  
  The great thing is that we can also create an UpSet plot using the same basic expression list.  You simply pass the fromExpression() function into the upset() function.  The remaining code is to format the labels and font size.
  
  How to read an UpSet plot:  UpSet plots offer a straight forward way for us to view set data by frequency.  On the bottom left hand side horizontal bar chart, we show the entire size of each set.  In this case, each set is of size 8.  The vertical bar chart on the upper right hand side shows the sizes of isolated set participation.  In the example, 5 values only belong to the #rstats set or only belong to the memes set.  3 values belong to both sets.  
  Set Analysis, Upset Diagram using upsetR in R
  # Create an UpsetR Plot
  upset(fromExpression(expressionInput), order.by = "freq")
  grid.text(
    "Why I Love Twitter  @littlemissdata",
    x = 0.80,
    y = 0.05,
    gp = gpar(
      fontsize = 10,
      fontface = 3
    )
  )
  While the UpSet graph is an exciting new addition to our set analysis, I’m going to have to give this round to Venn diagrams.  When trying to represent simple and easy to understand information, Venn diagrams are more visually appealing.
  Round 1.png
  Round 2: Complicated Sets
  Coming off of the round 1 win, Venn diagram may be feeling quite confident.  However, the stakes are getting higher and we need to expect more of our visualizations in this round.  We have more sets and interactions to visualize and more data to work with.
  
  Data Introduction
  
  The data is created using the 2017 Toronto Senior Survey from the Toronto Open Data Catalogue.  I feel proud that my current city (Austin) and my previous city (Toronto) both have high quality open data catalogs.  I feel strongly that data should be available to the people that pay for it. 
  
  This data set shows the output of a 2017 senior citizen survey to identify various needs of Toronto's seniors' population, in order to better inform decision making.  To make our data processing easier, I have stripped down the columns that we will use and have performed a little pre-formatting.  Please see below for a data dictionary and outline of what was changed. 
  Column	Source Column
  ID	Not previously included. This is a new unique key column.
  physicalActivity	Survey Question: "1. In the past 3 months, how often did you participate in physical activities like walking?"
  physicalActivityPerMonth	Survey Question: "1. In the past 3 months, how often did you participate in physical activities like walking?". This has been transformed into numerical format.
  volunteerParticipation	Survey Question: "5. During the past 3 months, how often did you participate in volunteer or charity work?"
  volunteerPerMonth	Survey Question: "5. During the past 3 months, how often did you participate in volunteer or charity work?". This has been transformed into numerical format.
  difficultFinancial	Survey Question: "9. In the last year, have you had difficulty paying your rent, mortgage, Hydro bill, or other housing costs? For example, have you had to go without groceries to pay for rent or other monthly housing expenses?"
  supportSystem	Survey Question: "13. Do you have people in your life who you can call on for help if you need it?"
  postalCode	"Survey Question: 14. What are the first three characters of your postal code?"
  employmentStatus	Survey Question: "15. What is your current employment status?"
  sex	Survey Question: "16. What is your sex/gender?"
  primaryLanguage	Survey Question: "18. In what language(s) would you feel most comfortable to receive services?" (first option listed)
  ageRange	Survey Question: "19. Which age category do you belong to?"
  ttcTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [TTC (bus, subway, or streetcar)]"
  walkTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [Walk]"
  driveTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [Drive]"
  cycleTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [Cycle]"
  taxiTransportation	Survey Question: " 6. To get around Toronto, what modes of transportation do you use frequently? [Taxi or Uber]"
  communityRideTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [Community Transportation Program, for example Toronto Ride or iRIDE]"
  wheelTransTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [Wheel-Trans]"
  friendsTransportation	Survey Question: "6. To get around Toronto, what modes of transportation do you use frequently? [Rides from family, friends or neighbours]"
  ageRange	Survey Question: "19. Which age category do you belong to?".
  minAgeRange	Survey Question: "19. Which age category do you belong to?". This has been converted to numerical format, taking the lowest age as the value.
  Bring in the Data
  
  We will start by bringing in the data, replacing the NA’s and renaming the columns for easier display.
  rawSets <- read.csv(
    file = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/sets/seniorTransportation.csv",
    header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  
  # Replace the NA's
  
  rawSets[is.na(rawSets)] <- 0
  
  # Rename the columns for easier display
  sets <- rawSets %>%
    rename(TTC = ttcTransportation, Walk = walkTransportation, Drive = driveTransportation, Cycle = cycleTransportation, Taxi = taxiTransportation, `Community Ride` = communityRideTransportation, `Wheel Trans` = wheelTransTransportation, Friends = friendsTransportation)
  
  dim(sets)
  head(sets)
  The data comes with the sets in the form of a binary matrix.
  The data comes with the sets in the form of a binary matrix.
  
  Create a Venn Diagram
  
  Now it’s time to create our Venn diagram.  The data is currently in the form of a binary matrix, but to pass it into the venneuler() function, we need to get it into a list of set, ID pairs.  
  # Prep the data for a Venn diagram
  vennSets <- sets %>%
    gather(transportation, binary,6:13) %>% # take all binary mappings and convert to be a the set indicator
    filter(binary == 1) %>% # only include set matches
    select(ID, transportation) %>% # only include ID and set category
    mutate(transportation = factor(transportation)) # set the transportation column as a factor
  
  dim(vennSets)
  The data has been transformed to have one set column and one ID column. An ID can be repeated for every set it belongs to.
  The data has been transformed to have one set column and one ID column. An ID can be repeated for every set it belongs to.
  
  Create the Venn diagram by passing the data frame into the venneuler() function.  The rest of the code is for labelling and formatting.  
  Venn Diagram using vennular in R
  v <- venneuler(data.frame(vennSets))
  
  #Note that if you need to move around the labels so that they are not overlapping, you can use the new line breaks like the example below.
  #v$labels <- c("TTC", "Walk", "Drive", "Cycle\n\n\n", "\nTaxi", "Community Ride", "Wheel Trans", "Friends")
  
  par(cex = 0.7) 
  plot(v, main = "Modes of Senior Transportation (Toronto 2017 Survey)", cex.main = 1.5)
  grid.text(
    "@littlemissdata",
    x = 0.52,
    y = 0.15,
    gp = gpar(
      fontsize = 10,
      fontface = 3
    )
  )
  
  # 
  # Create an UpSet Plot
  # 
  # Create an UpSet plot by passing the original binary matrix into the upset() function.  You can specify a number of parameters as outlined by this very clear vignette, but it also works very well outside of the box.  Other than the upset() function, the rest of the code is for labels and formatting. 
  # 

  
  upset(sets,
        nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,
        mainbar.y.label = "Modes of Senior Transportation (Toronto 2017 Survey)", sets.x.label = "Total Participants"
  )
  grid.text(
    "datascientist_@outlook.com",
    x = 0.90,
    y = 0.02,
    gp = gpar(
      fontsize = 7,
      fontface = 3
    )
  )
  
  
  
  
  # -------- Highlight 
  
  upset(sets,
        query.legend = "bottom", nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,
        mainbar.y.label = "Modes of Senior Transportation (Toronto 2017 Survey)", sets.x.label = "Total Participants", 
        queries = list(
          list(
            query = intersects,
            params = list("Cycle", "Walk"), 
            color = "#Df5286", 
            active = T,
            query.name = "Physically Active Transportation"
          )
        )
  )
  grid.text(
    "datascientist_@outlook.com",
    x = 0.90,
    y = 0.05,
    gp = gpar(
      fontsize = 7,
      fontface = 1.5
    )
  )



