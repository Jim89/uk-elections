###--------------------------------------------------
### Scraping 2015 UK election data from the BBC
###--------------------------------------------------

# set up working environment ---------------------------------------------------
library(ggplot2)
library(scales)
library(dplyr)
library(rvest)
library(stringr)
library(car)

library(rgeos)
library(maptools)
library(rgdal)
library(leaflet)


# create functions to be used --------------------------------------------------

# Convenience "not in" function
  "%nin%" <- function(x, y) {
    return( !(x %in% y) )
  }

# get the results
get.constituency.results <- function(constituency){

  name <- constituency %>%
          html_nodes(".constituency-title__title") %>%
          html_text()

  parties <- constituency %>%
             html_nodes(".party__name--long") %>%
             html_text()

  candidates <- constituency %>%
                html_nodes(".party__result--candidate") %>%
                html_text() %>%
                str_replace(", with candidate ", "")

  votes <- constituency %>%
           html_nodes(".party__result--votes") %>%
           html_text() %>%
           str_replace(",", "") %>%
           str_replace(" total votes taken.", "") %>%
           as.numeric()

  vote.share <- constituency %>%
                html_nodes(".party__result--votesshare") %>%
                html_text() %>%
                str_replace("% share of the total vote", "") %>%
                as.numeric()

  pct.swing <- constituency %>%
               html_nodes(".party__result--votesnet") %>%
               html_text() %>%
               str_replace("\\+", "") %>%
               str_replace("% change in share of the votes", "") %>%
              as.numeric()

  results.df <- data_frame(Constituency = name,
                           Party = parties,
                           Candidate = candidates,
                           Votes = votes,
                           Vote.Share = vote.share,
                           Swing = pct.swing
                           )
  return(results.df)
}


# get the data -----------------------------------------------------------------
#  ff we've done all this already, don't do it again
if ( !file.exists("data/constituency_names.csv")) {

    constituencies <- read_html("http://www.bbc.com/news/politics/constituencies") %>%
                      html_nodes("td , .az-table__row th")

    constituency_names <- read_html("http://www.bbc.com/news/politics/constituencies") %>%
                          html_nodes(".az-table")

    constituency_urls <- constituencies %>%
                         html_nodes("a") %>%
                         html_attrs() %>%
                         paste("http://bbc.com", ., sep="")

    ## The BBC URL is also the topoJSON identifier; useful later
    constituency_ids <- str_replace(constituency_urls, "http://bbc.com/news/politics/constituencies/", "")
    constituency_names <- lapply(constituency_names, html_table)

    library(data.table)
    constituency_names.df <- data.frame(rbindlist(constituency_names))
    detach(package:data.table)

    colnames(constituency_names.df) <- c("Constituency", "Nation")
    constituency_names.df$URL <- constituency_urls
    constituency_names.df$id <- constituency_ids

    write.csv(constituency_names.df, file="data/constituency_names.csv")
}


###  And if we've done all *this* already, don't do it again
if (!file.exists("data/uk-election-results-2015.csv")) {
# get and store all constituency pages and data
  all.results.list <- list()
  for(i in 1:nrow(constituency_names.df)){

      url <- constituency_names.df$URL[i]

      constituency <- read_html(url)

      name <- constituency %>%
              html_nodes(".constituency-title__title") %>%
              html_text()

      fname <- paste("data/", name, ".rda", sep="")

      save(constituency, file=fname)

      all.results.list[[name]] <- get.constituency.results(constituency)
      message(paste("Completed", name))
      Sys.sleep(1.5) ## Try to be polite

}

# combined the data and tidy up slightly ----------------------------------------
  data <- rbind_all(all.results.list)
  ind <- match(data$Constituency, constituency_names.df$Constituency)
  data$Region <- constituency_names.df$Nation[ind]

### parties by N candidates
  main.parties <- data %>%
                  group_by(Party) %>%
                  tally() %>%
                  arrange(desc(n)) %>%
                  filter(n>14) %>%
                  data.frame(.)

  ind <- data$Party %nin% main.parties$Party

  data$Party.all <- data$Party
  data$Party <- as.character(data$Party)
  data$Party[ind] <- "Other"
  data$Party <- factor(data$Party, levels=unique(data$Party))

# write data out to csv for next time
  write.csv(data, "data/uk-election-results-2015.csv")

} else {

# read in the data -------------------------------------------------------------
  data <- read.csv("data/uk-election-results-2015.csv", row.names=1)
  constituency_names.df <- read.csv("data/constituency_names.csv")

}

# some simple summaries and tidying that will be used later --------------------
  data <- data %>%
          group_by(Constituency) %>%
          mutate(Total.Votes.Cast=sum(Votes),
                 Rank=row_number(desc(Votes))) %>%
          data.frame()

## Code the Speaker of the House as Conservative
  data %>% filter(Constituency == "Buckingham")
  data$Party[data$Candidate == "John Bercow"] <- "Conservative"

  by.mps <- data %>%
            group_by(Constituency) %>%
            filter(Votes==max(Votes))  %>%
            ungroup() %>%
            arrange(desc(Vote.Share))  %>%
            data.frame(.)

  by.party <- by.mps %>%
              group_by(Party) %>% summarize(Seats=n()) %>% arrange(desc(Seats))

## In Seat order
by.seats <- data %>% group_by(Constituency) %>% filter(Votes==max(Votes)) %>%
    group_by(Party) %>% tally() %>% arrange(desc(n)) %>%
    data.frame(.)

library(gdata)
data$Party <- as.factor(data$Party)
data$Party <- reorder.factor(data$Party, new.order=by.seats$Party)
by.mps$Party <- reorder.factor(by.mps$Party, new.order=by.seats$Party)
detach(package:gdata)


###--------------------------------------------------
### Now we can start looking at the data
###--------------------------------------------------
uk.colors <- data.frame(Party=levels(by.mps$Party),
                        party.color=c(
                            "#1577C7", # Conservative
                            "#E8251F", # Labour
                            "#EAC135", # SNP
                            "#BC1D40", # DUP
                            "#FA8324", # Lim-Dems
                            "#126140", # Sinn Fein
                            "#559D37", # Plaid Cymru
                            "#6AA769", # SDLP
                            "#6EB2E4", # UUP
                            "#7EC031", # Greens
                            "#999999", # Independent
                            "#6E3485" # UKIP
                            ),
                        stringsAsFactors = FALSE)

winners <- left_join(by.mps, constituency_names.df) %>%
           left_join(uk.colors)
library(gdata)
winners$Party <- as.factor(winners$Party)
winners$Party <- reorder.factor(winners$Party, new.order=by.seats$Party)
detach(package:gdata)



###--------------------------------------------------
### Maps
###--------------------------------------------------

uk.map <- readOGR("maps/topo_wpc.json", "wpc")

## The name field didn't get imported properly for some reason.
# ind <- match(uk.map@data$id, constituency_names.df$id)
ind <- match(uk.map@data$id, winners$id)
uk.map@data$name <- winners$Constituency[ind]
uk.map@data$party <- winners$Party[ind]
uk.map@data$party.colour <- winners$party.color[ind]
uk.map@data$mp <- winners$Candidate[ind]
uk.map@data$popup <- paste0("<strong>",
                            uk.map@data$name,
                            "</strong>",
                            "<br><strong>MP: </strong>",
                            uk.map@data$mp)



leaflet(uk.map) %>%
  addPolygons(stroke = F, fillOpacity = 0.95, smoothFactor = 0.5,
              fillColor = ~uk.map@data$party.colour,
              popup = ~uk.map@data$popup)


