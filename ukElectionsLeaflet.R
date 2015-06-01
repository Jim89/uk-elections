################################################################################
# Scraping and plotting 2015 UK election data
################################################################################
# set up working environment ---------------------------------------------------
# load packages
  library(magrittr)
  library(dplyr)
  library(rvest)
  library(stringr)
  library(car)
  library(rgeos)
  library(rgdal)
  library(leaflet)
  library(readxl)

# create functions to be used --------------------------------------------------
# Convenience "not in" function
  "%nin%" <- function(x, y) {
    return( !(x %in% y) )
  }

# get the results
  get.constituency.results <- function (constituency) {
    library(rvest)

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
      library(rvest)
    constituencies <- html("http://www.bbc.com/news/politics/constituencies") %>%
                      html_nodes("td , .az-table__row th")

    constituency_names <- html("http://www.bbc.com/news/politics/constituencies") %>%
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

# read in the elections results data -------------------------------------------
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
  by.seats <- data %>%
              group_by(Constituency) %>%
              filter(Votes==max(Votes)) %>%
              group_by(Party) %>%
              tally() %>%
              arrange(desc(n)) %>%
              data.frame(.)

# recode the factor levels for Party
  library(gdata)
  data$Party <- as.factor(data$Party)
  data$Party <- reorder.factor(data$Party, new.order=by.seats$Party)
  by.mps$Party <- reorder.factor(by.mps$Party, new.order=by.seats$Party)
  detach(package:gdata)

# set the party colours
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

# read in the consituency economic data ----------------------------------------
  wealth <- read_excel("./data/wealthDataExcel.xls", skip = 2, na = c(":")) %>%
            setNames(c("Constituency", "id", "Prop.Over.65", "Pension.Claim.Rate",
                       "Good.Health.Rate", "Prop.Non.White", "Median.House.Price",
                       "Employment.Rate", "Unemployment.Rate", "Percentage.Tax.Credit",
                       "Median.Income","Businesses.Per.10k"))
  wealth <- wealth[1:650, ]


# election results with wealth data (for ease of use later) --------------------
  winners <- left_join(by.mps, constituency_names.df,
                       by = c("Constituency" = "Constituency")) %>% #View
             left_join(uk.colors, by = c("Party" = "Party")) %>% #View
             left_join(wealth, by = c("id" = "id")) %>%
             setNames(tolower(names(.))) %>%
             select(constituency.x, party, candidate, votes, vote.share, swing,
                    total.votes.cast, id, party.color, prop.over.65,
                    pension.claim.rate, good.health.rate, prop.non.white,
                    median.house.price, employment.rate, unemployment.rate,
                    percentage.tax.credit, median.income, businesses.per.10k) %>%
            rename(constituency = constituency.x)

# tidy up constituency to be a character not a factor
  winners$constituency %<>% as.character


# tidy up party to be a factor
  library(gdata)
  winners$party %<>% as.factor
  winners$party %<>% reorder.factor(new.order = by.seats$Party)
  detach(package:gdata)

# clean up the crap we don't need
  to_remove <- ls()
  to_remove <- to_remove[-grep("winners", to_remove)]
  rm(list = to_remove)
  gc()


# get and clean/combine the map data -------------------------------------------
# get the map - doesn't have Northern Ireland, sadly :(
  uk.map <- readOGR("maps/topo_wpc.json", "wpc")

# find the overlaps in the id to perform the join
  ind <- match(uk.map@data$id, winners$id)

# bolt on bits of info from the "winners" data set that we might want to plot
  uk.map@data$name <- winners$constituency[ind]
  uk.map@data$party <- winners$party[ind]
  uk.map@data$party.colour <- winners$party.color[ind]
  uk.map@data$mp <- winners$candidate[ind]
  uk.map@data$income <- winners$median.income[ind]
  uk.map@data$prop.over.65 <- winners$prop.over.65[ind]
  uk.map@data$businesses.per.10k <- winners$businesses.per.10k[ind]
  uk.map@data$popup <- paste0("<strong>",
                              uk.map@data$name,
                              "</strong>",
                              "<br><strong>MP: </strong>",
                              uk.map@data$mp,
                              " (",uk.map@data$party, ")",
                              "<br><strong>Median Income: </strong>&pound",
                              format(uk.map@data$income, big.mark = ","),
                              "<br><strong>Businesses per 10,000 people: </strong>",
                              format(round(uk.map@data$businesses.per.10k),
                                     big.mark = ","))


# create the leaflet plot ------------------------------------------------------
leaflet(uk.map) %>%
  #addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = T,
              color = "white",
              weight = 0.3,
              fillOpacity = 0.95,
              smoothFactor = 0.5,
              fillColor = ~uk.map@data$party.colour,
              popup = ~uk.map@data$popup) %>%
  addLegend(position = "topleft",
            colors = unique(uk.map@data$party.colour),
            labels = unique(uk.map@data$party),
            opacity = 0.95)

