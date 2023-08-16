library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)
library(tigris)
library(leaflet)
library(htmltools)
library(xlsx)

input_dir <- "input/"
data_dir  <- "data/"
out_dir   <- "out/"

shinyServer(
    function(session,input, output) {
        options(width = 240, readr.show_progress = FALSE)
        options(max.print=999999)

        states <- c("Alabama","Alaska","Arizona","Arkansas","California",
                    "Colorado","Connecticut","Delaware","DC","Florida",
                    "Georgia","Hawaii","Idaho","Illinois","Indiana",
                    "Iowa","Kansas","Kentucky","Louisiana","Maine",
                    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                    "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                    "South Carolina","South Dakota","Tennessee","Texas","Utah",
                    "Vermont","Virginia","Washington","West Virginia","Wisconsin",
                    "Wyoming","D.C.","District of Columbia")
        stabbr <- c("AL","AK","AZ","AR","CA",
                    "CO","CT","DE","DC","FL",
                    "GA","HI","ID","IL","IN",
                    "IA","KS","KY","LA","ME",
                    "MD","MA","MI","MN","MS",
                    "MO","MT","NE","NV","NH",
                    "NJ","NM","NY","NC","ND",
                    "OH","OK","OR","PA","RI",
                    "SC","SD","TN","TX","UT",
                    "VT","VA","WA","WV","WI","WY","DC","DC")
        zstates <- c("Alaska","Alabama","Arkansas","Arizona","California",
                     "Colorado","Connecticut","DC","Delaware","Florida",
                     "Georgia","Hawaii","Iowa","Idaho","Illinois",
                     "Indiana","Kansas","Kentucky","Louisiana","Massachusetts",
                     "Maryland","Maine","Michigan","Minnesota","Missouri",
                     "Mississippi","Montana","North Carolina","North Dakota","Nebraska",
                     "New Hampshire","New Jersey","New Mexico","Nevada","New York",
                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                     "South Carolina","South Dakota","Tennessee","Texas","Utah",
                     "Virginia","Vermont","Washington","Wisconsin","West Virginia",
                     "Wyoming","D.C.","District of Columbia")
        zstabbr <- c("AK","AL","AR","AZ","CA",
                     "CO","CT","DC","DE","FL",
                     "GA","HI","IA","ID","IL",
                     "IN","KS","KY","LA","MA",
                     "MD","ME","MI","MN","MO",
                     "MS","MT","NC","ND","NE",
                     "NH","NJ","NM","NV","NY",
                     "OH","OK","OR","PA","RI",
                     "SC","SD","TN","TX","UT",
                     "VA","VT","WA","WI","WV","WY","DC","DC")
        stinfo <- data.frame(states,stabbr)
        getStateAbbr <- function(str){
            for (i in 1:length(states)){
                pattern <- paste0("^",states[i])
                if (any(grep(pattern, str))){
                    str <- gsub(pattern, stabbr[i], str)
                    break
                }
            }
            return(str)
        }
        getState <- function(str){
            state <- as.character(stinfo$states[stinfo$stabbr == str])
            if (identical(state,character(0))){
                return(str)                
            }
            return(state)
        }
        get_congress_map <- function(cong=113) {
            tmp_file <- tempfile()
            tmp_dir  <- tempdir()
            zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
            download.file(zp, tmp_file)
            unzip(zipfile = tmp_file, exdir = tmp_dir)
            fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
            st_read(fpath)
        }
        create538_22 <- function(model){
            xx0 <- read_csv(paste0(input_dir,"house_district_toplines_2022.csv"))
            xx1 <- xx0[str_detect(xx0$forecastdate, "11/8/22"),]
            xx <- xx1[xx1$expression == model,]
            xx <- xx[,c("district","voteshare_mean_D1","voteshare_mean_R1",
                      "voteshare_mean_O1","mean_predicted_turnout")]
            names(xx) <- c("AREA","DEM","REP","OTH","TURNOUT")
            for (i in 2:4){
            xx[,i] <- xx$TURNOUT * xx[,i] / 100
            }
            xx <- xx[,1:(NCOL(xx)-1)]
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"House_538",model,"_2022.csv"))
            write_delim(xx, paste0(data_dir,"House_538",model,"_2022.csv"), append = TRUE, col_names = TRUE)
            
            xx0 <- read_csv(paste0(input_dir,"senate_state_toplines_2022.csv"))
            xx1 <- xx0[str_detect(xx0$forecastdate, "11/8/22"),]
            xx <- xx1[xx1$expression == model,]
            xx <- xx[,c("district","voteshare_mean_D1","voteshare_mean_R1","voteshare_mean_O1","mean_predicted_turnout")]
            names(xx) <- c("AREA","DEM","REP","OTH","TURNOUT")
            # xx$AREA[substr(xx$AREA,1,2) != "GA"] <- substr(xx$AREA[substr(xx$AREA,1,2) != "GA"],1,2)
            xx$AREA <- substr(xx$AREA,1,2)
            # xx$AREA[xx$AREA == "GA-S2"] <- "GA"
            # xx$AREA[xx$AREA == "GA-S3"] <- "GA-2"
            for (i in 2:4){
            xx[,i] <- xx$TURNOUT * xx[,i] / 100
            }
            xx <- xx[,1:(NCOL(xx)-1)]
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"Senate_538",model,"_2022.csv"))
            write_delim(xx, paste0(data_dir,"Senate_538",model,"_2022.csv"), append = TRUE, col_names = TRUE)
            
            xx0 <- read_csv(paste0(input_dir,"governor_state_toplines_2022.csv"))
            xx1 <- xx0[str_detect(xx0$forecastdate, "11/8/22"),]
            xx <- xx1[xx1$expression == model,]
            xx <- xx[,c("district","voteshare_mean_D1","voteshare_mean_R1","voteshare_mean_O1","mean_predicted_turnout")]
            names(xx) <- c("AREA","DEM","REP","OTH","TURNOUT")
            xx$AREA <- substr(xx$AREA,1,2)
            for (i in 2:4){
                xx[,i] <- xx$TURNOUT * xx[,i] / 100
            }
            xx <- xx[,1:(NCOL(xx)-1)]
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"Governor_538",model,"_2022.csv"))
            write_delim(xx, paste0(data_dir,"Governor_538",model,"_2022.csv"), append = TRUE, col_names = TRUE)
        }
        createHouse22 <- function(){
            filename <- paste0(input_dir,"House_US_221108.txt")
            ww <- read.delim(filename, header = FALSE)
            xx <- as.data.frame(ww)
            xx <- xx[seq(1,NROW(xx),2),]
            names(xx) <- c("DIST","WINNER","STATUS","DEM","REP","OTH","DEM%","REP%","OTH%","MARGIN22","MARGIN20","%OF20TO")
            xx$DIST <- gsub("-AL","-1", xx$DIST)
            xx$DIST <- gsub("-0", "-", xx$DIST)
            zxx <<- xx #DEBUG-RM
            xx <- xx[,c("DIST","DEM","REP","OTH")]
            names(xx) <- c("AREA","DEM","REP","OTH")
            write(paste(names(xx), collapse = " "), paste0(data_dir,"House_2022.csv"))
            write_delim(xx, paste0(data_dir,"House_2022.csv"), append = TRUE, col_names = TRUE)
        }
        createSenate22 <- function(){
            filename <- paste0(input_dir,"Senate_US_221108.txt")
            ww <- read.delim(filename, header = FALSE)
            xx <- as.data.frame(ww)
            xx$V2 <- gsub(",","",xx$V2)
            st2 <- NULL
            dem <- NULL
            rep <- NULL
            oth <- NULL
            for (i in 1:length(zstates)){
                j <- which (xx$V1 == zstates[i])
                j <- j+1
                if (length(j) > 0){
                    nr <- nd <- no <- 0
                    #cat(paste0("***** ",zstates[i],"\n")) #DEBUG-RM
                    if (!grepl("^ ", xx$V1[j])) j <- j+1
                    for (k in j:(j+1)){
                        if (grepl("(R)", xx$V1[k], fixed = TRUE)){
                            nr <- nr + as.integer(xx$V2[k])
                        }
                        else if (grepl("(D)", xx$V1[k], fixed = TRUE)){
                            nd <- nd + as.integer(xx$V2[k])
                        }
                        else{
                            no <- no + as.integer(xx$V2[k])
                        }
                    }
                    j <- j+2
                    while (TRUE){
                        if (!grepl("^ ", xx$V1[j])) break
                        no <- no + as.integer(xx$V2[j])
                        j <- j+1
                    }
                    st2 <- c(st2, zstabbr[i])
                    dem <- c(dem, nd)
                    rep <- c(rep, nr)
                    oth <- c(oth, no)
                }
            }
            zz <- data.frame(st2,dem,rep,oth)
            zzz <<- zz #DEBUG-RM
            names(zz) <- c("AREA","DEM","REP","OTH")
            write(paste(names(zz), collapse = " "), paste0(data_dir,"Senate_2022.csv"))
            write_delim(zz, paste0(data_dir,"Senate_2022.csv"), append = TRUE, col_names = TRUE)
        }
        createGovernor22 <- function(){
            filename <- paste0(input_dir,"Governor_US_221108.txt")
            ww <- read.delim(filename, header = FALSE)
            xx <- as.data.frame(ww)
            xx$V2 <- gsub(",","",xx$V2)
            st2 <- NULL
            dem <- NULL
            rep <- NULL
            oth <- NULL
            for (i in 1:length(zstates)){
                j <- which (xx$V1 == zstates[i])
                j <- j+1
                if (length(j) > 0){
                    nr <- nd <- no <- 0
                    #cat(paste0("***** ",zstates[i],"\n")) #DEBUG-RM
                    if (!grepl("^ ", xx$V1[j])) j <- j+1
                    for (k in j:(j+1)){
                        if (grepl("(R)", xx$V1[k], fixed = TRUE)){
                            nr <- nr + as.integer(xx$V2[k])
                        }
                        else if (grepl("(D)", xx$V1[k], fixed = TRUE)){
                            nd <- nd + as.integer(xx$V2[k])
                        }
                        else{
                            no <- no + as.integer(xx$V2[k])
                        }
                    }
                    j <- j+2
                    while (TRUE){
                        if (!grepl("^ ", xx$V1[j])) break
                        no <- no + as.integer(xx$V2[j])
                        j <- j+1
                    }
                    st2 <- c(st2, zstabbr[i])
                    dem <- c(dem, nd)
                    rep <- c(rep, nr)
                    oth <- c(oth, no)
                }
            }
            zz <- data.frame(st2,dem,rep,oth)
            zzz <<- zz #DEBUG-RM
            names(zz) <- c("AREA","DEM","REP","OTH")
            write(paste(names(zz), collapse = " "), paste0(data_dir,"Governor_2022.csv"))
            write_delim(zz, paste0(data_dir,"Governor_2022.csv"), append = TRUE, col_names = TRUE)
        }
        create538_20 <- function(model){
            xx0 <- read_csv(paste0(input_dir,"house_district_toplines_2020.csv"))
            xx1 <- xx0[str_detect(xx0$forecastdate, "11/3/20"),]
            xx <- xx1[xx1$expression == model,]
            xx <- xx[,c("district","voteshare_mean_D1","voteshare_mean_R1",
                        "voteshare_mean_O1","mean_predicted_turnout")]
            names(xx) <- c("AREA","DEM","REP","OTH","TURNOUT")
            for (i in 2:4){
                xx[,i] <- xx$TURNOUT * xx[,i] / 100
            }
            xx <- xx[,1:(NCOL(xx)-1)]
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"House_538",model,"_2020.csv"))
            write_delim(xx, paste0(data_dir,"House_538",model,"_2020.csv"), append = TRUE, col_names = TRUE)
            
            xx0 <- read_csv(paste0(input_dir,"senate_state_toplines_2020.csv"))
            xx1 <- xx0[str_detect(xx0$forecastdate, "11/3/20"),]
            xx <- xx1[xx1$expression == model,]
            xx <- xx[,c("district","voteshare_mean_D1","voteshare_mean_R1","voteshare_mean_O1","mean_predicted_turnout")]
            names(xx) <- c("AREA","DEM","REP","OTH","TURNOUT")
            xx$AREA[substr(xx$AREA,1,2) != "GA"] <- substr(xx$AREA[substr(xx$AREA,1,2) != "GA"],1,2)
            xx$AREA[xx$AREA == "GA-S2"] <- "GA"
            xx$AREA[xx$AREA == "GA-S3"] <- "GA-2"
            for (i in 2:4){
                xx[,i] <- xx$TURNOUT * xx[,i] / 100
            }
            xx <- xx[,1:(NCOL(xx)-1)]
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"Senate_538",model,"_2020.csv"))
            write_delim(xx, paste0(data_dir,"Senate_538",model,"_2020.csv"), append = TRUE, col_names = TRUE)
            
            xx0 <- read_csv(paste0(input_dir,"presidential_state_toplines_2020.csv"))
            xx1 <- xx0[str_detect(xx0$modeldate, "11/3/2020"),]
            xx <- xx1[,c("state","voteshare_chal","voteshare_inc","state_turnout")]
            names(xx) <- c("AREA","DEM","REP","TURNOUT")
            for (i in 1:length(xx$AREA)){
                xx$AREA[i] <- getStateAbbr(xx$AREA[i])
            }
            for (i in 2:3){
                xx[,i] <- xx$TURNOUT * xx[,i] / 100
            }
            xx <- xx[,1:(NCOL(xx)-1)]
            namesxx <- names(xx)
            names(xx)[2:3] <- c("Biden","Trump")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"President_538",model,"_2020.csv"))
            write_delim(xx, paste0(data_dir,"President_538",model,"_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createHouse538_18 <- function(model){
            xx0 <- read_csv(paste0(input_dir,"house_district_forecast.csv"))
            xx1 <- xx0[xx0$forecastdate == as.Date("2018-11-06"),]
            xx <- xx1[xx1$model == model,]
            xx$cd <- paste0(xx$state,"-",xx$district)
            xx$party[xx$party == "D"] <- "AADEM"
            xx$party[xx$party == "R"] <- "AAREP"
            xx <- xx[,c("cd","party","voteshare")]
            xx <- xx %>%
                group_by(cd, party) %>%
                summarize(voteshare = max(voteshare)) %>%
                spread(party,voteshare)
            #names(xx)[1] <- "AAAREA"
            #oo <- order(names(xx))
            #xx <- xx[,oo]
            names(xx)[1:3] <- c("AREA","DEM","REP")
            for (i in 4:NCOL(xx)){
                names(xx)[i] <- paste0("OTH",i)
            }
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"House_538_",model,"_2018.csv"))
            write_delim(xx, paste0(data_dir,"House_538_",model,"_2018.csv"), append = TRUE, col_names = TRUE)
        }
        createPresident20 <- function(){
            xx0 <- read_csv(paste0(input_dir,"1976-2020-president.csv"))
            xx1 <- xx0[xx0$year == 2020,]
            xx <- data.frame(unique(xx1$state_po))
            names(xx) <- "AREA"
            xx$DEM <- 0
            xx$REP <- 0
            xx$OTH <- 0
            xx$OVERVOTES <- 0
            xx$UNDERVOTES <- 0
            xx$BLANKVOTES <- 0
            xx$TOT <- 0
            for (i in 1:NROW(xx1)){
                if (xx1$party_simplified[i] == "DEMOCRAT"){
                    xx$DEM[xx$AREA == xx1$state_po[i]] <- max(
                        xx$DEM[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (xx1$party_simplified[i] == "REPUBLICAN"){
                    xx$REP[xx$AREA == xx1$state_po[i]] <- max(
                        xx$REP[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (is.na(xx1$candidate[i]) & (is.na(xx1$writein[i]) | xx1$writein[i])){ #check NA for line 274 (4015)
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "OVERVOTES"){
                    xx$OVERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "UNDERVOTES"){
                    xx$UNDERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "BLANK VOTES"){
                    xx$BLANKVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else{
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
            }
            xx <- xx[,1:4]
            namesxx <- names(xx)
            names(xx)[2:3] <- c("Biden","Trump")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"President_2020.csv"))
            write_delim(xx, paste0(data_dir,"President_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createPresident16 <- function(){
            xx0 <- read_csv(paste0(input_dir,"1976-2020-president.csv"))
            xx1 <- xx0[xx0$year == 2016,]
            xx <- data.frame(unique(xx1$state_po))
            names(xx) <- "AREA"
            xx$DEM <- 0
            xx$REP <- 0
            xx$OTH <- 0
            xx$OVERVOTES <- 0
            xx$UNDERVOTES <- 0
            xx$BLANKVOTES <- 0
            xx$TOT <- 0
            for (i in 1:NROW(xx1)){
                party_simplified <<- xx1$party_simplified[i]
                party_detailed <<- xx1$party_detailed[i]
                candidate <<- xx1$candidate[i]
                if (xx1$party_simplified[i] == "DEMOCRAT"){
                    xx$DEM[xx$AREA == xx1$state_po[i]] <- max(
                        xx$DEM[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (xx1$party_simplified[i] == "REPUBLICAN"){
                    xx$REP[xx$AREA == xx1$state_po[i]] <- max(
                        xx$REP[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (is.na(xx1$candidate[i]) & (is.na(xx1$writein[i]) | xx1$writein[i])){ #check NA for line 274 (4015)
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
                # else if (is.na(xx1$candidate[i]) & party_detailed == "UNENROLLED"){
                #     #ignore for now
                # }
                else if (is.na(xx1$candidate[i])){
                    #ignore for now
                }
                else if (xx1$candidate[i] == "OVERVOTES"){
                    xx$OVERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "UNDERVOTES"){
                    xx$UNDERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "BLANK VOTES"){
                    xx$BLANKVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else{
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
            }
            xx <- xx[,1:4]
            namesxx <- names(xx)
            names(xx)[2:3] <- c("Clinton","Trump")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"President_2016.csv"))
            write_delim(xx, paste0(data_dir,"President_2016.csv"), append = TRUE, col_names = TRUE)
        }
        createPresident12 <- function(){
            xx0 <- read_csv(paste0(input_dir,"1976-2020-president.csv"))
            xx1 <- xx0[xx0$year == 2012,]
            xx <- data.frame(unique(xx1$state_po))
            names(xx) <- "AREA"
            xx$DEM <- 0
            xx$REP <- 0
            xx$OTH <- 0
            xx$OVERVOTES <- 0
            xx$UNDERVOTES <- 0
            xx$BLANKVOTES <- 0
            xx$TOT <- 0
            for (i in 1:NROW(xx1)){
                party_simplified <<- xx1$party_simplified[i]
                party_detailed <<- xx1$party_detailed[i]
                candidate <<- xx1$candidate[i]
                if (xx1$party_simplified[i] == "DEMOCRAT"){
                    xx$DEM[xx$AREA == xx1$state_po[i]] <- max(
                        xx$DEM[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (xx1$party_simplified[i] == "REPUBLICAN"){
                    xx$REP[xx$AREA == xx1$state_po[i]] <- max(
                        xx$REP[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (is.na(xx1$candidate[i]) & (is.na(xx1$writein[i]) | xx1$writein[i])){ #check NA for line 274 (4015)
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
                # else if (is.na(xx1$candidate[i]) & party_detailed == "UNENROLLED"){
                #     #ignore for now
                # }
                else if (is.na(xx1$candidate[i])){
                    #ignore for now
                }
                else if (xx1$candidate[i] == "OVERVOTES"){
                    xx$OVERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "UNDERVOTES"){
                    xx$UNDERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "BLANK VOTES"){
                    xx$BLANKVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else{
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
            }
            xx <- xx[,1:4]
            namesxx <- names(xx)
            names(xx)[2:3] <- c("Obama","Romney")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"President_2012.csv"))
            write_delim(xx, paste0(data_dir,"President_2012.csv"), append = TRUE, col_names = TRUE)
        }
        createPresident04 <- function(){
            xx0 <- read_csv(paste0(input_dir,"1976-2020-president.csv"))
            xx1 <- xx0[xx0$year == 2004,]
            xx <- data.frame(unique(xx1$state_po))
            names(xx) <- "AREA"
            xx$DEM <- 0
            xx$REP <- 0
            xx$OTH <- 0
            xx$OVERVOTES <- 0
            xx$UNDERVOTES <- 0
            xx$BLANKVOTES <- 0
            xx$TOT <- 0
            for (i in 1:NROW(xx1)){
                party_simplified <<- xx1$party_simplified[i]
                party_detailed <<- xx1$party_detailed[i]
                candidate <<- xx1$candidate[i]
                if (xx1$party_simplified[i] == "DEMOCRAT"){
                    xx$DEM[xx$AREA == xx1$state_po[i]] <- max(
                        xx$DEM[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (xx1$party_simplified[i] == "REPUBLICAN"){
                    xx$REP[xx$AREA == xx1$state_po[i]] <- max(
                        xx$REP[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                }
                else if (is.na(xx1$candidate[i]) & (is.na(xx1$writein[i]) | xx1$writein[i])){ #check NA for line 274 (4015)
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
                # else if (is.na(xx1$candidate[i]) & party_detailed == "UNENROLLED"){
                #     #ignore for now
                # }
                else if (is.na(xx1$candidate[i])){
                    #ignore for now
                }
                else if (xx1$candidate[i] == "OVERVOTES"){
                    xx$OVERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "UNDERVOTES"){
                    xx$UNDERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "BLANK VOTES"){
                    xx$BLANKVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                }
                else{
                    xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                }
            }
            xx <- xx[,1:4]
            namesxx <- names(xx)
            names(xx)[2:3] <- c("Obama","Romney")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"President_2004.csv"))
            write_delim(xx, paste0(data_dir,"President_2004.csv"), append = TRUE, col_names = TRUE)
        }
        createSenate20 <- function(){
            xx0 <- read_csv(paste0(input_dir,"1976-2020-senate.csv"))
            xx1 <- xx0[xx0$year == 2020 | xx0$year == 2021,] # 2021 for GA runoff
            xx <- data.frame(c(unique(xx1$state_po),"GA-2"), stringsAsFactors = FALSE)
            names(xx) <- "AREA"
            xx$DEM <- 0
            xx$REP <- 0
            xx$OTH <- 0
            xx$OVERVOTES <- 0
            xx$UNDERVOTES <- 0
            xx$BLANKVOTES <- 0
            xx$TOT <- 0
            for (i in 1:NROW(xx1)){
                if (as.character(xx1$state_po[i]) == "GA"){
                    if (xx1$stage[i] == "runoff"){
                        xxarea <- "GA"
                        if (xx1$special[i]){
                            xxarea <- "GA-2"
                        }
                        if (xx1$party_simplified[i] == "DEMOCRAT"){
                            xx$DEM[xx$AREA == xxarea] <- xx1$candidatevotes[i]
                        }
                        else if (xx1$party_simplified[i] == "REPUBLICAN"){
                            xx$REP[xx$AREA == xxarea] <- xx1$candidatevotes[i]
                        }
                    }
                }
                else{
                    if (xx1$party_simplified[i] == "DEMOCRAT"){
                        xx$DEM[xx$AREA == xx1$state_po[i]] <- max(
                            xx$DEM[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                    }
                    else if (xx1$party_simplified[i] == "REPUBLICAN"){
                        xx$REP[xx$AREA == xx1$state_po[i]] <- max(
                            xx$REP[xx$AREA == xx1$state_po[i]],xx1$candidatevotes[i])
                    }
                    else if (is.na(xx1$candidate[i]) & (is.na(xx1$writein[i]) | xx1$writein[i])){ #check NA for line 274 (4015)
                        xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                    }
                    else if (xx1$candidate[i] == "OVER VOTES" | xx1$candidate[i] == "OVER VOTE"){
                        xx$OVERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                    }
                    else if (xx1$candidate[i] == "UNDER VOTES" | xx1$candidate[i] == "UNDER VOTE"){
                        xx$UNDERVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                    }
                    else if (xx1$candidate[i] == "BLANK VOTES" | xx1$candidate[i] == "BLANK VOTE"){
                        xx$BLANKVOTES[xx$AREA == xx1$state_po[i]] <- xx1$candidatevotes[i]
                    }
                    else{
                        xx$OTH[xx$AREA == xx1$state_po[i]] <- xx$OTH[xx$AREA == xx1$state_po[i]] + xx1$candidatevotes[i]
                    }
                }
            }
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"Senate_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createHouse20 <- function(){
            xx <- read_excel(paste0(input_dir,"House Popular Vote Tracker.xlsx"), sheet = "Sheet1")
            cdn <- xx$`CD#`
            cdn[cdn == "AL"] <- 1
            cdn <- as.integer(cdn)
            for (i in 1:length(xx$state)){
                xx$district[i] <- paste0(getStateAbbr(xx$state[i]),"-",cdn[i])
            }
            xx <- xx[2:NROW(xx),c("district","dem_votes","rep_votes","other_votes")]
            names(xx) <- c("AREA","DEM","REP","OTH")
            write(paste(names(xx), collapse = " "), paste0(data_dir,"House_2020.csv"))
            write_delim(xx, paste0(data_dir,"House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        getvotes <- function(xx, i){
            ggxx <<- xx
            ggi <<- i
            if (xx$fusion_ticket[i]){
                if (substr(xx$party[i],1,8) == "DEMOCRAT" |
                    substr(xx$party[i],1,10) == "REPUBLICAN"){
                    votes <- sum(xx$candidatevotes[xx$year == xx$year[i] &
                                                       xx$cd == xx$cd[i] &
                                                       !is.na(xx$candidate) &
                                                       xx$candidate == xx$candidate[i]])
                    #cat(file=stderr(), paste0("+++ ",xx$year[i],"|",xx$cd[i],": FUSION_TICKET ",xx$candidate[i],", VOTES: ",xx$candidatevotes[i]," -> ",votes,"\n"))
                    cat(file=stderr(), paste0("+++ ",xx$year[i],"|",xx$cd[i],": FUSION_TICKET ",xx$candidate[i],"|",xx$party[i],": ADD VOTES: ",votes-xx$candidatevotes[i],"\n"))
                }
                else{
                    votes <- 0
                    cat(file=stderr(), paste0("--- ",xx$year[i],"|",xx$cd[i],": FUSION_TICKET ",xx$candidate[i],"|",xx$party[i],": SUB VOTES: ",-xx$candidatevotes[i],"\n"))
                }
            }
            else{
                votes <- xx$candidatevotes[i]
            }
            return(votes)
        }
        createHouseNN <- function(year){
            xx0 <- read_csv(paste0(input_dir,"1976-2018-house3.csv"))
            xx1 <- xx0[xx0$year == year,] # no 2019 for runoff
            xx1$district[xx1$district == 0] <- 1
            xx1$cd <- paste0(xx1$state_po,"-",xx1$district)
            xx <- data.frame(unique(xx1$cd), stringsAsFactors = FALSE)
            names(xx) <- "AREA"
            xx$DEM <- 0
            xx$REP <- 0
            xx$OTH <- 0
            xx$OVERVOTES <- 0
            xx$UNDERVOTES <- 0
            xx$BLANKVOTES <- 0
            xx$TOT <- 0
            for (i in 1:NROW(xx1)){
                gxx1 <<- xx1 #DEBUG-RM
                gxx <<- xx #DEBUG-RM
                ii <<- i #DEBUG-RM
                #if (is.na(xx1$party[i]) & xx1$writein[i]){
                if (is.na(xx1$party[i])){
                    if (!is.na(xx1$writein[i]) & xx1$writein[i]){
                        xx$OTH[xx$AREA == xx1$cd[i]] <- xx$OTH[xx$AREA == xx1$cd[i]] + xx1$candidatevotes[i]
                    }
                    #check?
                }
                else if (xx1$party[i] == "DEMOCRAT"){
                    votes <- getvotes(xx1, i)
                    xx$DEM[xx$AREA == xx1$cd[i]] <- max(
                        xx$DEM[xx$AREA == xx1$cd[i]],votes)
                }
                else if (substr(xx1$party[i],1,8) == "DEMOCRAT"){
                    cat(file=stderr(), paste0("=== ",xx1$year[i],"|",xx1$cd[i],": READ ",xx1$party[i]," as DEMOCRAT\n"))
                    votes <- getvotes(xx1, i)
                    xx$DEM[xx$AREA == xx1$cd[i]] <- max(
                        xx$DEM[xx$AREA == xx1$cd[i]],votes)
                }
                else if (xx1$party[i] == "REPUBLICAN"){
                    votes <- getvotes(xx1, i)
                    xx$REP[xx$AREA == xx1$cd[i]] <- max(
                        xx$REP[xx$AREA == xx1$cd[i]],votes)
                }
                else if (substr(xx1$party[i],1,10) == "REPUBLICAN"){
                    cat(file=stderr(), paste0("=== ",xx1$year[i],"|",xx1$cd[i],": READ ",xx1$party[i]," as REPUBLICAN\n"))
                    votes <- getvotes(xx1, i)
                    xx$REP[xx$AREA == xx1$cd[i]] <- max(
                        xx$REP[xx$AREA == xx1$cd[i]],votes)
                }
                else if (is.na(xx1$candidate[i]) & (is.na(xx1$writein[i]) | xx1$writein[i])){ #check NA for line 274 (4015)
                    votes <- getvotes(xx1, i)
                    xx$OTH[xx$AREA == xx1$cd[i]] <- xx$OTH[xx$AREA == xx1$cd[i]] + votes
                }
                else if (xx1$candidate[i] == "OVER VOTE" | xx1$candidate[i] == "OVER VOTES"){
                    xx$OVERVOTES[xx$AREA == xx1$cd[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "UNDER VOTE" | xx1$candidate[i] == "UNDER VOTES"){
                    xx$UNDERVOTES[xx$AREA == xx1$cd[i]] <- xx1$candidatevotes[i]
                }
                else if (xx1$candidate[i] == "BLANK VOTE" | xx1$candidate[i] == "BLANK VOTE/SCATTERING"){
                    xx$BLANKVOTES[xx$AREA == xx1$cd[i]] <- xx1$candidatevotes[i]
                }
                else{
                    votes <- getvotes(xx1, i)
                    xx$OTH[xx$AREA == xx1$cd[i]] <- xx$OTH[xx$AREA == xx1$cd[i]] + votes
                }
            }
            for (i in 1:NROW(xx)){
                if (is.na(xx$DEM[i])){
                    cat(file=stderr(), paste0("... ",year,"|",xx$AREA[i],": NO DEMOCRAT (NA)\n"))
                }
                else if (xx$DEM[i] == 0){
                    cat(file=stderr(), paste0("... ",year,"|",xx$AREA[i],": NO DEMOCRAT\n"))
                }
                if (is.na(xx$REP[i])){
                    cat(file=stderr(), paste0("... ",year,"|",xx$AREA[i],": NO REPUBLICAN (NA)\n"))
                }
                else if (xx$REP[i] == 0){
                    cat(file=stderr(), paste0("... ",year,"|",xx$AREA[i],": NO REPUBLICAN\n"))
                }
            } 
            xx$DEM[xx$DEM == 0]<- NA
            xx$REP[xx$REP == 0]<- NA
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"House_",year,".csv"))
            write_delim(xx, paste0(data_dir,"House_",year,".csv"), append = TRUE, col_names = TRUE)
        }
        getlabels <- function(type){
            if (input$measure == "Percent change"){
                tshift <- "% Change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "% Ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (type != "plot"){
                if (input$tnote == ""){
                    tnote <- paste0("(",input$units,")")
                }
                else{
                    tnote <- input$tnote
                }
            }
            else if (input$party == "Margin"){
                if (input$tnote == ""){
                    tnote <- "(positive direction is more Democratic)"
                }
                else{
                    tnote <- input$tnote
                }
            }
            else{
                tnote <- ""
            }
            tstate2 <- input$state2
            if (tstate2 == ""){
                tstate2 <- "U.S."
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$modely == "(same as above)"){
                modely <- input$model
            }
            else{
                modely <- input$modely
            }
            if (grepl("_538$", input$racex)){
                racex <- paste0(input$racex,input$model,"_",input$yearx)
            }
            if (grepl("_538$", input$racey)){
                racey <- paste0(input$racey,modely,"_",input$yearx)
            }
            if (input$racey == "Registered"){
                title <- paste(tshift, input$party, "Voters for",
                               racex, "to", input$party, racey,
                               "in", tstate2, tnote)
                ylabel <- paste(tshift, input$party, "Voters to", input$party,
                                racey)
            }
            else{
                title <- paste(tshift, input$party, tunits, "from",
                               racex, "to", racey,
                               "Race in", tstate2, tnote)
                if (input$flipy){
                    ylabel <- paste(tshift, input$party, tunits, "from", racey)
                }
                else{
                    ylabel <- paste(tshift, input$party, tunits, "for", racey)
                }
            }
            xlabel <- paste0(input$party," ",tunits," for ", racex,
                             "\nSources: see http://econdataus.com/voting_538.htm")
            title2 <- title
            if (input$mapvar == "MARGIN1"){
                title2 <- paste("Margin", tunits, "in", racex, "Race in", tstate2,
                                tnote)
            }
            else if (input$mapvar == "MARGIN2"){
                title2 <- paste("Margin", tunits, "in", racey, "Race in", tstate2,
                                tnote)
            }
            labels <- c(title, xlabel, ylabel, title2)
            return(labels)
        }
        output$myUsage <- renderUI({
            includeHTML("http://econdataus.com/voting_538.htm")
        })
        output$myPlot <- renderPlot({
            xx <- getdata()
            if (input$units == "Count"){
                xx <- xx[-NROW(xx),]
            }
            names(xx)[2:9] <- c("DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            xx <- xx[xx$DEM1 > 0 & xx$REP1 > 0 & xx$DEM2 > 0 & xx$REP2,]
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            xx$Party <- ""
            gxx <<- xx #DEBUG-RM
            if (input$xlimit != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit, ",")))
                vparty <- unlist(strsplit(input$xparty, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                for (i in 1:length(vlimit)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                }
            }
            if (input$vlimit != ""){
                vlimit <- as.numeric(unlist(strsplit(input$vlimit, ","))) * 1000
                vdesc <- unlist(strsplit(input$vdesc, ","))
                xx$Votes <- vdesc[length(vdesc)]
                xx$Votes[xx[[party1n]] < vlimit[1]] <- vdesc[1]
                for (i in 1:length(vlimit)){
                    xx$Votes[xx[[party1n]] >= vlimit[i] & xx[[party1n]] < vlimit[i+1]] <- vdesc[i+1]
                }
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                isParty <- c(isParty, any(xx$Party == vparty[i]))
            }
            isVotes <- NULL
            for (i in 1:length(vdesc)){
                isVotes <- c(isVotes, any(xx$Votes == vdesc[i]))
            }
            gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            gg <- gg + geom_point(data=xx, size=3, alpha=0.7,
                                  aes_string(color="Party",shape="Votes"))
            if (input$party == "Margin"){
                if (input$flipy){
                    gg <- gg + geom_abline(intercept=0, slope=1, color="gray", linetype="dashed")
                }
                else{
                    gg <- gg + geom_abline(intercept=0, slope=-1, color="gray", linetype="dashed")
                }
            }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color="gray")
            }
            if (input$measure != "Percent ratio"){
                gg <- gg + geom_hline(yintercept=0, color="gray")
            }
            vcolor <- unlist(strsplit(input$xcolor, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 1){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vshape <- as.numeric(unlist(strsplit(input$vshape, ",")))
            vshape <- vshape[isVotes]
            if (length(vshape) > 1){
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            labels <- getlabels("plot")
            gg <- gg + ggtitle(labels[1])
            gg <- gg + xlab(labels[2])
            gg <- gg + ylab(labels[3])
            xx$POS   <- 2 # RESET TO 2
            xx$LABEL <- xx$AREA
            spos1 <- unlist(strsplit(input$pos1, ","))
            xx$POS[xx$LABEL %in% spos1] <- 1
            spos3 <- unlist(strsplit(input$pos3, ","))
            xx$POS[xx$LABEL %in% spos3] <- 3
            if (!grepl("^House",input$racex) | !grepl("^House",input$racey)){
                if (input$fullstate){
                    for (i in 1:length(xx$AREA)){
                        xx$AREA[i] <- getState(xx$AREA[i])
                    }
                    xx$LABEL <- xx$AREA
                }
            }
            if (input$showrow){
                xx$LABEL <- paste0(xx$LABEL,"-",row.names(xx))
            }
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            if (input$fpop != ""){
                kpop <- 1000 * as.numeric(input$fpop)
                if (kpop > 0){
                    xx$LABEL[xx$TOT1_N < kpop] <- ""
                }
            }
            if (input$showlabels){
                if (input$party == "Democrat"){
                    gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                        color="red", hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Republican"){
                    gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                        color="red", hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Total"){
                    gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                        color="red", hjust = 0, vjust = xx$VJUST)
                }
                else{
                    gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                        color="red", hjust = 0, vjust = xx$VJUST)
                }
            }
            if (input$plotmean != "None"){
                xxr <- xx[xx$Party == vparty[1],]
                xxrx <- mean(xxr[[party1]])
                xxry <- mean(xxr[[party_sh]])
                xxd <- xx[xx$Party == vparty[length(vparty)],]
                xxdx <- mean(xxd[[party1]])
                xxdy <- mean(xxd[[party_sh]])
                meanxy <- c(xxrx,xxry,xxdx,xxdy)
                zmeanxy <<- meanxy #DEBUG-RM
                print(zmeanxy)
                zxxr <<- xxr #DEBUG-RM
                zxxd <<- xxd #DEBUG-RM
                gg <- gg + geom_point(aes(x=xxrx, y=xxry),color="black",shape=input$meanshape,size=input$meansize)
                gg <- gg + geom_point(aes(x=xxdx, y=xxdy),color="black",shape=input$meanshape,size=input$meansize)
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale != ""){
                sxx <- unlist(strsplit(input$xscale, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale != ""){
                syy <- unlist(strsplit(input$yscale, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }, height = 600, width = 1000)
        output$myPlot2 <- renderPlot({
            xx <- read_csv(paste0(input_dir,"1976-2018-house3.csv"))
            gxx2 <<- xx #DEBUG-RM
            minyear <- input$minyear
            maxyear <- input$maxyear
            cd <- "IA-1" #"FL-27"
            state2 <- input$state2_2
            dist <- as.numeric(input$district2)
            dvotes <- xx[xx$year >= minyear & xx$year <= maxyear & xx$party == "DEMOCRAT" &
                         xx$state_po == state2 & xx$district == dist,
                         c("year","candidatevotes","totalvotes")]
            names(dvotes) <- c("YEAR","DEM","DEMTOT")
            rvotes <- xx[xx$year >= minyear & xx$year <= maxyear & xx$party == "REPUBLICAN" &
                         xx$state_po == state2 & xx$district == dist,
                         c("year","candidatevotes","totalvotes")]
            names(rvotes) <- c("YEAR","REP","REPTOT")
            yy <- merge(dvotes,rvotes)
            yy$margin <- 100 * (yy$DEM/yy$DEMTOT - yy$REP/yy$REPTOT)
            gyy2 <<- yy #DEBUG-RM
            plot(yy$YEAR,yy$margin)
        })
        write_excel <- function(){
            races  <- c("House","Senate","Governor")
            models <- c("_deluxe","_classic","_lite")
            zraces  <- c("Hous","Sen","Gov")
            zmodels <- c("_dlux","_clas","_lite")
            dp <- input$decplaces
            if (input$tnote == ""){
                fileout <- paste0(input$measure,"_",input$yearx,"_",input$units,"_",dp,".xlsx")
            }
            else{
                fileout <- paste0(input$measure,"_",input$yearx,"_",input$units,"_",dp,"_",input$tnote,".xlsx")
            }
            append <- FALSE
            for (i in 1:3){
                for (j in 1:3){
                    dd <- getdatav(paste0(races[i],"_538"),races[i],models[j],models[j],input$units)
                    if (dp >= 0){
                        for (k in 2:NCOL(dd)){
                            dd[,k] <- format(round(dd[,k], dp), big.mark=",", scientific=FALSE)
                        }
                    }
                    sheet <- paste0(zraces[i],zmodels[j])
                    write.xlsx(dd, paste0(out_dir,fileout), sheetName = sheet, append = append)
                    append <- TRUE
                }
            }
            
        }
        output$myText <- renderPrint({
            dd <- getdata()
            if (!grepl("^House",input$racex) | !grepl("^House",input$racey)){
                if (input$fullstate){
                    for (i in 1:length(dd$AREA)){
                        dd$AREA[i] <- getState(dd$AREA[i])
                    }
                }
            }
            dd <- dd[,1:(NCOL(dd)-4)]
            # Format decimal numbers into character strings
            dp <- input$decplaces
            if (dp >= 0){
                for (i in 2:NCOL(dd)){
                    dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
                }
            }
            if (input$writeoutput){
                # fileout <- paste0(input$measure,"_",input$yearx,"_",input$racex,"_",
                #                   input$model,  "_",input$yeary,"_",input$racey,".xlsx")
                # sheet <- paste0(input$units)
                # write.xlsx(dd, paste0(out_dir,fileout), sheetName = sheet, append = TRUE)
                write_excel()
            }
            labels <- getlabels("text")
            cat(paste0(labels[1],"\n\n"))
            print(dd)
        })
        output$myVoteData <- renderPrint({
            states <- c("AZ","CA","FL","GA","IA","KY","ME","MI","MT","NC",
                        "PA","SC","TX")
            races <- c("President","President_538",
                       "Senate","Senate_538_lite","Senate_538_classic","Senate_538_deluxe",
                       "House","House_538_lite","House_538_classic","House_538_deluxe",
                       "Governor","Registered")
            stabbr2b <- c("", stabbr)
            years <- seq(input$year_first, input$year_last, by = input$year_step)
            files <- data.frame(matrix(ncol = length(years)+1, nrow = 0))
            colnames(files) <- c("Race", years)
            for (race in races){
                newrow <- race
                tot <- 0
                for (year in years){
                    filename <- paste0(data_dir,race,"_",year,".csv")
                    cnt <- 0
                    if (file.exists(filename)){
                        cnt <- length(readLines(filename)) - 1
                        tot <- tot + 1
                    }
                    newrow <- c(newrow, cnt)
                }
                if (tot > 0){
                    files[nrow(files)+1,] = newrow
                }
            }
            print(files)
            cat("\n")
            files <- data.frame(matrix(ncol = length(years)+1, nrow = 0))
            colnames(files) <- c("Saved Params", years)
            for (race in races){
                for (st in stabbr2b){
                    race_st <- paste0("Plot_",st,"_",race)
                    newrow <- race_st
                    tot <- 0
                    for (year in years){
                        filename <- paste0(data_dir,"Plot_",st,"_",race,"_",year,".csv")
                        cnt <- 0
                        if (file.exists(filename)){
                            cnt <- length(readLines(filename)) - 1
                            tot <- tot + 1
                        }
                        newrow <- c(newrow, cnt)
                    }
                    if (tot > 0){
                        files[nrow(files)+1,] = newrow
                    }
                }
            }
            for (race in races){
                for (st in stabbr2b){
                    race_st <- paste0("Map_",st,"_",race)
                    newrow <- race_st
                    tot <- 0
                    for (year in years){
                        filename <- paste0(data_dir,"Map_",st,"_",race,"_",year,".csv")
                        cnt <- 0
                        if (file.exists(filename)){
                            cnt <- length(readLines(filename)) - 1
                            tot <- tot + 1
                        }
                        newrow <- c(newrow, cnt)
                    }
                    if (tot > 0){
                        files[nrow(files)+1,] = newrow
                    }
                }
            }
            races <- c("Map_Parms", "Plot_Parms")
            for (race in races){
                newrow <- c(0, race)
                tot <- 0
                for (state in states){
                    filename <- paste0(data_dir,state,"_",race,".csv")
                    cnt <- 0
                    if (file.exists(filename)){
                        cnt <- length(readLines(filename)) - 1
                        tot <- tot + 1
                    }
                    newrow <- c(newrow, cnt)
                }
                if (tot > 0){
                    files[nrow(files)+1,] = newrow
                }
            }
            print(files)
        })
        output$myggMap <- renderLeaflet({
            dd <- getdata() # AREA,DEM1,REP1,MARGIN1,TOTAL1,DEM2,REP2,MARGIN2,TOTAL2,
                            # DEM_SH,REP_SH,MAR_SH,TOT_SH,FLIP,DEM1_N,REP1_N,MAR1_N,TOT1_N
            dd <- dd[dd$AREA != "TOTAL",]
            names(dd) <- c("AREA","DEM1","REP1","MARGIN1","TOTAL1",
                           "DEM2","REP2","MARGIN2","TOTAL2",
                           "DEM_SH","REP_SH","MAR_SH","TOT_SH","FLIP",
                           "DEM1_N","REP1_N","MAR1_N","TOT1_N")
            dd <- dd[dd$DEM1 != 0 & dd$REP1 != 0 & dd$DEM2 != 0 & dd$REP2 != 0,] #Include only 2-party races
            mapvar <- input$mapvar
            ee <- dd[!is.na(dd[[mapvar]]),]
            if (input$maplimitset == "Auto set to min,max"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                maplimits <- paste0(minlimit,",",maxlimit)
                updateTextInput(session, "maplimits", value = maplimits)
            }
            else if (input$maplimitset == "Auto set balanced"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                abslimit <- max(abs(minlimit), abs(maxlimit))
                stepsize <- ceiling(abslimit/5)
                abslimit <- ceiling(abslimit/stepsize) * stepsize
                maplimits <- paste0("-",abslimit,",",abslimit,",",stepsize)
                updateTextInput(session, "maplimits", value = maplimits)
            }
            limits <- unlist(strsplit(input$maplimits, ","))
            if (length(limits) <= 2){
                pal <- colorNumeric(input$mapcolors, dd[[mapvar]])
            }
            else if (length(limits) == 3){
                bins <- seq.int(limits[1], limits[2], limits[3])
                pal <- colorBin(input$mapcolors, domain = dd[[mapvar]], bins = bins)
            }
            else{
                bins <- limits
                pal <- colorBin(input$mapcolors, domain = dd[[mapvar]], bins = bins)
            }
            fipref <- "state.txt" # read from local memory
            ff <<- read.csv(fipref, sep = "|", stringsAsFactors = FALSE)
            if (!exists("cd116")){
                cd <- congressional_districts(cb = TRUE, resolution = '20m')
                for (i in 1:NROW(cd)){
                    cd$DISTRICT[i] <- paste0(
                        ff$STUSAB[ff$STATE == as.numeric(cd$STATEFP[i])],
                        "-",as.numeric(cd$CD116FP[i]))
                }
                cd$DISTRICT <- gsub("-0","-1",cd$DISTRICT)
                cd116 <<- cd
            }
            tag.map.title <- tags$style(HTML("
                  .leaflet-control.map-title {
                    !transform: translate(-50%,20%);
                    position: fixed !important;
                    left: 38%;
                    text-align: center;
                    padding-left: 10px; 
                    padding-right: 10px; 
                    background: rgba(255,255,255,0.75);
                    font-weight: normal;
                    font-size: 14px;
                  }
            "))
            labels <- getlabels("map")
            line2 <- "Sources: see http://econdataus.com/voting_538.htm"
            if (input$mapvar == "FLIP"){
                if (input$xclose == "0"){
                    line2 <- paste0("<b>Flipped by shift</b> - ",line2)
                }
                else{
                    line2 <- paste0("<b>Flipped or within ",input$xclose,"%</b> - ",line2)
                }
            }
            titletext <- paste0("<b>",labels[1],"</b><br>",line2)
            title <- tags$div(
                tag.map.title, HTML(titletext)
            )  
            if (grepl("^House",input$racex) & grepl("^House",input$racey)){
                
                dd <- cd116 %>%
                    left_join(dd, by = c("DISTRICT" = "AREA"))
                leaflet(dd) %>%
                    addTiles() %>%
                    addControl(title, position = "topright", className="map-title") %>%
                    setView(-96, 37.8, 4) %>%
                    addLegend(pal = pal, values = dd[[mapvar]], opacity = 0.7, title = NULL,
                              position = "bottomright") %>%
                    addPolygons(
                        fillColor = ~pal(dd[[mapvar]]),
                        weight = 1, #was 2
                        opacity = 1,
                        color = "white", #was "white"
                        dashArray = "1", #was "3"
                        fillOpacity = 0.7,
                        popup = paste0(dd$DISTRICT,"<br>",round(dd[[mapvar]],2))
                    )
            }
            else{
                names(dd)[1] <- "AREA"
                lstates <- states(cb=T)
                dd <- lstates %>%
                    left_join(dd, by = c("STUSPS" = "AREA")) # match on state abbv
                leaflet(dd) %>%
                    addTiles() %>%
                    addControl(title, position = "topright", className="map-title") %>%
                    setView(-96, 37.8, 4) %>%
                    addLegend(pal = pal, values = dd[[mapvar]], opacity = 0.7, title = NULL,
                              position = "bottomright") %>%
                    addPolygons(
                        fillColor = ~pal(dd[[mapvar]]),
                        weight = 1, #was 2
                        opacity = 1,
                        color = "white", #was "white"
                        dashArray = "1", #was "3"
                        fillOpacity = 0.7,
                        popup = paste0(dd$NAME,"<br>",round(dd[[mapvar]],2))
                    )
            }
        })
        #############################################################
        # getdata
        #############################################################
        getdata <- reactive({
            if (input$createfiles){
                ayears <- seq(1976,2010,2)
                cyears <- input$yearx
                if (input$yearx != input$yeary){
                    cyears <- c(cyears, input$yeary)
                }
                for (year in cyears){
                    if (year == 2022){
                        create538_22("_deluxe")
                        create538_22("_classic")
                        create538_22("_lite")
                        createHouse22()
                        createSenate22()
                        createGovernor22()
                    }
                    else if (year == 2020){
                        create538_20("_deluxe")
                        create538_20("_classic")
                        create538_20("_lite")
                        createPresident20()
                        createSenate20()
                        createHouse20()
                    }
                    else if (year == 2018){
                        createHouse538_18("deluxe")
                        createHouse538_18("classic")
                        createHouse538_18("lite")
                        createHouseNN(year)
                    }
                    else if (year == 2016){
                        createPresident16()
                        createHouseNN(year)
                    }
                    else if (year == 2014){
                        createHouseNN(year)
                    }
                    else if (year == 2012){
                        createPresident12()
                        createHouseNN(year)
                    }
                    else if (year == 2004){
                        createPresident04()
                        createHouseNN(year)
                    }
                    else if (year %in% ayear){
                        createHouseNN(year)
                    }
                }
            }
            if (input$modely == "(same as above)"){
                modely <- input$model
            }
            else{
                modely <- input$modely
            }
            dd <- getdatav(input$racex,input$racey,input$model,modely,input$units)
            return(dd)
        })
        getdatav <- function(racex,racey,model,modely,units){
            if (input$flipy){
                msh1 <- -1
                msh100 <- -100
            }
            else{
                msh1 <- 1
                msh100 <- 100
            }
            if (grepl("House_538$", racex) | grepl("Senate_538$", racex) |
                grepl("President_538$", racex) | grepl("Governor_538$", racex)){
                filenamex <- paste0(data_dir,racex,model,"_",input$yearx,".csv")
            }
            else{
                filenamex <- paste0(data_dir,racex,"_",input$yearx,".csv")
            }
            if (grepl("House_538$", racey) | grepl("Senate_538$", racey) |
                grepl("President_538$", racey) | grepl("Governor_538$", racey)){
                filenamey <- paste0(data_dir,racey,modely,"_",input$yeary,".csv")
            }
            else{
                filenamey <- paste0(data_dir,racey,"_",input$yeary,".csv")
            }
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            yyparty <- read_delim(filenamey, ' ', col_names = FALSE, n_max = 1)
            
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where AREA starts with X_
            xx0 <- xx0[,!grepl("^X_",xxparty)]
            xxparty <- xxparty[,!grepl("^X_",xxparty)]
            xx0 <- xx0[ !grepl("^X_",xx0$AREA),]
            
            yy0 <- read_delim(filenamey, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where AREA starts with X_
            yy0 <- yy0[,!grepl("^X_",yyparty)]
            yyparty <- yyparty[,!grepl("^X_",yyparty)]
            yy0 <- yy0[ !grepl("^X_",yy0$AREA),]
            
            xx0$MARGIN1 <- 0
            xx0$TOTAL1 <- rowSums(xx0[,2:(NCOL(xx0)-1)], na.rm = TRUE) # excludes MARGIN1
            idem <- which(xxparty == "DEM")
            irep <- which(xxparty == "REP")
            xx <- xx0[,c(1,idem,irep,NCOL(xx0)-1,NCOL(xx0))] # AREA,DEM,REP,MARGIN1,TOTAL1
            names(xx)[2] <- paste0(names(xx[2]),"1")
            names(xx)[3] <- paste0(names(xx[3]),"1")
            
            yy0$MARGIN2 <- 0
            yy0$TOTAL2 <- rowSums(yy0[,2:(NCOL(yy0)-1)], na.rm = TRUE) # excludes MARGIN2
            idem <- which(yyparty == "DEM")
            irep <- which(yyparty == "REP")
            yy <- yy0[,c(1,idem,irep,NCOL(yy0)-1,NCOL(yy0))] # AREA,DEM,REP,MARGIN2,TOTAL2
            if (input$state2 != ""){
                states2 <- unlist(strsplit(input$state2, ","))
                xx <- xx[substr(xx$AREA,1,2) %in% states2,]
                yy <- yy[substr(yy$AREA,1,2) %in% states2,]
            }
            names(yy)[2] <- paste0(names(yy[2]),"2")
            names(yy)[3] <- paste0(names(yy[3]),"2")
            
            if (input$showall){
                # xx$DEM1[is.na(xx$DEM1)] <- 1
                # xx$REP1[is.na(xx$REP1)] <- 1
                # yy$DEM2[is.na(yy$DEM2)] <- 1
                # yy$REP2[is.na(yy$REP2)] <- 1
                dd <- as.data.frame(merge(xx, yy, by = "AREA", all = TRUE))
                dd$na1 <- is.na(dd$DEM1) & is.na(dd$REP1) & is.na(dd$MARGIN1) & is.na(dd$TOTAL1)
                dd$na2 <- is.na(dd$DEM2) & is.na(dd$REP2) & is.na(dd$MARGIN2) & is.na(dd$TOTAL2)
                dd$DEM1[dd$na1] <- dd$TOTAL2[dd$na1] / 2
                dd$REP1[dd$na1] <- dd$TOTAL2[dd$na1] / 2
                dd$MARGIN1[dd$na1] <- 0
                dd$TOTAL1[dd$na1] <- dd$TOTAL2[dd$na1]
                dd$DEM2[dd$na2] <- dd$TOTAL1[dd$na2] / 2
                dd$REP2[dd$na2] <- dd$TOTAL1[dd$na2] / 2
                dd$MARGIN2[dd$na2] <- 0
                dd$TOTAL2[dd$na2] <- dd$TOTAL1[dd$na2]
                dd$DEM1[is.na(dd$DEM1)] <- 1
                dd$REP1[is.na(dd$REP1)] <- 1
                dd$DEM2[is.na(dd$DEM2)] <- 1
                dd$REP2[is.na(dd$REP2)] <- 1
                dd <- dd[,1:9]
                dd2 <<- dd
            }
            else{
                dd <- as.data.frame(merge(xx, yy, by = "AREA"))
            }
            ddnames <- names(dd)
            names(dd) <- c("AREA","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            dd3 <<- dd
            ddtot <- data.frame("TOTAL",sum(dd$DEM1,na.rm=TRUE),sum(dd$REP1,na.rm=TRUE),
                                sum(dd$MARGIN1,na.rm=TRUE),sum(dd$TOTAL1,na.rm=TRUE),
                                sum(dd$DEM2,na.rm=TRUE),sum(dd$REP2,na.rm=TRUE),
                                sum(dd$MARGIN2,na.rm=TRUE),sum(dd$TOTAL2,na.rm=TRUE))
            names(ddtot) <- names(dd)
            if (!input$model %in% c("_exitpoll","_combined","_polls") &
                !input$modely %in% c("_exitpoll","_combined","_polls")){
                dd <- rbind(dd, ddtot)
            }
            dd4 <<- dd
            DEM1_N <- dd$DEM1
            REP1_N <- dd$REP1
            MAR1_N <- dd$MARGIN1
            if (input$vuse2){
                TOT1_N <- dd$TOTAL2
            }
            else{
                TOT1_N <- dd$TOTAL1
            }
            dd$MARGIN1 <- dd$DEM1 - dd$REP1
            dd$MARGIN2 <- dd$DEM2 - dd$REP2
            if (input$dronly){
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
                if (input$showall){
                    dd$TOTAL1[dd$TOTAL1 == 0] <- 1
                    dd$TOTAL2[dd$TOTAL2 == 0] <- 1
                }
            }
            dd5 <<- dd
            if (racey == "Registered"){
                if (input$measure == "Percent change"){
                    dd$DEM_SH <- msh100 * (dd$DEM2 - dd$DEM1) / dd$DEM1
                    dd$REP_SH <- msh100 * (dd$REP2 - dd$REP1) / dd$REP1
                    dd$MAR_SH <- msh100 * (dd$MARGIN2 - dd$MARGIN1) / dd$MARGIN1
                    dd$TOT_SH <- msh100 * (dd$TOTAL2 - dd$TOTAL1) / dd$TOTAL1
                }
                else if (input$measure == "Percent ratio"){
                    dd$DEM_SH <- msh100 * dd$DEM1 / dd$DEM2
                    dd$REP_SH <- msh100 * dd$REP1 / dd$REP2
                    dd$MAR_SH <- msh100 * dd$MARGIN1 / dd$MARGIN2
                    dd$TOT_SH <- msh100 * dd$TOTAL1 / dd$TOTAL2
                }
                else{	
                    dd$DEM_SH <- msh1 * (dd$DEM2 - dd$DEM1)
                    dd$REP_SH <- msh1 * (dd$REP2 - dd$REP1)
                    dd$MAR_SH <- msh1 * (dd$MARGIN2 - dd$MARGIN1)
                    dd$TOT_SH <- msh1 * (dd$TOTAL2 - dd$TOTAL1)
                }
            }
            if (units == "Percent"){
                if (min(dd$REP1[!is.na(dd$REP1)]) == 0 & max(dd$REP1[!is.na(dd$REP1)]) == 0){
                    dd$MARGIN1 <- dd$DEM1
                    dd$REP1 <- 50 - (dd$DEM1/2)
                    dd$DEM1 <- 50 + (dd$DEM1/2)
                }
                else if (input$model == "_exitpoll"){
                    #dd$MARGIN1 <- dd$DEM1 - dd$REP
                }
                else{
                    dd$DEM1 <- 100 * dd$DEM1 / dd$TOTAL1
                    dd$REP1 <- 100 * dd$REP1 / dd$TOTAL1
                    dd$MARGIN1 <- 100 * dd$MARGIN1 / dd$TOTAL1
                }
                if (min(dd$REP2[!is.na(dd$REP2)]) == 0 & max(dd$REP2[!is.na(dd$REP2)]) == 0){
                    dd$MARGIN2 <- dd$DEM2
                    dd$REP2 <- 50 - (dd$DEM2/2)
                    dd$DEM2 <- 50 + (dd$DEM2/2)
                }
                else{
                    dd$DEM2 <- 100 * dd$DEM2 / dd$TOTAL2
                    dd$REP2 <- 100 * dd$REP2 / dd$TOTAL2
                    dd$MARGIN2 <- 100 * dd$MARGIN2 / dd$TOTAL2
                }
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
            }
            dd6 <<- dd
            if (racey != "Registered"){
                if (input$measure == "Percent change"){
                    dd$DEM_SH <- msh100 * (dd$DEM2 - dd$DEM1) / dd$DEM1
                    dd$REP_SH <- msh100 * (dd$REP2 - dd$REP1) / dd$REP1
                    dd$MAR_SH <- msh100 * (dd$MARGIN2 - dd$MARGIN1) / dd$MARGIN1
                    dd$TOT_SH <- msh100 * (dd$TOTAL2 - dd$TOTAL1) / dd$TOTAL1
                }
                else if (input$measure == "Percent ratio"){
                    dd$DEM_SH <- msh100 * dd$DEM1 / dd$DEM2
                    dd$REP_SH <- msh100 * dd$REP1 / dd$REP2
                    dd$MAR_SH <- msh100 * dd$MARGIN1 / dd$MARGIN2
                    dd$TOT_SH <- msh100 * dd$TOTAL1 / dd$TOTAL2
                }
                else{
                    dd$DEM_SH <- msh1 * (dd$DEM2 - dd$DEM1)
                    dd$REP_SH <- msh1 * (dd$REP2 - dd$REP1)
                    dd$MAR_SH <- msh1 * (dd$MARGIN2 - dd$MARGIN1)
                    dd$TOT_SH <- msh1 * (dd$TOTAL2 - dd$TOTAL1)
                }
            }
            dd7 <<- dd
            names(dd)[1:9] <- ddnames
            xclose <- unlist(strsplit(input$xclose, ","))
            if (length(xclose) == 1){
                xclose1 <- as.numeric(xclose[1])
                xclose2 <- as.numeric(xclose[1])
            }
            else if (length(xclose) <= 2){
                xclose1 <- as.numeric(xclose[1])
                xclose2 <- as.numeric(xclose[2])
            }
            else{
                xclose1 <- 0
                xclose2 <- 0
            }
            skipflip <- FALSE
            if (!input$showflips){
                xclose1 <- abs(xclose1)
                xclose2 <- abs(xclose2)
                skipflip <- TRUE
            }
            dd$FLIP <- NA
            for (i in 1:NROW(dd)){
                if (!is.na(dd$MARGIN1[i]) & !is.na(dd$MARGIN2[i])){
                    if (skipflip){
                        if (((dd$MARGIN1[i] < 0 & dd$MARGIN2[i] < 0) |
                             (dd$MARGIN1[i] > 0 & dd$MARGIN2[i] > 0)) &
                            ((abs(dd$MARGIN1[i]) <= xclose1) |
                             (abs(dd$MARGIN2[i]) <= xclose2))){
                            dd$FLIP[i] <- dd$MAR_SH[i]
                        }
                    }
                    else{
                        if ((dd$MARGIN1[i] <= 0 & dd$MARGIN2[i] >= 0) |
                            (dd$MARGIN1[i] >= 0 & dd$MARGIN2[i] <= 0) |
                            (abs(dd$MARGIN1[i]) <= xclose1) |
                            (abs(dd$MARGIN2[i]) <= xclose2)){
                            dd$FLIP[i] <- dd$MAR_SH[i]
                        }
                    }
                }
            }
            dd$DEM1_N <- DEM1_N
            dd$REP1_N <- REP1_N
            dd$MAR1_N <- MAR1_N
            dd$TOT1_N <- TOT1_N
            if (input$xsort != "(no sort)"){
                if (input$xsortdir == "Ascending"){
                    dd <- dd[order(dd[[input$xsort]]),]
                }
                else{
                    if (class(dd[[input$xsort]]) == "numeric"){
                        dd <- dd[order(-dd[[input$xsort]]),]
                    }
                    else{
                        dd <- dd[order(dd[[input$xsort]]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            gdd <<- dd #DEBUG-RM
            return(dd)
        }
        observe({
            eventid <- "Plot"
            loadid <- "plotload"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            parmup <- c("checkbox", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "select",
                        "vlimit", "vshape", "vdesc")
            if (grepl("House_538$", input$racex) | grepl("Senate_538$", input$racex)){
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,input$model,"_",input$yearx,".csv")
            }
            else{
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,"_",input$yearx,".csv")
            }
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = as.logical(pp$value[pp$label == parmid[i]]))
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$plotsave,{
            eventid <- "Plot"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            if (grepl("House_538$", input$racex) | grepl("Senate_538$", input$racex)){
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,input$model,"_",input$yearx,".csv")
            }
            else{
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,"_",input$yearx,".csv")
            }
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Map"
            loadid <- "mapload"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            parmup <- c("numeric", "numeric", "skipcity",
                        "showcity", "select", "maplimits",
                        "numeric","select","mapcolors")
            if (grepl("House_538$", input$racex) | grepl("Senate_538$", input$racex)){
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,input$model,"_",input$yearx,".csv")
            }
            else{
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,"_",input$yearx,".csv")
            }
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$mapsave,{
            eventid <- "Map"
            parmid <- c("maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            if (grepl("House_538$", input$racex) | grepl("Senate_538$", input$racex)){
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,input$model,"_",input$yearx,".csv")
            }
            else{
                filename <- paste0(data_dir,eventid,"_",input$state2,"_",input$racex,"_",input$yearx,".csv")
            }
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observeEvent(input$yearx,{
            if (input$yearx == 2016 | input$yearx == 2004){
                updateSelectInput(session=session,"model","538 Model",
                                  choices = c("_exitpoll","_combined","_polls"),
                                  selected = "_exitpoll")
            }
            else{
                updateSelectInput(session=session,"model","538 Model",
                                  choices = c("_lite","_classic","_deluxe"),
                                  selected = "_deluxe")
            }
        })
        observe({
            cat(file=stderr(), paste0("v3: ",input$state2," ",input$tabs,"\n"))
        })
    }
)
