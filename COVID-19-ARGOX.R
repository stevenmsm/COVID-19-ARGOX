#' daily data to train, predict next 7 day, weekly evaluation

# load data -----------------------------------------------------------------------------------------------------------------------------------------------
library(data.table)
library(xts)
library(argo)
library(forecast)

setwd("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/ARGOX")
us_national <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") #New York Times 
us_states <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

google_mobility <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
# relative volume of directions requests per country/region, sub-region or city compared to a baseline volume (Jan13 2020)
apple_mobility <- read.csv("~/applemobilitytrends-2021-04-03.csv") 
apple_mobility = setDT(apple_mobility)

us_national <- data.table(us_national)
us_national$new_cases <- c(0, diff(us_national$cases)) #new cases=lag of totalcases (today-yesterday)
us_national$daily_deaths <- c(0, diff(us_national$deaths))
us_national$date <- as.Date(us_national$date)
plot(us_national$date, us_national$new_cases)
plot(us_national$date, us_national$daily_deaths)

gt.folder <- "~/US-covid19-api_raw_results-2021-03-28"
files <- list.files(gt.folder) #get all files name as string and combine as list (in order to use lapply)
f <- files[1]

gt.parser.pub.api <- function(gt.folder, f){
  gtdata <- read.csv(file.path(gt.folder, f)) #get search word (common cold) frequency csv file
  f_info <- strsplit(f, "_")[[1]] #get the area of the file ("US" or US-State")
  state <- f_info[1]
  gtdata <- xts(gtdata[,ncol(gtdata),drop=FALSE], as.Date(gtdata$date)) #pick last column (frequency of searched word) and create a time-series object (index is dates)
  if(all(diff(index(gtdata))==7)){ #if each row is one week difference
    index(gtdata) <- index(gtdata) + 6 
    gtfreq <- "week"
  }else if (all(diff(index(gtdata))==1)){ #if each row is one day difference
    gtfreq <- "day"
  }else{ #if each row is one month difference
    index(gtdata) <- as.yearmon(index(gtdata))
    gtfreq <- "month"
  }
  term <- colnames(gtdata) #get the search word (string)
  term <- gsub(".*\\.m\\.", "",term)  #clean up added chars in the search word (usually occur due to space); replace un-need chars with no space
  term <- gsub("^\\X.", "",term)   #replace un-need chars with no space
  if(!grepl(term, gsub(" ",".",f))){  #each freq file's column name(search word) should be the same as filename, if not then output that filename
    cat(f,colnames(gtdata),"\n")  #output that filename if search query doesnt match with file name
  }
  #return the state label (string), a time series object (vector), and a string for frequency
  #later will need "US" or US-state" for example "US-AL"
  return(list(state=state, gtdata=gtdata, gtfreq=gtfreq))  
}

raw_data_parsed <- lapply(files, function(f) gt.parser.pub.api(gt.folder, f)) #apply above function on all elements in the list "files", output each search query file in a list, thus this is a list embedded with a series of lists
all_geo <- sapply(raw_data_parsed, function(x) x$state)   #extract all results' states ("US" or "US-state")
gtdata_all <- lapply(raw_data_parsed, function(x) x$gtdata)   #get frequencies of all words searched, still keep in list (no month or week only day), started since 2019-01-01

# tapply applies some function to the first input argument based on the levels of the second input argument. First and second argument are same size (row and columns). Here, there are different levels in all_geo ("US","US-AK","US-AL",...).
gtdata_daily_state <- tapply(gtdata_all, all_geo, function(gt.eachstate){ #for each all_geo levels, there is a list of search query frequency. For example, first level: "US" has first US_commoncold, US_coronavirus,.... under that level.
  tab <- do.call(merge, gt.eachstate)                                     #tapple recognize which arrays in gtdata_all belongs to which level in all_geo and merge gtdata_all based on these levels, i.e. under "US" list now have all its searched words frequencies, each word as its own list
  tab[,!grepl("\\.1$",colnames(tab))]                                     #return in big list of lists (regions), each list is a matrix: rows are dates, columns are word frequences for each word (column) in that region, i.e list one is matrix of "US", each columns are search freq of words under the level "US"   
})

# Get national level death and cases and State level death and cases
covid_nat_incre <- xts(us_national[,.(new_cases, daily_deaths)], us_national$date) #select national new cases and daily death, make date as index
# Get State level death and cases
region_name_mapping = unique(google_mobility[country_region_code=="US" & iso_3166_2_code != "",.(sub_region_1, iso_3166_2_code)]) #get all U.S. states name and abbreviated name
not_counted_region <- unique(us_states$state)[(!unique(us_states$state) %in% region_name_mapping$sub_region_1)] #find the 4 regions not included, i.e. Puerto Rico
not_counted_region <- as.character(not_counted_region)
us_states = subset(us_states, !(state %in% c(not_counted_region))) #after removing the five regions above
region_code_all = names(gtdata_daily_state)
region_code_all = region_code_all[region_code_all!="US"] #remove "US" as it is not one of the 51 regions
covid_state_level = list()
for (region_code in region_code_all){ #loop through all regions in the order of names(gtdata_daily_state), the list contains mobility and search query
  region_full_name = region_name_mapping[region_name_mapping$iso_3166_2_code %in% region_code]$sub_region_1 #get the full name
  us_states_temp = us_states[us_states$state %in% region_full_name,] #select the given region's cases and deaths
  us_states_temp <- data.table(us_states_temp) #convert to table for easier transite to xts
  us_states_temp$new_cases <- c(0, diff(us_states_temp$cases)) #new cases=lag of totalcases (today-yesterday)
  us_states_temp$daily_deaths <- c(0, diff(us_states_temp$deaths)) #new death=lag of totaldeath (today-yesterday)
  us_states_temp$date <- as.Date(us_states_temp$date) #get date
  covid_state_temp <- xts(us_states_temp[,.(new_cases, daily_deaths)], us_states_temp$date)
  covid_state_level[[region_code]] <- covid_state_temp #put the state level new cases and death in the list in the same order as gtdata_daily_state
}
for (region_code in region_code_all){ #Check if all state's new deaths and cases are index by daily
  if (!all(diff(index(covid_state_level[[region_code]]))==1)){
    print(paste0("ERROR, Daily count not index with difference of 1 in ",region_code," !!"))
  }
}
covid_state_level[["US"]] <- covid_nat_incre #add "US" new deaths and cases into covid_state_level list
covid_state_level = covid_state_level[names(gtdata_daily_state)] #order new cases and death in the list in the same order as gtdata_daily_state
for (region_code in  names(gtdata_daily_state)){ #loop over all state's new death and cases and add some earlier dates and force same number of rows/dates
  #region_code = "US"
  temp = cbind(gtdata_daily_state[[region_code]], covid_state_level[[region_code]])
  covid_state_temp = cbind(temp$new_cases,temp$daily_deaths)
  covid_state_temp = covid_state_temp[-(1:(which(index(covid_state_temp)=="2020-01-21")-1)),]
  covid_state_temp[is.na(covid_state_temp)] = 0
  covid_state_level[[region_code]] = covid_state_temp
  if (region_code == "US-NJ"){ #2020/6/25 New Jersey added additional 1854 Accumulative probable death from COVID-19 to that day. We remove that for now.
    covid_state_level[[region_code]]["2020-06-25",]$daily_deaths = covid_state_level[[region_code]]["2020-06-25",]$daily_deaths - 1854
  }
}


######################################################################################################################################################
# Import Age and Sex
testing_states = names(gtdata_daily_state)
population.file <- "~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/ARGOX/Population.csv" #gives population of each state and which region it belongs
state_region_info <- fread(population.file)
state_region_info$Population <- as.numeric(gsub(",", "", state_region_info$Population))
state_names = setdiff(testing_states, c("US")) #get rid of US, that is national level

# Construct regional information (linear combination of the states' information in that region)
population.file <- "~/Population.csv" #gives population of each state and which region it belongs
state_region_info <- fread(population.file)
state_region_info$Population <- as.numeric(gsub(",", "", state_region_info$Population))
state_names = setdiff(testing_states, c("US")) #get rid of US, that is national level
gtdata_regional <- lapply(1:10, function(region_number) {
  states_id <- which(state_region_info$Region == region_number)
  GT_states_wgted <- lapply(states_id, function(j) {
    gtdata_daily_state[[ paste0("US-",state_region_info$Abbre[j]) ]] * state_region_info$Population[j]/sum(state_region_info$Population[states_id])
  })
  Reduce("+", GT_states_wgted)
})
names(gtdata_regional) <- paste0("Region-", 1:10)

# Construct regional daily death, by simply adding up the states daily deaths thats in the region
covid_regional_level <- lapply(1:10, function(region_number) {
  states_id <- which(state_region_info$Region == region_number)
  covid_states_wgted <- lapply(states_id, function(j) {
    covid_state_level[[ paste0("US-",state_region_info$Abbre[j]) ]] #simply aggregate?Not linear combination with population portion???
  })
  Reduce("+", covid_states_wgted)
})
names(covid_regional_level) <- paste0("Region-", 1:10)

#Here we modify the Google Search query freqency for state level using a linear combination of the statelevel and regional level
#Here we set mix=1/3, where statelevel has weight 2/3 and its specific region has weight 1/3. Assuming that the spacial impact is 1/3. 
gtdata_daily_state_MIXED <- list()
mix <- 1/3 
for (each_state in state_names){ #Note that strsplit if parsing "." then either use "\\." or "[.] or "fixed=TRUE"
  region_id_for_state = state_region_info[Abbre==strsplit(each_state, "-")[[1]][2], Region] #get the region idx (range from 1 to 10) for specific state 
  gtdata_daily_state_MIXED[[each_state]] = (gtdata_daily_state[[each_state]] * (1-mix) + gtdata_regional[[paste0("Region-",region_id_for_state)]] * mix)
}

#national GT is the same 
gtdata_national = gtdata_daily_state[["US"]]
covid_national_level = covid_state_level[["US"]]

# Force negative death to be zero
for (idx in names(covid_state_level)){
  covid_state_level[[idx]][covid_state_level[[idx]]$daily_deaths<0,2]=0
}
for (idx in names(covid_regional_level)){
  covid_regional_level[[idx]][covid_regional_level[[idx]]$daily_deaths<0,2]=0
}
covid_national_level[covid_national_level$daily_deaths<0,2]=0
covid_national_level["2020-06-25",]$daily_deaths = covid_national_level["2020-06-25",]$daily_deaths-1854 #Remove Probable accumulative death of 1854 from New Jersey at 2020-06-25 for now.

######################################################################################################################################################
## DATA FILTERING
######################################################################################################################################################
# Drop the google search terms that have frequency lower than 50,000. Filter out too low and too high values using IQR, while checking its neighbours.
# NOTE THAT all regions (national, regional, states) should have same length of dates (index)
dropped_terms = list()
gtdata_original = gtdata_daily_state #store un-IQR and Media filtered JUST IN CASE
gtdata_mixed_original = gtdata_daily_state_MIXED
gtdata_regional_original = gtdata_regional
# WORKING on national and state level (not mixed) here. FILTERING
for (regions in names(gtdata_daily_state)){
  terms_workon = names(gtdata_daily_state[[regions]])
  for (terms in terms_workon){
    gtdata_daily_state[[regions]][is.na(gtdata_daily_state[[regions]][,terms]),terms] = 0
    #if the search term is below 50 search volume OR if there are more than 70% 0s OR if search volume greater than 50 is less than 10% of total days. DELETE
    if (max(gtdata_daily_state[[regions]][,terms])<=50 || length(which(gtdata_daily_state[[regions]][,terms]!=0)) <(0.3*length(gtdata_daily_state[[regions]][,terms])) || length(which(gtdata_daily_state[[regions]][,terms]>50))< (0.1*length(gtdata_daily_state[[regions]][,terms])) ){ 
      dropped_terms[[regions]] = c(dropped_terms[[regions]],as.character(terms))
    }
    else{ #if not, filter out its outliers
      start_filter_date = which(index(gtdata_daily_state[[regions]][,terms]) == "2020-05-01")
      end_filter_date = length(gtdata_daily_state[[regions]][,terms])
      #large valued outlier
      idx_high_outlier = which(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms] >= quantile(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms],0.999))
      idx_high_outlier = start_filter_date+idx_high_outlier-1
      if (!length(idx_high_outlier)==0){
        for (idx in 1:length(idx_high_outlier)){
          if (abs(end_filter_date-idx_high_outlier[idx])<=2){
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]-2))
          }
          else{
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]+1),c(idx_high_outlier[idx]+2))
          }
          high_outlier_neighbour_P={}
          for (i in 1:length(idx_high_neighbour)){ #compute the P-values of high outlier's neighbours by assuming Normal Distribution with mean equal to the outliers, covariance equal to the empirical covariance of the google search term series 
            high_outlier_neighbour_P = c(high_outlier_neighbour_P,pnorm(gtdata_daily_state[[regions]][idx_high_neighbour[i],terms],gtdata_daily_state[[regions]][idx_high_outlier[idx],terms],sqrt(cov(gtdata_daily_state[[regions]][,terms]))))
          }
          if (all(high_outlier_neighbour_P<0.05)){ #if there are hight outliers via IQR and their neighbours have p-values less than 5%, replace the outliers via previous two day average
            gtdata_daily_state[[regions]][idx_high_outlier[idx],terms] = (as.matrix(gtdata_daily_state[[regions]][idx_high_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_high_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_high_outlier[idx]-3,terms]) )/3
          }
        }
      }
      #Small valued outlier. Less strict than high valued outlier above. No need to use p-value
      idx_low_outlier = which(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms] <= quantile(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms],0.01))
      idx_low_outlier = start_filter_date + idx_low_outlier - 1
      if (!length(idx_low_outlier)==0){
        for (idx in 1:length(idx_low_outlier)){
          gtdata_daily_state[[regions]][idx_low_outlier[idx],terms] = (as.matrix(gtdata_daily_state[[regions]][idx_low_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_low_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_low_outlier[idx]-3,terms]) )/3
        }
      }
    }
  }
}
names(dropped_terms) = names(gtdata_daily_state) #name the list dropped_terms with region names
for (regions in names(gtdata_daily_state)){ #DELETE the terms with lower than 50 search volume
  gtdata_daily_state[[regions]] = gtdata_daily_state[[regions]][,-which(names(gtdata_daily_state[[regions]])%in% unlist(dropped_terms[regions]))]
  # 2020-08-06 these two terms have a spike, REMOVE
  gtdata_daily_state[[regions]]["2020-08-06","X.covid.19."] = gtdata_daily_state[[regions]]["2020-08-05","X.covid.19."]
  gtdata_daily_state[[regions]]["2020-08-06","covid.19"] = gtdata_daily_state[[regions]]["2020-08-05","covid.19"]
}
gtdataALL_IQR_noMedia = gtdata_daily_state #only IQR filtered

#*************************************************************************************************************************************
# WORKING on Regional here!
dropped_terms = list()
for(regions in names(gtdata_regional)){
  terms_workon = names(gtdata_regional[[regions]])
  for (terms in terms_workon){
    gtdata_regional[[regions]][is.na(gtdata_regional[[regions]][,terms]),terms] = 0
    #if the search term is below 50 search volume OR if there are more than 70% 0s OR if search volume greater than 50 is less than 2% of total days. DELETE
    if (max(gtdata_regional[[regions]][,terms])<=50 || length(which(gtdata_regional[[regions]][,terms]!=0)) <(0.3*length(gtdata_regional[[regions]][,terms])) || length(which(gtdata_regional[[regions]][,terms]>50))< (0.1*length(gtdata_regional[[regions]][,terms])) ){ 
      dropped_terms[[regions]] = c(dropped_terms[[regions]],as.character(terms))
    }
    else{ #if not, filter out its outliers
      start_filter_date = which(index(gtdata_regional[[regions]][,terms]) == "2020-05-01")
      end_filter_date = length(gtdata_regional[[regions]][,terms])
      #large valued outlier
      idx_high_outlier = which(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms] >= quantile(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms],0.999))
      idx_high_outlier = start_filter_date+idx_high_outlier-1
      if (!length(idx_high_outlier)==0){
        for (idx in 1:length(idx_high_outlier)){
          if (abs(end_filter_date-idx_high_outlier[idx])<=2){
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]-2))
          }
          else{
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]+1),c(idx_high_outlier[idx]+2))
          }
          high_outlier_neighbour_P={}
          for (i in 1:length(idx_high_neighbour)){ #compute the P-values of high outlier's neighbours by assuming Normal Distribution with mean equal to the outliers, covariance equal to the empirical covariance of the google search term series 
            high_outlier_neighbour_P = c(high_outlier_neighbour_P,pnorm(gtdata_regional[[regions]][idx_high_neighbour[i],terms],gtdata_regional[[regions]][idx_high_outlier[idx],terms],sqrt(cov(gtdata_regional[[regions]][,terms]))))
          }
          if (all(high_outlier_neighbour_P<0.05)){ #if there are hight outliers via IQR and their neighbours have p-values less than 5%, replace the outliers via previous two day average
            gtdata_regional[[regions]][idx_high_outlier[idx],terms] = (as.matrix(gtdata_regional[[regions]][idx_high_outlier[idx]-1,terms]) + as.matrix(gtdata_regional[[regions]][idx_high_outlier[idx]-2,terms]) + as.matrix(gtdata_regional[[regions]][idx_high_outlier[idx]-3,terms]) )/3
          }
        }
      }
      #Small valued outlier. Less strict than high valued outlier above. No need to use p-value
      idx_low_outlier = which(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms] <= quantile(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms],0.01))
      idx_low_outlier = start_filter_date + idx_low_outlier - 1
      if (!length(idx_low_outlier)==0){
        for (idx in 1:length(idx_low_outlier)){
          gtdata_regional[[regions]][idx_low_outlier[idx],terms] = (as.matrix(gtdata_regional[[regions]][idx_low_outlier[idx]-1,terms]) + as.matrix(gtdata_regional[[regions]][idx_low_outlier[idx]-2,terms]) + as.matrix(gtdata_regional[[regions]][idx_low_outlier[idx]-3,terms]) )/3
        }
      }
    }
  }
}
names(dropped_terms) = names(gtdata_regional) #name the list dropped_terms with region names
for (regions in names(gtdata_regional)){ #DELETE the terms with lower than 50 search volume
  gtdata_regional[[regions]] = gtdata_regional[[regions]][,-which(names(gtdata_regional[[regions]])%in% unlist(dropped_terms[regions]))]
}
gtdata_Region_IQR_noMedia = gtdata_regional #only IQR filtered

#*************************************************************************************************************************************
# WORKING on Mixed State Levels here!
dropped_terms = list()
for (regions in names(gtdata_daily_state_MIXED)){
  terms_workon = names(gtdata_daily_state_MIXED[[regions]])
  for (terms in terms_workon){
    gtdata_daily_state_MIXED[[regions]][is.na(gtdata_daily_state_MIXED[[regions]][,terms]),terms] = 0
    #if the search term is below 50 search volume OR if there are more than 70% 0s OR if search volume greater than 50 is less than 2% of total days. DELETE
    if (max(gtdata_daily_state_MIXED[[regions]][,terms])<=50 || length(which(gtdata_daily_state_MIXED[[regions]][,terms]!=0)) <(0.3*length(gtdata_daily_state_MIXED[[regions]][,terms])) || length(which(gtdata_daily_state_MIXED[[regions]][,terms]>50))< (0.1*length(gtdata_daily_state_MIXED[[regions]][,terms])) ){ 
      dropped_terms[[regions]] = c(dropped_terms[[regions]],as.character(terms))
    }
    else{ #if not, filter out its outliers
      start_filter_date = which(index(gtdata_daily_state_MIXED[[regions]][,terms]) == "2020-05-01")
      end_filter_date = length(gtdata_daily_state_MIXED[[regions]][,terms])
      #large valued outlier
      idx_high_outlier = which(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms] >= quantile(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms],0.999))
      idx_high_outlier = start_filter_date+idx_high_outlier-1
      if (!length(idx_high_outlier)==0){
        for (idx in 1:length(idx_high_outlier)){
          if (abs(end_filter_date-idx_high_outlier[idx])<=2){
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]-2))
          }
          else{
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]+1),c(idx_high_outlier[idx]+2))
          }
          high_outlier_neighbour_P={}
          for (i in 1:length(idx_high_neighbour)){ #compute the P-values of high outlier's neighbours by assuming Normal Distribution with mean equal to the outliers, covariance equal to the empirical covariance of the google search term series 
            high_outlier_neighbour_P = c(high_outlier_neighbour_P,pnorm(gtdata_daily_state_MIXED[[regions]][idx_high_neighbour[i],terms],gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx],terms],sqrt(cov(gtdata_daily_state_MIXED[[regions]][,terms]))))
          }
          if (all(high_outlier_neighbour_P<0.05)){ #if there are hight outliers via IQR and their neighbours have p-values less than 5%, replace the outliers via previous two day average
            gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx],terms] = (as.matrix(gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx]-3,terms]) )/3
          }
        }
      }
      #Small valued outlier. Less strict than high valued outlier above. No need to use p-value
      idx_low_outlier = which(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms] <= quantile(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms],0.01))
      idx_low_outlier = start_filter_date + idx_low_outlier - 1
      if (!length(idx_low_outlier)==0){
        for (idx in 1:length(idx_low_outlier)){
          gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx],terms] = (as.matrix(gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx]-3,terms]) )/3
        }
      }
    }
  }
}
names(dropped_terms) = names(gtdata_daily_state_MIXED) #name the list dropped_terms with region names
for (regions in names(gtdata_daily_state_MIXED)){ #DELETE the terms with lower than 50 search volume
  gtdata_daily_state_MIXED[[regions]] = gtdata_daily_state_MIXED[[regions]][,-which(names(gtdata_daily_state_MIXED[[regions]])%in% unlist(dropped_terms[regions]))]
}
gtdata_stateMixed_IQR_noMedia = gtdata_daily_state_MIXED #only IQR filtered

###########################################################################################################################################################
# ALL STATES JUST ONLY GT!!! (STEP 1 of ARGOX)
###########################################################################################################################################################
gtdata_tempNAT = gtdataALL_IQR_noMedia[["US"]]
terms_workon = names(gtdata_tempNAT)

gt_lag = 4:35
beta_sandPval_MedRaw = matrix(0,length(terms_workon),length(gt_lag))
rownames(beta_sandPval_MedRaw) = terms_workon
colnames(beta_sandPval_MedRaw) = paste0("lag-",as.character(gt_lag))
beta_MedRaw = list()
resid_MSE_MedRaw = matrix(0,length(terms_workon),length(gt_lag))
rownames(resid_MSE_MedRaw) = terms_workon
colnames(resid_MSE_MedRaw) = paste0("lag-",as.character(gt_lag))
for (terms in terms_workon){
  current_term = gtdata_tempNAT[,terms] #get the current working google search term
  #current_term = gtdata_smooth[,terms]
  gt_lag_train = lag(current_term, gt_lag) #get lagged search term for all lags working on, in later loop, work on each column at a time
  gt_train_cur <- na.omit(merge(gt_lag_train)) 
  test_dates_cur <- index(na.omit(merge(gt_train_cur, covid_national_level, all = F))) #get the common training and forecasting for all lags 
  #temp_date = test_dates_cur[c(which(test_dates_cur=="2020-05-01"):length(test_dates_cur))] 
  #temp_date =  as.Date(as.Date("2020-06-01"):as.Date("2020-08-31"))
  temp_date =  as.Date(as.Date("2020-04-01"):as.Date("2021-03-26"))
  beta_MedRaw_mat = matrix(0,2,length(gt_lag))
  colnames(beta_MedRaw_mat) = paste0("lag-",as.character(gt_lag))
  rownames(beta_MedRaw_mat) = c("Intercept", terms)
  for (i in 1:length(gt_lag)){
    temp_lm_medRaw = lm(covid_national_level$daily_deaths[temp_date]~ gt_train_cur[temp_date,i])
    #temp_lm_medRaw = lm(covid_national_level$new_cases[temp_date]~ gt_train_cur[temp_date,i])
    #temp_lm_medRaw = lm(CovidTracking_national_level$daily_deaths[temp_date]~ gt_train_cur[temp_date,i])
    #temp_lm_medRaw = lm(temp_nat_death[temp_date]~ gt_train_cur[temp_date,i])
    beta_MedRaw_mat[,i] = coef(temp_lm_medRaw)
    resid_MSE_MedRaw[terms,i] = sum(temp_lm_medRaw$residuals^2)
    sandwich_se = diag(sandwich::vcovHAC(temp_lm_medRaw))[2]^0.5
    beta_sandPval_MedRaw[terms,i] <- as.numeric(2* (1 - pt(abs(coef(temp_lm_medRaw)[2]/(diag(sandwich::vcovHAC(temp_lm_medRaw))[2]^0.5)) , 88) ) )
  }
  beta_MedRaw[[terms]] = beta_MedRaw_mat
}
MedRaw_optLag = apply(resid_MSE_MedRaw , 1, which.min) #Find the optimal lag if looking at all data (fitted lm over all data)
MedRaw_optLag_MSE = apply(resid_MSE_MedRaw , 1, min)
index2D <- function(v = MedRaw_optLag, DF = beta_sandPval_MedRaw){sapply(1:length(v), function(X){DF[X,v[X]]})}
beta_sandPval_MedRaw_opt = data.frame(index2D())
rownames(beta_sandPval_MedRaw_opt) = terms_workon

MedRaw_optLag = MedRaw_optLag+3
MedRaw_optLag_cases = MedRaw_optLag
MedRaw_optLag_death = MedRaw_optLag

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Selected terms that are significant to cases since June. 33 TERMS. Select them and lag them with optimal lag to cases/death
terms_important = c("X.coronavirus.vaccine.", "X.Cough.","X.covid.19.","X.covid.19.vaccine.", "X.Fever.", "X.Nausea.","X.Sore.throat.",
                    "X.Headache.", "bronchitis", "coronavirus.exposure", "coronavirus.cases", "coronavirus.test",
                     "covid.19.cases", "exposed.to.coronavirus",  "how.long.covid.19", 
                     "how.long.contagious",  "loss.of.smell", "loss.of.taste", "pneumonia", "rapid.covid.19", "rapid.coronavirus","robitussin","strep.throat",
                    "sinus", "symptoms.of.the.covid.19", "the.covid.19", "upper.respiratory")
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Daily Indicator (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
Days_of_Week = matrix(0,dim(gtdata_tempNAT)[1],7)
Days_of_Week = xts(Days_of_Week, index(gtdata_tempNAT))
colnames(Days_of_Week) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
for (i in 1:7){
  Days_of_Week[which(weekdays(index(Days_of_Week)) == colnames(Days_of_Week)[i]),i] = 1
}

########################################################################################################################################################
# ARGO STEP 1!!!!
########################################################################################################################################################
n_train <- 56  #use previous 90 days to predict today
n_train_minimum <- 42  #use at least previous 40 days to predict today
list_results_step1.Nat <- list()
coef_all_step1.Nat <- list()
coef_smooth_step1.Nat <- list()
# daily training weekly prediction -----------
ts_max_lag <- 1:7  #lag a week
case_lag <- c(7,14,21,28) #a week and two weeks
mobility_lag <- c(21,28,35)
total_forward = 14  #predicting how many days forward
decay <- 0.8
pred_week = 1:7
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# National Level Step 1
# end of day prediction for the future (stand on today want to predict upto two weeks ahead, using data before today, lagged)
RawNAT_32terms = gtdata_tempNAT[,terms_important]
gt_nat <- RawNAT_32terms
gt_lag <- MedRaw_optLag_death[terms_important] #The ALL data optimal Lag to Cases/Death (change here if switching Cases to Death)!!!
pred_target = covid_national_level$daily_deaths #prediction target

for(n_forward in 1:total_forward){ 
  x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1) #lag new death from one day to one week (matrix form) loop starting today upto 2 weeks (not using any "new data" as of today, since ts_max_lag+n_forward-1 not ts_max_lag)
  x_cases <- lag(covid_national_level$new_cases, pmax(case_lag, n_forward)) #(always make sure not use future data). If today is Monday, predict next Tuesday, cannot use tomorrow's data, thus when n_forward>7or14 have to replace 7,14 with n_forward, only have data before and today
  x_gt <- lapply(terms_important,function(i){lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )})
  x_gt = do.call(cbind,x_gt)
  x_curr <- na.omit(merge(x_ar, x_cases, x_gt , Days_of_Week)) 
  common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
  
  pred <-  pred_target[common_id_curr]  #predict the dates that have lagged data (omit first two weeks of data)
  pred[] <- NA
  pred_smooth <-pred_target[common_id_curr]  
  pred_smooth[] <- NA
  coef <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
  coef <- xts(coef, order.by = common_id_curr)
  colnames(coef) <- c("intercept",colnames(x_curr))
  coef_smooth <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
  coef_smooth <- xts(coef_smooth, order.by = common_id_curr)
  colnames(coef_smooth) <- c("intercept",colnames(x_curr))
  
  x_gt_new <- lapply(terms_important,function(i){lag(gt_nat[,i],pmax(gt_lag[i]-n_forward,0) )})
  x_gt_new = do.call(cbind,x_gt_new)
  x_new <- cbind( #started since 2019-01-01 sice word freq started then
    lag(pred_target, ts_max_lag - 1),  # latest available ar lags daily deaths (today, yesterday,...,6 days ago)
    lag(covid_national_level$new_cases, pmax(case_lag - n_forward, 0)), # confirmed cases a week ago new cases (a week ago today and two weeks ago today)
    x_gt_new,
    lag(Days_of_Week,(7-n_forward%%7))
    )  # latest available gt
  
   for(date_cur in common_id_curr){
    train_id <- as.Date(date_cur) - ((n_train-1):0) #set training set, use up to 90 days of data to train parameters, when predicting each day use lagged death, cases, wordfreq and mobility
    train_id <- train_id[train_id >= common_id_curr[1]] #keep only dates past the first day of lagged data, 2020-02-04 (since before that no explanatory variables)
    if(length(train_id) >= n_train_minimum){ 
      if (length(which(as.matrix(pred_target[train_id])==0))>(n_train_minimum-5)){ #if too many zeros then perdict death is 0
        coef[as.Date(date_cur),] = 0 
        pred[as.Date(date_cur)] = pred_target[as.Date(date_cur)]
      }else{ #start training only when having at least n_train_minimum days of training data to tain (not considering lagged dates)
        train_weights <- (decay)^(length(train_id):1)
        set.seed(100)
        lasso.fit <- glmnet::cv.glmnet(x = as.matrix(x_curr[train_id]), 
                                       y = as.matrix(pred_target[train_id]), nfolds = 10, grouped = FALSE,  #10 fold, alpha=1->lasso
                                       alpha = 1, weights = train_weights)
        lam.s <- lasso.fit$lambda.1se #select the lambda that gives the simpliest model within one standard error of optimal val of lambda
        coef[as.Date(date_cur),] <- as.numeric(coef(lasso.fit, lambda = lam.s)) #store the coef for the simpliest lambda in the training date's row (if common_id_curr[40] then stored in row 2020-02-04+39=2020-03-14) for all explanatory variables 
        pred[as.Date(date_cur)] <- predict(lasso.fit, newx = as.matrix(x_new[as.Date(date_cur)]), s = lam.s) #store prediction for that date ???predict tomorrow???
      }
      # ARGO Smooth, Manually adjust the smoothing period
      if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) ){
      #if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-3),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-4),])) ){
        temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) )/3
        #temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) +  as.numeric(coef[(as.Date(date_cur)-3),]) +  as.numeric(coef[(as.Date(date_cur)-4),]) )/5
        coef_smooth[as.Date(date_cur),] = temp_coef
        pred_smooth[as.Date(date_cur)] = sum(temp_coef * c(1,as.numeric(x_new[as.Date(date_cur)])))
      }
    }
  }
  ##
  truth <- lag(pred_target, -n_forward) #lag backwards (yesterday has today's data), truth is today predicting tomorrow and upto two weeks head (save to today's row now)
  naive <- pred_target #naive approach simply using the death count today (newest) to predict future (whether it is tomorrow or next week)
  naive_period <- lag(pred_target,(7-n_forward%%7) ) #second naive approach simply using the death count 7 days ago to predict today. 
  set.seed(100)
  argo_pre <- round((pred))
  argo_presmooth <- round((pred_smooth))
  results <- merge(truth, naive, naive_period, argo_pre, argo_presmooth, all = F)
  colnames(results) <- c("truth", "naive", "naive_period", "argo", "argo_smooth")
  
  list_results_step1.Nat[[n_forward]] <- results  
  coef_all_step1.Nat[[n_forward]] <- coef
  coef_smooth_step1.Nat[[n_forward]] <- coef_smooth
}



#------------------------------------------------------------------------------------------------------------------------------------------------------------
# Regional Level Step 1
ts_max_lag <- 1:7  #lag a week
gt_lag <- c(7, 14) #a week and two weeks   
case_lag <- c(7,14,21,28) #a week and two weeks
total_forward = 14  #predicting how many days forward
decay <- 0.8

n_train <- 56  
n_train_minimum <- 42 
list_results_step1.Reg <- list()
coef_all_step1.Reg <- list()
coef_smooth_step1.Reg <- list()
for (region in names(gtdata_Region_IQR_noMedia)){
  pred_target = covid_regional_level[[region]]$daily_deaths
  temp_terms = names(gtdata_Region_IQR_noMedia[[region]])[names(gtdata_Region_IQR_noMedia[[region]])%in% terms_important]
  gt_nat <- gtdata_Region_IQR_noMedia[[region]] [,temp_terms]
  gt_lag <- MedRaw_optLag_death[temp_terms]

  # end of day prediction for the future (stand on today want to predict upto two weeks ahead, using data before today, lagged)
  for(n_forward in 1:total_forward){ 
    x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1)
    x_cases <- lag(covid_regional_level[[region]]$new_cases, pmax(case_lag, n_forward))
    x_gt <- lapply(temp_terms,function(i){lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )})
    x_gt = do.call(cbind,x_gt)
    x_curr <- na.omit(merge(x_ar, x_cases, x_gt ,  Days_of_Week)) 
    common_id_curr <- index(na.omit(merge(x_curr, pred_target, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
    
    pred <-  pred_target[common_id_curr] #predict the dates that have lagged data (omit first two weeks of data)
    pred[] <- NA
    pred_smooth <-pred_target[common_id_curr]  
    pred_smooth[] <- NA
    coef <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
    coef <- xts(coef, order.by = common_id_curr)
    colnames(coef) <- c("intercept",colnames(x_curr))
    coef_smooth <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
    coef_smooth <- xts(coef_smooth, order.by = common_id_curr)
    colnames(coef_smooth) <- c("intercept",colnames(x_curr))
    
    x_gt_new <- lapply(temp_terms,function(i){lag(gt_nat[,i],pmax(gt_lag[i]-n_forward,0) )})
    x_gt_new = do.call(cbind,x_gt_new)
    x_new <- cbind(
      lag(pred_target, ts_max_lag - 1),  
      lag(covid_regional_level[[region]]$new_cases, pmax(case_lag - n_forward, 0)), 
      x_gt_new,
      lag(Days_of_Week,(7-n_forward%%7))
    )  
    
    for(date_cur in common_id_curr){
      train_id <- as.Date(date_cur) - ((n_train-1):0) #set training set, use up to 90 days of data to train parameters, when predicting each day use lagged death, cases, wordfreq and mobility
      train_id <- train_id[train_id >= common_id_curr[1]] #keep only dates past the first day of lagged data, 2020-02-04 (since before that no explanatory variables)
      if(length(train_id) >= n_train_minimum){ 
        if (length(which(as.matrix(pred_target[train_id])==0))>(n_train_minimum-5)){ #if too many zeros then perdict death is 0
          coef[as.Date(date_cur),] = 0 
          pred[as.Date(date_cur)] =pred_target[as.Date(date_cur)]
        }else{ #start training only when having at least n_train_minimum days of training data to tain (not considering lagged dates)
          train_weights <- (decay)^(length(train_id):1)
          set.seed(100)
          lasso.fit <- glmnet::cv.glmnet(x = as.matrix(x_curr[train_id]), 
                                         y = as.matrix(pred_target[train_id]), nfolds = 10, grouped = FALSE,  #10 fold, alpha=1->lasso
                                         alpha = 1, weights = train_weights)
          lam.s <- lasso.fit$lambda.1se #select the lambda that gives the simpliest model within one standard error of optimal val of lambda
          coef[as.Date(date_cur),] <- as.numeric(coef(lasso.fit, lambda = lam.s)) #store the coef for the simpliest lambda in the training date's row (if common_id_curr[40] then stored in row 2020-02-04+39=2020-03-14) for all explanatory variables 
          pred[as.Date(date_cur)] <- predict(lasso.fit, newx = as.matrix(x_new[as.Date(date_cur)]), s = lam.s) #store prediction for that date ???predict tomorrow???
        }
        if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-3),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-4),])) ){
          temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) + as.numeric(coef[(as.Date(date_cur)-3),]) + as.numeric(coef[(as.Date(date_cur)-4),]) )/5
          coef_smooth[as.Date(date_cur),] = temp_coef
          pred_smooth[as.Date(date_cur)] = sum(temp_coef * c(1,as.numeric(x_new[as.Date(date_cur)])))
        }
      }
    }
    ##
    truth <- lag(pred_target, -n_forward) #lag backwards (yesterday has today's data), truth is today predicting tomorrow and upto two weeks head (save to today's row now)
    naive <- pred_target#naive approach simply using the death count today (newest) to predict future (whether it is tomorrow or next week)
    naive_period <- lag(pred_target , (7-n_forward%%7) ) #second naive approach simply using the death count 7 days ago to predict today. 
    set.seed(100)
    argo_pre <- round((pred))
    argo_presmooth <- round((pred_smooth))
    results <- merge(truth, naive, naive_period, argo_pre, argo_presmooth, all = F)
    colnames(results) <- c("truth", "naive", "naive_period", "argo", "argo_smooth")
    
    list_results_step1.Reg[[region]][[n_forward]] <- results  
    coef_all_step1.Reg[[region]][[n_forward]] <- coef
    coef_smooth_step1.Reg[[region]][[n_forward]] <- coef_smooth
  }
}

###########################################################################################################################################################
# 51 STATES 
n_train <- 56 
n_train_minimum <- 40
list_results_step1.State <- list()
coef_all_step1.State <- list()
coef_smooth_step1.State <- list()
ts_max_lag <- 1:7  #lag a week
case_lag <- c(14,21,28,35) #a week and two weeks
#gt_lag = case_lag
mobility_lag <- c(14,21,28,35)
total_forward = 14  #predicting how many days forward
decay <- 0.8
pred_week = 1:7
for (region in state_names){
  pred_target = covid_state_level[[region]]$daily_deaths
  temp_terms = names(gtdata_stateMixed_IQR_noMedia[[region]])[names(gtdata_stateMixed_IQR_noMedia[[region]])%in% terms_important]
  gt_nat <- gtdata_stateMixed_IQR_noMedia[[region]][,temp_terms]
  gt_lag <- MedRaw_optLag_death[temp_terms]

  for(n_forward in 1:total_forward){
    x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1)
    x_cases <- lag(covid_state_level[[region]]$new_cases, pmax(case_lag, n_forward))
    x_gt <- lapply(temp_terms,function(i){lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )}) 
    x_gt = do.call(cbind,x_gt)
    x_curr <- na.omit(merge(x_ar, x_cases, x_gt ,  Days_of_Week)) 
    common_id_curr <- index(na.omit(merge(x_curr, pred_target, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
    
    pred <-  pred_target[common_id_curr] #predict the dates that have lagged data (omit first two weeks of data)
    pred[] <- NA
    pred_smooth <-pred_target[common_id_curr]  
    pred_smooth[] <- NA
    coef <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
    coef <- xts(coef, order.by = common_id_curr)
    colnames(coef) <- c("intercept",colnames(x_curr))
    coef_smooth <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
    coef_smooth <- xts(coef_smooth, order.by = common_id_curr)
    colnames(coef_smooth) <- c("intercept",colnames(x_curr))
    
    x_gt_new <- lapply(temp_terms,function(i){lag(gt_nat[,i],pmax(gt_lag[i]-n_forward,0) )})
    x_gt_new = do.call(cbind,x_gt_new)
    x_new <- cbind(
      lag(pred_target, ts_max_lag - 1),  
      lag(covid_state_level[[region]]$new_cases, pmax(case_lag - n_forward, 0)), 
      x_gt_new,
      lag(Days_of_Week,(7-n_forward%%7))
    )  
    
    for(date_cur in common_id_curr){
      train_id <- as.Date(date_cur) - ((n_train-1):0) #set training set, use up to 90 days of data to train parameters, when predicting each day use lagged death, cases, wordfreq and mobility
      train_id <- train_id[train_id >= common_id_curr[1]] #keep only dates past the first day of lagged data, 2020-02-04 (since before that no explanatory variables)
      if(length(train_id) >= n_train_minimum){ 
        if (length(which(as.matrix(pred_target[train_id])==0))>(n_train_minimum-5)){ #if too many zeros then perdict death is 0
          coef[as.Date(date_cur),] = 0 
          pred[as.Date(date_cur)] =pred_target[as.Date(date_cur)]
        }else{ #start training only when having at least n_train_minimum days of training data to tain (not considering lagged dates)
          train_weights <- (decay)^(length(train_id):1)
          set.seed(100)
          lasso.fit <- glmnet::cv.glmnet(x = as.matrix(x_curr[train_id]), 
                                         y = as.matrix(pred_target[train_id]), nfolds = 10, grouped = FALSE,  #10 fold, alpha=1->lasso
                                         alpha = 1, weights = train_weights)
          lam.s <- lasso.fit$lambda.1se #select the lambda that gives the simpliest model within one standard error of optimal val of lambda
          coef[as.Date(date_cur),] <- as.numeric(coef(lasso.fit, lambda = lam.s)) #store the coef for the simpliest lambda in the training date's row (if common_id_curr[40] then stored in row 2020-02-04+39=2020-03-14) for all explanatory variables 
          pred[as.Date(date_cur)] <- predict(lasso.fit, newx = as.matrix(x_new[as.Date(date_cur)]), s = lam.s) #store prediction for that date ???predict tomorrow???
        }
        if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-3),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-4),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-5),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-6),])) ){
          temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) + as.numeric(coef[(as.Date(date_cur)-3),]) + as.numeric(coef[(as.Date(date_cur)-4),]) + as.numeric(coef[(as.Date(date_cur)-5),]) + as.numeric(coef[(as.Date(date_cur)-6),]) )/7
          coef_smooth[as.Date(date_cur),] = temp_coef
          pred_smooth[as.Date(date_cur)] = sum(temp_coef * c(1,as.numeric(x_new[as.Date(date_cur)])))
        }
      }
    }
    ##
    truth <- lag(pred_target, -n_forward) #lag backwards (yesterday has today's data), truth is today predicting tomorrow and upto two weeks head (save to today's row now)
    naive <- pred_target #naive approach simply using the death count today (newest) to predict future (whether it is tomorrow or next week)
    naive_period <- lag(pred_target, (7-n_forward%%7)) #second naive approach simply using the death count 7 days ago to predict today. 
    set.seed(100)
    argo_pre <- round((pred))
    argo_presmooth <- round((pred_smooth))
    results <- merge(truth, naive, naive_period, argo_pre, argo_presmooth, all = F)
    colnames(results) <- c("truth", "naive", "naive_period", "argo", "argo_smooth")
    list_results_step1.State[[region]][[n_forward]] <- results  
    coef_all_step1.State[[region]][[n_forward]] <- coef
    coef_smooth_step1.State[[region]][[n_forward]] <- coef_smooth
  }
}


###########################################################################################################################################################
#########################################################################################################################################################
# Multiple correlation 
alone_states = c("US-AK", "US-HI")
joint_states = state_names[!state_names %in% alone_states]
multi_cor = matrix(0, 2, length(joint_states))
colnames(multi_cor) = joint_states
rownames(multi_cor) = c("Adj.R-sqr", "R-sqr")
for (region in joint_states){
  region_id_for_state = state_info[Abbre==strsplit(region, "-")[[1]][2], Region]
  region_truth <- lapply(list_results_step1.Reg, function(x) x[,"truth"]) #regional level predicted Covid-19 weekly death
  region_truth <- do.call(merge, region_truth)
  colnames(region_truth) = names(list_results_step1.Reg)
  region_truth = region_truth[,-c(region_id_for_state)]
  
  state_truth <- lapply(list_results_step1.State, function(x) x[,"truth"]) #regional level predicted Covid-19 weekly death
  state_truth <- do.call(merge, state_truth)
  colnames(state_truth) = names(list_results_step1.State)
  state_truth_X = state_truth[,joint_states[!joint_states %in% region]]
  state_truth_Y = state_truth[,region]
  state_region_national = na.omit(cbind(state_truth_X, region_truth, list_results_step1.Nat$truth))
  state_truth_Y = state_truth_Y[index(state_region_national)]
  lm_fit = lm(state_truth_Y[,region] ~ state_region_national ) 
  multi_cor[1,region] = as.numeric(summary(lm_fit)[9])
  multi_cor[2,region] = as.numeric(summary(lm_fit)[8])
}
alone_states = c(alone_states,names(which(multi_cor[1,]<0.97)))
joint_states = state_names[!state_names %in% alone_states]

argo.org = list()
argo.org[[1]]= argo.state.true; argo.org[[2]]= argo.state.p; argo.org[[3]]= argo.reg.p; argo.org[[4]]= argo.nat.p

##################################################################################################################################################################################################################################################################################################################
# ARGOX step 2, predict diff 1, Spacial Correlated JOINT States only, ARGOX no Constraint. Daily Predict (daily stored moving average 7 days)
####################################################################################################################################################################################################################################################################################################################
# Though daily stored weekly, Y_{t} - Y_{t-1}, the (t-1) is still a week ago, i.e. t-7
argo.reg.p = {}
for (i in names(list_results_step1.Reg)){
  if (temp_region[5,i]<=temp_region[4,i]){
    argo.reg.p = cbind(argo.reg.p, list_results_step1.Reg[[i]][,5])
    print(paste0(i,"-ARGOSmooth"))
  }
  else{
    argo.reg.p = cbind(argo.reg.p, list_results_step1.Reg[[i]][,4])
    print(paste0(i,"-ARGO"))
  }
}
colnames(argo.reg.p) <- names(list_results_step1.Reg) #set column names to "Region-x"
index(argo.reg.p) <- as.Date(index(argo.reg.p)) #convert to daily index (apply.weekly will change index to seconds UTC, weird)

argo.state.p = {}
for (i in names(list_results_step1.State)){
  if (temp_state[5,i]<=temp_state[4,i]){
    argo.state.p = cbind(argo.state.p, list_results_step1.State[[i]][,5])
  }
  else{
    argo.state.p = cbind(argo.state.p, list_results_step1.State[[i]][,4])
  }
}
colnames(argo.state.p) <- names(list_results_step1.State)
index(argo.state.p) <- as.Date(index(argo.state.p))

# Note that on 3/8, truth is total death for 3/9+3/10+...+3/15. Naive later is on 3/8, stored 3/2+3/3+...+3/8. True diff is on 3/8, stored this week-last week. Lagged 1 week diff is on 3/8, stored last week - lastlast week
# Thus, predicted stored on 3/8 is the forecast for 3/9+...+3/15
argo.state.true <- lapply(list_results_step1.State, function(x) x[,"truth"]) #state level true Covid-19 weekly death
argo.state.true <- do.call(merge, argo.state.true)
colnames(argo.state.true) <- names(list_results_step1.State)
index(argo.state.true) <- as.Date(index(argo.state.true))

argo.nat.p <- list_results_step1.Nat$argo_smooth #national level predicted Covid-19 weekly death
index(argo.nat.p) <- as.Date(index(argo.nat.p))


LAG_PREDICT = 1 #n_forward weeks
state_info = state_region_info
truth_JOINT = argo.state.true[,joint_states]
use_yt2=TRUE

argo.state.p_JOINT = argo.state.p[,joint_states]
state_names_JOINT = joint_states
# Take in truth (the true p at state level, matrix of number of states columns), predicted states p, regional p, national p, state names, which state to which region
argo2 <- function(truth, argo.state.p, argo.reg.p, argo.nat.p, argo.state_1.5, state_names_JOINT, state_info, use_yt2=TRUE, Nat_Constraint = TRUE){
  training_period = 30  #COVID-19 use 70 days, 10 weeks training period
  Ensumble_training_period = 120
  
  naive.p <- truth_JOINT  
  index(naive.p) <- index(truth_JOINT ) + 7*(1+LAG_PREDICT) #construct naive.p which simply uses this week's %ILI to predict next week's (7 days forward or n_forward=7*1, can change in Covid where basis is 1:1day instead of 7 days:1week)
  #LAG_PREDICT is like n_forward, if =1 then predicting next week's. Or more like the length of a period, default is 1 week as the same as the data index. If =1, then is 2 weeks difference, i.e. t, t-1 is differed by 2 weeks
  #if(LAG_PREDICT > 0){ #If n_foward>0, then we need to expand the responds p rows since forecast will outofscope
  #truth_JOINT  <- rbind(truth_JOINT , xts(matrix(NaN, nrow=LAG_PREDICT, ncol=ncol(truth_JOINT )), max(index(truth_JOINT )) + (1:LAG_PREDICT)*7))
  #}
  
  #Note that Y is a matrix where each columns contain true %ILI for each state
  Y <- truth_JOINT  - naive.p #obtain Z_t, the difference of p_t-p_{t-1}, here one difference in time steps is a week
  Yt2 <- Y 
  index(Yt2) <- index(Yt2) + 7*(1+LAG_PREDICT) #Yt2 is like Z_{t}=p_{t-1}-p_{t-2}, which is same as Z_{t-1}=p_{t-1}-p_{t-2}. i.e. at time t, Y[t] is Z_t while Yt2 is Z_{t-1}. Used for correlation calculation
  
  #common_idx <- index(na.omit(merge(naive.p, argo.state.p_JOINT , argo.nat.p, argo.state_1.5, Yt2))) #get the common index
  common_idx <- index(na.omit(merge(naive.p, argo.state.p_JOINT , argo.nat.p, Yt2))) 
  
  argo.nat.p <- argo.nat.p[common_idx]
  argo.reg.p <- argo.reg.p[common_idx]
  naive.p <- naive.p[common_idx]
  truth_JOINT  <- truth_JOINT [common_idx]
  argo.state.p_JOINT  <- argo.state.p_JOINT [common_idx]
  #argo.state_1.5 <- argo.state_1.5[common_idx]
  Yt2 <- Yt2[common_idx]
  #colMeans(truth_JOINT -argo.state.p_JOINT )/colMeans((naive.p-truth_JOINT )^2)
  
  X <- argo.state.p_JOINT  - naive.p #obtain p^{GT}_t-p_{t-1}, state increment prediction for t
  #X_1.5 <- argo.state_1.5 - naive.p #obtain p^{AR}_t-p_{t-1}, step1.5 (case+death predictions) increment prediction for t
  X.nat <- as.numeric(argo.nat.p) - naive.p #obtain p^{nat}_t-p_{t-1}, national increment prediction for t. 
  X.nat <- X.nat[common_idx]#Note that everything here are same dimensions now, all having 51 columns (number of states), for region its the same value for all states in that region then minus truth_JOINT  (state level), which is stable 
  #for national its the same value accross all states, then when minus truth_JOINT  and get p^{nat}_t-p_{t} its also stable since p_{t} is different across states
  argo.reg.p.dup <- lapply(state_names_JOINT, function(each_state){
    region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
    argo.reg.p[,region_id_for_state]
  })
  argo.reg.p.dup <- do.call(merge, argo.reg.p.dup)
  names(argo.reg.p.dup) <- state_names_JOINT #Here, insteaad of having 10 columns in region p, we bump to 51 where simply duplicate the regions p for all states in that region. STORE IN argo.reg.p.dup
  X.reg <- argo.reg.p.dup - naive.p #obtain p^{reg}_t-p_{t-1}, here each column is the region that state is in's p (51 columns instead of 10), regional increment prediction for t
  
  projection.mat <- list()
  mean.mat <- list()
  
  Y.pred <- X
  Y.pred[] <- NA
  
  zw_used <- list()
  fitting_error <- list()
  sigma_ww.structured <- sigma_ww.empirical <-
    sigma_zw.structured <- sigma_zw.empirical <-
    heat.vec.structured <-
    sigma_zwzw.structured <- sigma_zwzw.empirical <- list() #construct lists for variance matrix elements
  
  for(it in (training_period+7*(1+LAG_PREDICT)):length(common_idx) ){ #Note n_training in ARGO is 104
    training_idx <- common_idx[(it-training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))] #get training dates/period (total of fixed 104 days), same idea as first step (have a training period for prediction at t, which is why a "-1'at the end of range)
    t.now <- common_idx[it] #The time step t that we predict (testing upon training)
    y <- Y[training_idx,]  #get Z_t=p_t-p_{t-1}
    x <- X[training_idx,]  #get p^{GT}_t-p_{t-1}, state_level
    #x_1.5 <- X_1.5[training_idx,]  #get p^{AR}_t-p_{t-1}, state_level
    x.nat <- X.nat[training_idx,] #get p^{nat}_t-p_{t-1}
    x.reg <- X.reg[training_idx,] #get p^{reg}_t-p_{t-1}
    yt2 <- Yt2[training_idx,] #get Z_{t-1}
    
    sigma_yy <- var(y) #compute sigma_zz
    
    m1 <- cor(y, yt2) #??First, we are assuming each t (rows) follow a multivariate distribution, but here is computing according to column?
    m2 <- cor(y)  #Also, This way of cumpting ACF for one lag doesn't seem to be correct? Isn't it just computing sample correlation and scale by variance?
    rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
    
    autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
    
    
    # vcov.x_xnat <- 
    #   cbind(rbind(sigma_yy+diag(diag(var((argo.state.p_JOINT  - truth_JOINT )[training_idx,]))),sigma_yy),
    #         rbind(sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT )[training_idx,])))
    #vcov.x_xreg_xnat <- #constructed the lower 4x4 part of sigma_ww page 11, see derivation in the notes. Added in step 1.5 AR predictions, bump dimension +1
    #cbind(rbind(sigma_yy+var((argo.state.p_JOINT  - truth_JOINT )[training_idx,]),sigma_yy, sigma_yy, sigma_yy),
    #rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_JOINT )[training_idx,]), sigma_yy, sigma_yy),
    #rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT )[training_idx,]), sigma_yy),
    #rbind(sigma_yy, sigma_yy, sigma_yy, sigma_yy+var((argo.state_1.5 - truth_JOINT )[training_idx,])) )
    vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_JOINT  - truth_JOINT )[training_idx,]),sigma_yy, sigma_yy),
                              rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_JOINT )[training_idx,]), sigma_yy),
                              rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT )[training_idx,]) ) )
    
    #sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy,sigma_yy) #created the last 4 elements vector for sigma_zw (included step1.5 AR predictions)
    sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy) 
    
    if(use_yt2){ #whether using naive increment estimation in W "vector" also? if yes, then add-in its term in sigma_ww and sigma_zw
      #vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2,autocov.y.yt2,autocov.y.yt2,autocov.y.yt2)) #here its coded in different order than in the paper, see Pg11
      vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
      #vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy)) #why transpose here? isnt sigma_zz symmetric matrix?
      vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
      sigma_zw <- cbind(sigma_zw, autocov.y.yt2)
    }
    
    # shrinked  
    if(use_yt2){
      #Using equation (6) ridge inspired idea, adding empirical covariance diagonal in the inverse. This is "forecast time step t"
      #y.pred <- colMeans(y) + sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5,Yt2)[training_idx,]))), 
      #c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(X_1.5[t.now,])-colMeans(x_1.5), t(Yt2[t.now,]-colMeans(yt2))))
      y.pred <- colMeans(y) + sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))), 
                                                 c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) )
      
      # Get the fitted value also (training time steps)
      #y.fitted <- colMeans(y) + sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5, Yt2)[training_idx,]))), 
      #rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(x_1.5)-colMeans(x_1.5)), (t(yt2)-colMeans(yt2))))
      y.fitted <- colMeans(y) + sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat, Yt2)[training_idx,]))), 
                                                   rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2))) )
      residual_epsilon <- y - t(y.fitted) #get fitted residual: true_training-predicted_training
    }else{
      y.pred <- colMeans(y) +
        sigma_zw %*% 
        solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5)[training_idx,]))), 
              c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(X_1.5[t.now,])-colMeans(x_1.5)))
    }
    
    Y.pred[t.now, ] <- t(y.pred) #store the prediction from equation (6)
    
    #projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5,Yt2)[training_idx,])))) #store siga_zw*(sigma_ww+D_ww)^{-1}   
    projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))))
    #mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat), colMeans(x_1.5),colMeans(yt2)) #store the training set's empirical mean for each forecast of t 
    mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat),colMeans(yt2))
    
    #Store everything in a list where each element contains all stuff for the forecast of time step t and its training time steps
    sigma_ww <- vcov.x_xreg_xnat 
    sigma_zz <- sigma_yy
    sigma_ww.structured[[as.character(t.now)]] <- sigma_ww #structured is using independence assumption and derived 
    #sigma_ww.empirical[[as.character(t.now)]] <- var(cbind(X,X.reg,X.nat,X_1.5, Yt2)[training_idx,]) #the empirical here is the matrix D (not using independence assumption)
    sigma_ww.empirical[[as.character(t.now)]] <- var(cbind(X,X.reg,X.nat, Yt2)[training_idx,])
    sigma_zw.structured[[as.character(t.now)]] <- sigma_zw
    #sigma_zw.empirical[[as.character(t.now)]] <- cov(Y[training_idx,], cbind(X,X.reg,X.nat,X_1.5,Yt2)[training_idx,])
    sigma_zw.empirical[[as.character(t.now)]] <- cov(Y[training_idx,], cbind(X,X.reg,X.nat,Yt2)[training_idx,])
    
    sigma_zwzw.structured[[as.character(t.now)]] <- rbind( #The huge covariance matrix of Z (1 "element") and W (4 "elemtns")
      cbind(sigma_zz, sigma_zw),
      cbind(t(sigma_zw), sigma_ww)
    )
    #sigma_zwzw.empirical[[as.character(t.now)]] <- var(cbind(Y,X,X.reg,X.nat,X_1.5,Yt2)[training_idx,])
    sigma_zwzw.empirical[[as.character(t.now)]] <- var(cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,])
    #zw_used[[as.character(t.now)]] <- cbind(Y,X,X.reg,X.nat,X_1.5,Yt2)[training_idx,] #For forecasting each time t, store the training set used all elements
    zw_used[[as.character(t.now)]] <- cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,] 
    fitting_error[[as.character(t.now)]] <- residual_epsilon 
  }
  
  projection.mat <- sapply(projection.mat, identity, simplify = "array") #convert a list into 3D matrix (dimensions agree)
  mean.mat <- sapply(mean.mat, identity, simplify = "array") #convert to a 2D matrix, each column is a mean vector for forecast of time step t. (5*51 columns)
  
  argo2.p <- Y.pred + naive.p #the actual forecast of %ILI for time t (p_t=p_{t-1}+(p_t-p_{t-1})) is p_{t-1}+\hat{Z}_t
  argo2.p[argo2.p<0] = 0
  
  err.twostep <- argo2.p - truth_JOINT  #compute the forecasting error. Here 2-step means using 2-step ARGOX, 1-step means just using Google search step 1 only.
  
  #Store "vector" [Z_t,W'_t], where Z_t=p_t-p_{t-1}, W'_t=[p^{GT}_t-p_t, p^{reg}_t-p_t, p^{nat}_t-p_t, p_{t-1}-p_{t-2}] where W'_t are the independent error vectors 
  heat.vec <- na.omit(merge(truth_JOINT -naive.p, argo.state.p_JOINT  - truth_JOINT , argo.reg.p.dup - truth_JOINT , as.numeric(argo.nat.p) - truth_JOINT ,argo.state_1.5-truth_JOINT ,Yt2))
  colnames(heat.vec) <- paste0(rep(c("True Increment", "err.argo.", "err.reg.", "err.nat.", "err.argo.AR.", "err.y2."), each=length(state_names_JOINT)), state_names_JOINT)
  
  #convert all matrix stored as lists above to 3D arrays
  sigma_ww.structured <- sapply(sigma_ww.structured, identity, simplify = "array")
  sigma_ww.empirical <- sapply(sigma_ww.empirical, identity, simplify = "array")
  sigma_zw.structured <- sapply(sigma_zw.structured, identity, simplify = "array")
  sigma_zw.empirical <- sapply(sigma_zw.empirical, identity, simplify = "array")
  sigma_zwzw.structured <- sapply(sigma_zwzw.structured, identity, simplify = "array")
  sigma_zwzw.empirical <- sapply(sigma_zwzw.empirical, identity, simplify = "array")
  zw_used <- sapply(zw_used, identity, simplify = "array")
  
  list(onestep=argo.state.p_JOINT , twostep=argo2.p, naive=naive.p, truth_JOINT =truth_JOINT ,
       Y.pred=Y.pred, err.twostep=err.twostep,
       heat.vec=heat.vec, projection.mat=projection.mat, mean.mat=mean.mat,
       sigma_ww.structured=sigma_ww.structured, sigma_ww.empirical=sigma_ww.empirical,
       sigma_zw.structured=sigma_zw.structured, sigma_zw.empirical=sigma_zw.empirical,
       sigma_zwzw.structured=sigma_zwzw.structured, sigma_zwzw.empirical=sigma_zwzw.empirical,
       zw_used=zw_used, zw_overall = cbind(Y,X,X.reg,X.nat,X_1.5,Yt2), fitting_error=fitting_error)
}
# RMSE
ARGO_JOINT_MSE = colMeans(na.omit(argo.state.p_JOINT -truth_JOINT )[index(na.omit(err.twostep))]^2) / colMeans(na.omit(naive.p - truth_JOINT )[index(na.omit(err.twostep))]^2)
ARGOX_JOINT_MSE = colMeans(na.omit(err.twostep)[index(na.omit(err.twostep))]^2) / colMeans(na.omit(naive.p - truth_JOINT )[index(na.omit(err.twostep))]^2)

#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Daily stored weekly aggregated, Stand Alone States ARGOX, no constraint.
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
argo.state.p_ALONE = {}
for (i in names(list_results_step1.State)){
  if (temp_state[5,i]<=temp_state[4,i]){
    argo.state.p_ALONE = cbind(argo.state.p_ALONE, list_results_step1.State[[i]][,5])
  }
  else{
    argo.state.p_ALONE = cbind(argo.state.p_ALONE, list_results_step1.State[[i]][,4])
  }
}
colnames(argo.state.p_ALONE) <- names(list_results_step1.State)
index(argo.state.p_ALONE) <- as.Date(index(argo.state.p_ALONE))

state_info = state_region_info
truth_ALONE = argo.state.true[,alone_states]
argo.state.p_ALONE = argo.state.p_ALONE[,alone_states]

argo_ind <- function(truth_ALONE, argo.state.p_ALONE, argo.nat.p, state_info){
  training_period_ALONE = 30
  naive.p_Alone <- truth_ALONE
  index(naive.p_Alone) <- index(truth_ALONE) + 7*(1+LAG_PREDICT)
  #if(LAG_PREDICT > 0){
  #truth_ALONE <- rbind(truth_ALONE, xts(matrix(NaN, nrow=LAG_PREDICT, ncol=ncol(truth_ALONE)), max(index(truth_ALONE)) + (1:LAG_PREDICT)*7))
  #}
  
  Y_ALONE <- truth_ALONE - naive.p_Alone
  Yt2_ALONE <- Y_ALONE
  index(Yt2_ALONE) <- index(Yt2_ALONE) + 7*(1+LAG_PREDICT)
  
  common_idx <- index(na.omit(merge(naive.p_Alone, argo.state.p_ALONE, argo.nat.p, Yt2_ALONE)))
  
  argo.nat.p <- argo.nat.p[common_idx]
  naive.p_Alone <- naive.p_Alone[common_idx]
  truth_ALONE <- truth_ALONE[common_idx]
  argo.state.p_ALONE <- argo.state.p_ALONE[common_idx]
  Yt2_ALONE <- Yt2_ALONE[common_idx]
  
  X_ALONE <- argo.state.p_ALONE - naive.p_Alone
  
  X.nat_ALONE <- as.numeric(argo.nat.p) - naive.p_Alone
  X.nat_ALONE <- X.nat_ALONE[common_idx]
  
  Y.pred_ALONE <- X_ALONE
  Y.pred_ALONE[] <- NA
  
  fitting_error <- list()
  
  for(it in (training_period_ALONE+7*(1+LAG_PREDICT)):length(common_idx) ){
    training_idx <- common_idx[(it-training_period_ALONE-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))]
    residual_epsilon <- Y_ALONE[training_idx,]
    residual_epsilon[,] <- NA
    
    for(state_id in 1:ncol(argo.state.p_ALONE)){
      t.now <- common_idx[it]
      y_ALONE <- Y_ALONE[training_idx,state_id] #p_t - p_{t-1}
      x_ALONE <- X_ALONE[training_idx,state_id] #p^{GT}_t - p_{t-1}
      x.nat_ALONE <- X.nat_ALONE[training_idx,state_id] #p^{nat}_t - p_{t-1}
      yt2_ALONE <- Yt2_ALONE[training_idx,state_id] #p_{t-1} - p_{t-2}
      
      sigma_zw_ALONE <- cov(y_ALONE, cbind(x_ALONE, x.nat_ALONE, yt2_ALONE))
      vcov.x_xreg_xnats_ALONE <- var(cbind(x_ALONE, x.nat_ALONE, yt2_ALONE))
      
      y.pred_ALONE <- colMeans(y_ALONE) +
        sigma_zw_ALONE %*% 
        solve(vcov.x_xreg_xnats_ALONE + diag(diag(vcov.x_xreg_xnats_ALONE)+1), 
              c(X_ALONE[t.now,state_id]-mean(x_ALONE), X.nat_ALONE[t.now,state_id]-mean(x.nat_ALONE), Yt2_ALONE[t.now,state_id]-mean(yt2)))
      
      y.fitted_ALONE <- colMeans(y_ALONE) +
        sigma_zw_ALONE %*% 
        solve(vcov.x_xreg_xnats_ALONE + diag(diag(vcov.x_xreg_xnats_ALONE)+1), 
              t(cbind(x_ALONE-mean(x_ALONE), x.nat_ALONE-mean(x.nat_ALONE), yt2_ALONE-mean(yt2_ALONE))))
      
      residual_epsilon[, state_id] <- y_ALONE - t(y.fitted_ALONE)
      
      Y.pred_ALONE[t.now, state_id] <- y.pred_ALONE
    }
    fitting_error[[as.character(t.now)]] <- residual_epsilon
  }
  
  argo2.p_ALONE <- Y.pred_ALONE + naive.p_Alone
  argo2.p_ALONE[argo2.p_ALONE<0] = 0
  err.twostep_ALONE <- argo2.p_ALONE - truth_ALONE
  
  list(onestep=argo.state.p_ALONE, twostep=argo2.p_ALONE, naive=naive.p_Alone, truth=truth_ALONE,
       Y.pred=Y.pred_ALONE, err.twostep=err.twostep_ALONE, fitting_error=fitting_error)
}
# RMSE
ARGO_ALONE_MSE = colMeans(na.omit(argo.state.p_ALONE-truth_ALONE)[index(na.omit(err.twostep_ALONE))]^2) / colMeans(na.omit(naive.p_Alone - truth_ALONE)[index(na.omit(err.twostep_ALONE))]^2)
ARGOX_ALONE_MSE = colMeans(na.omit(err.twostep_ALONE)[index(na.omit(err.twostep_ALONE))]^2) / colMeans(na.omit(naive.p_Alone - truth_ALONE)[index(na.omit(err.twostep_ALONE))]^2)
ARGOX_2Step_MSE = c(ARGOX_JOINT_MSE, ARGOX_ALONE_MSE)
ARGOX_2Step_MSE = ARGOX_2Step_MSE[match(names(list_results_step1.State),gsub("\\.","-",names(ARGOX_2Step_MSE)))]
argo2.p_2Step = cbind(argo2.p, argo2.p_ALONE)
argo2.p_2Step = argo2.p_2Step[,match(names(list_results_step1.State),gsub("\\.","-",names(argo2.p_2Step)))]
colnames(argo2.p_2Step) = names(list_results_step1.State)

ARGO_MSE = c(ARGO_JOINT_MSE, ARGO_ALONE_MSE)
ARGO_MSE = ARGO_MSE[match(names(list_results_step1.State),gsub("\\.","-",names(ARGO_MSE)))]
argo_StateLevel = cbind(argo.state.p_JOINT, argo.state.p_ALONE)
argo_StateLevel = argo_StateLevel[,match(names(list_results_step1.State),gsub("\\.","-",names(argo_StateLevel)))]
colnames(argo_StateLevel) = names(list_results_step1.State)
#
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ARGOX constraint nat ONLY. DOING it on all 51 states! ALL
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
argo.state.p_ALL = {}
for (i in names(list_results_step1.State)){
  if (temp_state[5,i]<=temp_state[4,i]){
    argo.state.p_ALL = cbind(argo.state.p_ALL, list_results_step1.State[[i]][,5])
  }
  else{
    argo.state.p_ALL = cbind(argo.state.p_ALL, list_results_step1.State[[i]][,4])
  }
}
colnames(argo.state.p_ALL) <- names(list_results_step1.State)
index(argo.state.p_ALL) <- as.Date(index(argo.state.p_ALL))

VT_HI = c("US-HI", "US-VT")
Not_VT_HI = names(list_results_step1.State)[!names(list_results_step1.State)%in%VT_HI] #VT and HI has many 0s in the covid daily death, thus disregard them and let national constraint be w_nat minus ARGO predictions 

state_info = state_region_info
truth_ALL = argo.state.true[,Not_VT_HI]
argo.state.p_ALL = argo.state.p_ALL[,Not_VT_HI]
use_yt2=TRUE
Nat_Constraint = TRUE
Lambda_CV = FALSE
Ensumble = FALSE
Lambda_Nat_Penalty = seq(-2,2,1e-2)

state_names_ALL = Not_VT_HI
# Take in truth (the true p at state level, matrix of number of states columns), predicted states p, regional p, national p, state names, which state to which region
argo2 <- function(truth_ALL, argo.state.p_ALL, argo.reg.p, argo.nat.p, argo.state_1.5, state_names_ALL, state_info, use_yt2=TRUE, Nat_Constraint = TRUE){
  training_period = 30  #COVID-19 use 70 days, 10 weeks training period
  Ensumble_training_period = 120
  
  naive.p_ALL <- truth_ALL 
  index(naive.p_ALL) <- index(truth_ALL) + 7*(1+LAG_PREDICT) #construct naive.p which simply uses this week's %ILI to predict next week's (7 days forward or n_forward=7*1, can change in Covid where basis is 1:1day instead of 7 days:1week)
  
  #Note that Y is a matrix where each columns contain true %ILI for each state
  Y <- truth_ALL - naive.p_ALL #obtain Z_t, the difference of p_t-p_{t-1}, here one difference in time steps is a week
  Yt2 <- Y 
  index(Yt2) <- index(Yt2) + 7*(1+LAG_PREDICT) #Yt2 is like Z_{t}=p_{t-1}-p_{t-2}, which is same as Z_{t-1}=p_{t-1}-p_{t-2}. i.e. at time t, Y[t] is Z_t while Yt2 is Z_{t-1}. Used for correlation calculation
  
  #common_idx <- index(na.omit(merge(naive.p_ALL, argo.state.p_ALL, argo.nat.p, argo.state_1.5, Yt2))) #get the common index
  common_idx <- index(na.omit(merge(naive.p_ALL, argo.state.p_ALL, argo.nat.p, Yt2))) 
  
  argo.nat.p <- argo.nat.p[common_idx]
  argo.reg.p <- argo.reg.p[common_idx]
  naive.p_ALL <- naive.p_ALL[common_idx]
  truth_ALL <- truth_ALL[common_idx]
  argo.state.p_ALL <- argo.state.p_ALL[common_idx]
  #argo.state_1.5 <- argo.state_1.5[common_idx]
  Yt2 <- Yt2[common_idx]
  #colMeans(truth_ALL-argo.state.p_ALL)/colMeans((naive.p_ALL-truth_ALL)^2)
  
  X <- argo.state.p_ALL - naive.p_ALL #obtain p^{GT}_t-p_{t-1}, state increment prediction for t
  #X_1.5 <- argo.state_1.5 - naive.p_ALL #obtain p^{AR}_t-p_{t-1}, step1.5 (case+death predictions) increment prediction for t
  X.nat <- as.numeric(argo.nat.p) - naive.p_ALL #obtain p^{nat}_t-p_{t-1}, national increment prediction for t. 
  X.nat <- X.nat[common_idx]#Note that everything here are same dimensions now, all having 51 columns (number of states), for region its the same value for all states in that region then minus truth (state level), which is stable 
  #for national its the same value accross all states, then when minus truth and get p^{nat}_t-p_{t} its also stable since p_{t} is different across states
  argo.reg.p.dup <- lapply(state_names_ALL, function(each_state){
    region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
    argo.reg.p[,region_id_for_state]
  })
  argo.reg.p.dup <- do.call(merge, argo.reg.p.dup)
  names(argo.reg.p.dup) <- state_names_ALL #Here, insteaad of having 10 columns in region p, we bump to 51 where simply duplicate the regions p for all states in that region. STORE IN argo.reg.p.dup
  X.reg <- argo.reg.p.dup - naive.p_ALL #obtain p^{reg}_t-p_{t-1}, here each column is the region that state is in's p (51 columns instead of 10), regional increment prediction for t
  
  projection.mat <- list()
  mean.mat <- list()
  
  Y.pred_ALL <- X
  Y.pred_ALL[] <- NA
  Y_NatConstraint.pred <- X
  Y_NatConstraint.pred[] <- NA
  NatConstraint_constant <-X[,1]
  colnames(NatConstraint_constant) <- "NatConstraint_constant"
  NatConstraint_constant[] <- NA
  Y.Lambda_Constraint <- X
  Y.Lambda_Constraint[] <- NA
  Lambda_Constraint <- X[,1]
  Lambda_Constraint[] <- NA
  Y.ensumble <- X
  Y.ensumble[] <- NA
  Ensumble_Opt <- X
  Ensumble_Opt[] <- NA
  
  zw_used <- list()
  fitting_error <- list()
  sigma_ww.structured <- sigma_ww.empirical <-
    sigma_zw.structured <- sigma_zw.empirical <-
    heat.vec.structured <-
    sigma_zwzw.structured <- sigma_zwzw.empirical <- list() #construct lists for variance matrix elements
  
  for(it in (training_period+7*(1+LAG_PREDICT)):length(common_idx) ){ #Note n_training in ARGO is 104
    if (it<=(Ensumble_training_period+7*(1+LAG_PREDICT))){ #Select the training id for ensumble model
      Ensumble_train_idx = as.Date(common_idx[it]) - ((Ensumble_training_period-7*(1+LAG_PREDICT)+1):(7*(1+LAG_PREDICT)))
      Ensumble_train_idx <- Ensumble_train_idx[Ensumble_train_idx >= common_idx[1]]
    }else{
      Ensumble_train_idx <- common_idx[(it-Ensumble_training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))]
    }
    training_idx <- common_idx[(it-training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))] #get training dates/period (total of fixed 104 days), same idea as first step (have a training period for prediction at t, which is why a "-1'at the end of range)
    t.now <- common_idx[it] #The time step t that we predict (testing upon training)
    y <- Y[training_idx,]  #get Z_t=p_t-p_{t-1}
    x <- X[training_idx,]  #get p^{GT}_t-p_{t-1}, state_level
    #x_1.5 <- X_1.5[training_idx,]  #get p^{AR}_t-p_{t-1}, state_level
    x.nat <- X.nat[training_idx,] #get p^{nat}_t-p_{t-1}
    x.reg <- X.reg[training_idx,] #get p^{reg}_t-p_{t-1}
    yt2 <- Yt2[training_idx,] #get Z_{t-1}
    
    sigma_yy <- var(y) #compute sigma_zz
    
    m1 <- cor(y, yt2) #??First, we are assuming each t (rows) follow a multivariate distribution, but here is computing according to column?
    m2 <- cor(y)  #Also, This way of cumpting ACF for one lag doesn't seem to be correct? Isn't it just computing sample correlation and scale by variance?
    m1[is.na(m1)] = 0
    m2[is.na(m2)] = 0
    rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
    
    autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
    
    vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_ALL - truth_ALL)[training_idx,]),sigma_yy, sigma_yy),
                              rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_ALL)[training_idx,]), sigma_yy),
                              rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_ALL)[training_idx,]) ) )
    
    #sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy,sigma_yy) #created the last 4 elements vector for sigma_zw (included step1.5 AR predictions)
    sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy) 
    
    if(use_yt2){ #whether using naive increment estimation in W "vector" also? if yes, then add-in its term in sigma_ww and sigma_zw
      #vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2,autocov.y.yt2,autocov.y.yt2,autocov.y.yt2)) #here its coded in different order than in the paper, see Pg11
      vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
      #vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy)) #why transpose here? isnt sigma_zz symmetric matrix?
      vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
      sigma_zw <- cbind(sigma_zw, autocov.y.yt2)
      
      ridge_sig = vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))) #get the ridge inspired sigma matrix
    }
    
    if(use_yt2 && Nat_Constraint){ # if enforcing summing state predictions equal to national truth
      w_nat = argo.nat.p[t.now] - sum(colMeans(y)) - sum(naive.p_ALL[t.now])  - sum(argo.state.p_ALONE[t.now,VT_HI]) #w_nat in the equation is national truth at t minus sum of state truth at t-1 (since predicted z is diff)
      w_t = as.matrix( c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) ) # w vector is mean 0
      #w_t = c(t(X[t.now,]), t(X.reg[t.now,]), t(X.nat[t.now,]), t(Yt2[t.now,]))
      one_vec = matrix(1,length(state_names_ALL),1)
      quad_frac = as.numeric( (w_nat - t(one_vec)%*%sigma_zw%*%solve(ridge_sig,w_t)) / (sum(one_vec) * t(w_t) %*% solve(ridge_sig, w_t)) ) #compute the constaint created constant
      A_hat = sigma_zw + quad_frac * (one_vec %*% t(w_t)) #compute the A_hat matrix, without the ridge inspired sigma matrix inverse multiplied yet
      y_natconstraint.pred <- A_hat %*% solve(ridge_sig, w_t)
      Y_NatConstraint.pred[t.now, ] <- t(y_natconstraint.pred)
      NatConstraint_constant[t.now, ] <- quad_frac
    }
    
    # if Using Cross Validation to find the lambda for enforcing summing state predictions equal to national truth
    if(use_yt2 && Lambda_CV){ 
      Sample_test_Date_list = list() 
      Sample_test_Date = sample(factor(rep(1:10, length.out=length(training_idx)))) #randomly assign dates to be one of the 10 folds
      for (i in 1:10){
        Sample_test_Date_list[[i]] = training_idx[Sample_test_Date==i] #determine which date is in which fold, for each lambda, train the rest 9 folds and test on that one fold.
      }
      CV_10fold_Err = matrix(0,10,length(Lambda_Nat_Penalty)) # matrix to store CV 10 fold error (objective value) for each lambda
      for (CV_folds in 1:10){
        CV_train_Date = training_idx[!training_idx%in%Sample_test_Date_list[[CV_folds]] ]
        CV_test_Date = training_idx[training_idx%in%Sample_test_Date_list[[CV_folds]] ]
        
        CV_train_sigma_yy <- var(Y[CV_train_Date,])
        CV_train_autocov.y.yt2 <- (sum(cor(Y[CV_train_Date,], Yt2[CV_train_Date,]) * cor(Y[CV_train_Date,]) ) / sum( cor(Y[CV_train_Date,]) ^2) ) * CV_train_sigma_yy 
        
        Sigma_ww_train <- cbind(rbind(CV_train_sigma_yy+var((argo.state.p_ALL - truth_ALL)[CV_train_Date,]),CV_train_sigma_yy, CV_train_sigma_yy),
                                rbind(CV_train_sigma_yy,CV_train_sigma_yy+var((argo.reg.p.dup - truth_ALL)[CV_train_Date,]), CV_train_sigma_yy),
                                rbind(CV_train_sigma_yy,CV_train_sigma_yy,CV_train_sigma_yy+var((as.numeric(argo.nat.p) - truth_ALL)[CV_train_Date,]) ) )
        Sigma_zw_train <- cbind(CV_train_sigma_yy,CV_train_sigma_yy,CV_train_sigma_yy) 
        
        Sigma_ww_train <- cbind(Sigma_ww_train, rbind(CV_train_autocov.y.yt2, CV_train_autocov.y.yt2, CV_train_autocov.y.yt2) ) 
        Sigma_ww_train <- rbind(Sigma_ww_train, cbind(t(CV_train_autocov.y.yt2),t(CV_train_autocov.y.yt2),t(CV_train_autocov.y.yt2),CV_train_sigma_yy))
        Sigma_zw_train <- cbind(Sigma_zw_train, CV_train_autocov.y.yt2)
        
        CV_train_ridge_sig = Sigma_ww_train + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[CV_train_Date,]))) #get the ridge inspired sigma matrix
        CV_train_ridge_sig[which(diag(CV_train_ridge_sig) == 0),which(diag(CV_train_ridge_sig) == 0)] = 1
        CV_train_ridge_sig_INV = solve(CV_train_ridge_sig) 
        Objec_Val= matrix(0,length(CV_test_Date),length(Lambda_Nat_Penalty))
        for (CV_fold_test_date_IDX in 1:length(CV_test_Date)){ #since there are multiple test dates in one fold, need to loop over
          CV_fold_test_date = CV_test_Date[CV_fold_test_date_IDX]
          CV_train_w_nat = argo.nat.p[CV_fold_test_date] - sum(naive.p_ALL[CV_fold_test_date]) - sum(colMeans(y)) - sum(argo.state.p_ALONE[CV_fold_test_date,VT_HI]) #w_nat in the equation is national truth at t minus sum of state truth at t-1 (since predicted z is diff)
          CV_train_w_t = as.matrix( c(t(X[CV_fold_test_date,])-colMeans(X[CV_train_Date,]), t(X.reg[CV_fold_test_date,])-colMeans(X.reg[CV_train_Date,]), 
                                      t(X.nat[CV_fold_test_date,])-colMeans(X.nat[CV_train_Date,]), t(Yt2[CV_fold_test_date,]-colMeans(Yt2[CV_train_Date,]))) ) # w vector is mean 0
          one_vec = matrix(1,length(state_names),1)
          for (Lambda_testing in 1:length(Lambda_Nat_Penalty) ){
            A_hat = (Sigma_zw_train + 2*Lambda_Nat_Penalty[Lambda_testing]*one_vec%*%t(CV_train_w_t) ) %*% CV_train_ridge_sig_INV #compute the A_hat matrix For CV
            Objec_Val[CV_fold_test_date_IDX, Lambda_testing] = 0.5*sum(diag(A_hat%*%CV_train_ridge_sig%*%t(A_hat))) - sum(diag(Sigma_zw_train%*%t(A_hat))) + Lambda_Nat_Penalty[Lambda_testing]*norm((CV_train_w_nat-t(one_vec)%*%A_hat%*%CV_train_w_t),"2")
          }
        }
        CV_10fold_Err[CV_folds,] = colMeans(Objec_Val)
      }
      Opt_Lambda = colMeans(CV_10fold_Err)
      Lambda_Constraint[t.now] = Lambda_Nat_Penalty[which.min(Opt_Lambda)]
      w_t = as.matrix( c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) ) 
      y.Lambda_Constraint = (sigma_zw + 2*Lambda_Constraint[t.now]*one_vec%*%t(w_t) ) %*% solve(ridge_sig , w_t) 
      Y.Lambda_Constraint[t.now, ] <- t(y.Lambda_Constraint)
    }
    
    # shrinked  
    if(use_yt2){
      y.pred_ALL <- colMeans(y) + sigma_zw %*% solve(ridge_sig, 
                                                     c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) )
      
      y.fitted <- colMeans(y) + sigma_zw %*% solve(ridge_sig, 
                                                   rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2))) )
      residual_epsilon <- y - t(y.fitted) #get fitted residual: true_training-predicted_training
    }else{
      y.pred_ALL <- colMeans(y) +
        sigma_zw %*% 
        solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5)[training_idx,]))), 
              c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(X_1.5[t.now,])-colMeans(x_1.5)))
    }
    
    Y.pred_ALL[t.now, ] <- t(y.pred_ALL) #store the prediction from equation (6)
    
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Ensumble of ARGO, ARGOX (no constraint) and ARGOX (constraint on sum to nat)
    if (Ensumble){
      if (dim(na.omit(Y.pred_ALL[Ensumble_train_idx]))[1] < 0){
        # Compute each method's out sample error for a period of time (15 days)
        Ensumble_ARGO_err = colMeans(na.omit(argo.state.p_ALL[Ensumble_train_idx] - truth_ALL[Ensumble_train_idx])^2) / colMeans(na.omit(naive.p_ALL[Ensumble_train_idx] - truth_ALL[Ensumble_train_idx])^2) 
        Ensumble_ARGOX_NoConstraint_ALL_err = colMeans(na.omit(Y.pred_ALL[Ensumble_train_idx]+naive.p_ALL[Ensumble_train_idx] - truth_ALL[Ensumble_train_idx])^2) / colMeans(na.omit(naive.p_ALL[Ensumble_train_idx] - truth_ALL[Ensumble_train_idx])^2) 
        
        Ensumble_ARGOX_NoConstraint_2Step_err = colMeans(na.omit(argo2.p[Ensumble_train_idx] - truth[Ensumble_train_idx])^2) / colMeans(na.omit(naive.p[Ensumble_train_idx] - truth[Ensumble_train_idx])^2)
        Ensumble_ARGOX_NoConstraint_2Step_err = c(Ensumble_ARGOX_NoConstraint_2Step_err,  colMeans(na.omit(argo2.p_ALONE[Ensumble_train_idx] - truth_ALONE[Ensumble_train_idx])^2) / colMeans(na.omit(naive.p_Alone[Ensumble_train_idx] - truth_ALONE[Ensumble_train_idx])^2) )
        Ensumble_ARGOX_NoConstraint_2Step_err = Ensumble_ARGOX_NoConstraint_2Step_err[match(state_names_ALL,names(Ensumble_ARGOX_NoConstraint_2Step_err))]
        
        Ensumble_ARGOX_NatConstraint_err = colMeans(na.omit(Y_NatConstraint.pred[Ensumble_train_idx]+naive.p_ALL[Ensumble_train_idx] - truth_ALL[Ensumble_train_idx])^2) / colMeans(na.omit(naive.p_ALL[Ensumble_train_idx] - truth_ALL[Ensumble_train_idx])^2) 
        Ensumble_err = rbind(Ensumble_ARGO_err, Ensumble_ARGOX_NoConstraint_ALL_err, Ensumble_ARGOX_NoConstraint_2Step_err , Ensumble_ARGOX_NatConstraint_err)
        Z_pred_now = rbind( (argo.state.p_ALL[t.now,] - naive.p_ALL[t.now,]),(argo2.p_2Step[t.now,] - naive.p_ALL[t.now,]), Y.pred_ALL[t.now, ], Y_NatConstraint.pred[t.now, ])
        # Find for each state to use which method's result
        Ensumble_Opt_Method = apply(Ensumble_err, 2 ,which.min)
        Ensumble_Opt[t.now,] = Ensumble_Opt_Method
        index2D <- function(v = Ensumble_Opt_Method, DF = Z_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
        Y.ensumble[t.now,] = index2D()
        #index2D_temp <- function(v = Ensumble_order, DF = Z_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
        #Y.ensumble[t.now,] = index2D_temp()
      }
      else{
        Z_pred_now = rbind( (argo.state.p_ALL[t.now,] - naive.p_ALL[t.now,]), Y.pred_ALL[t.now, ], Y_NatConstraint.pred[t.now, ])
        index2D_temp <- function(v = Ensumble_order, DF = Z_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
        Y.ensumble[t.now,] = index2D_temp()
      }
    }
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    ridge_sig = vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))) #get the ridge inspired sigma matrix
    ridge_sig[which(diag(ridge_sig) == 0),which(diag(ridge_sig) == 0)] = 1
    #projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5,Yt2)[training_idx,])))) #store siga_zw*(sigma_ww+D_ww)^{-1}   
    projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(ridge_sig)
    #mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat), colMeans(x_1.5),colMeans(yt2)) #store the training set's empirical mean for each forecast of t 
    mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat),colMeans(yt2))
    
    #Store everything in a list where each element contains all stuff for the forecast of time step t and its training time steps
    sigma_ww <- vcov.x_xreg_xnat 
    sigma_zz <- sigma_yy
    sigma_ww.structured[[as.character(t.now)]] <- sigma_ww #structured is using independence assumption and derived 
    #sigma_ww.empirical[[as.character(t.now)]] <- var(cbind(X,X.reg,X.nat,X_1.5, Yt2)[training_idx,]) #the empirical here is the matrix D (not using independence assumption)
    sigma_ww.empirical[[as.character(t.now)]] <- var(cbind(X,X.reg,X.nat, Yt2)[training_idx,])
    sigma_zw.structured[[as.character(t.now)]] <- sigma_zw
    #sigma_zw.empirical[[as.character(t.now)]] <- cov(Y[training_idx,], cbind(X,X.reg,X.nat,X_1.5,Yt2)[training_idx,])
    sigma_zw.empirical[[as.character(t.now)]] <- cov(Y[training_idx,], cbind(X,X.reg,X.nat,Yt2)[training_idx,])
    
    sigma_zwzw.structured[[as.character(t.now)]] <- rbind( #The huge covariance matrix of Z (1 "element") and W (4 "elemtns")
      cbind(sigma_zz, sigma_zw),
      cbind(t(sigma_zw), sigma_ww)
    )
    #sigma_zwzw.empirical[[as.character(t.now)]] <- var(cbind(Y,X,X.reg,X.nat,X_1.5,Yt2)[training_idx,])
    sigma_zwzw.empirical[[as.character(t.now)]] <- var(cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,])
    #zw_used[[as.character(t.now)]] <- cbind(Y,X,X.reg,X.nat,X_1.5,Yt2)[training_idx,] #For forecasting each time t, store the training set used all elements
    zw_used[[as.character(t.now)]] <- cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,] 
    fitting_error[[as.character(t.now)]] <- residual_epsilon 
  }
  
  projection.mat <- sapply(projection.mat, identity, simplify = "array") #convert a list into 3D matrix (dimensions agree)
  mean.mat <- sapply(mean.mat, identity, simplify = "array") #convert to a 2D matrix, each column is a mean vector for forecast of time step t. (5*51 columns)
  
  argo2.p_ALL <- Y.pred_ALL + naive.p_ALL #the actual forecast of %ILI for time t (p_t=p_{t-1}+(p_t-p_{t-1})) is p_{t-1}+\hat{Z}_t
  argo2_NatConstraint.p <- Y_NatConstraint.pred + naive.p_ALL 
  argo2_Lambda_Constraint.p <- Y.Lambda_Constraint + naive.p_ALL
  argo2_Ensumble.p <- Y.ensumble + naive.p_ALL
  argo2_Ensumble.p[1:(training_period+6+LAG_PREDICT),] = argo.state.p_ALL[1:(training_period+6+LAG_PREDICT),]
  argo2.p_ALL[argo2.p_ALL<0] = 0
  argo2_NatConstraint.p[argo2_NatConstraint.p<0] = 0
  argo2_Lambda_Constraint.p[argo2_Lambda_Constraint.p<0] = 0
  argo2_Ensumble.p[argo2_Ensumble.p<0] = 0
  
  err.twostep_ALL <- argo2.p_ALL - truth_ALL #compute the forecasting error. Here 2-step means using 2-step ARGOX, 1-step means just using Google search step 1 only.
  err_NatConstraint.twostep <- argo2_NatConstraint.p - truth_ALL
  err_Lambda_Constraint.twostep <- argo2_Lambda_Constraint.p - truth_ALL
  err_Ensumble.twostep <- argo2_Ensumble.p - truth_ALL
  
  #Store "vector" [Z_t,W'_t], where Z_t=p_t-p_{t-1}, W'_t=[p^{GT}_t-p_t, p^{reg}_t-p_t, p^{nat}_t-p_t, p_{t-1}-p_{t-2}] where W'_t are the independent error vectors 
  heat.vec <- na.omit(merge(truth_ALL-naive.p_ALL, argo.state.p_ALL - truth_ALL, argo.reg.p.dup - truth_ALL, as.numeric(argo.nat.p) - truth_ALL,argo.state_1.5-truth_ALL,Yt2))
  colnames(heat.vec) <- paste0(rep(c("True Increment", "err.argo.", "err.reg.", "err.nat.", "err.argo.AR.", "err.y2."), each=length(state_names)), state_names)
  
  #convert all matrix stored as lists above to 3D arrays
  sigma_ww.structured <- sapply(sigma_ww.structured, identity, simplify = "array")
  sigma_ww.empirical <- sapply(sigma_ww.empirical, identity, simplify = "array")
  sigma_zw.structured <- sapply(sigma_zw.structured, identity, simplify = "array")
  sigma_zw.empirical <- sapply(sigma_zw.empirical, identity, simplify = "array")
  sigma_zwzw.structured <- sapply(sigma_zwzw.structured, identity, simplify = "array")
  sigma_zwzw.empirical <- sapply(sigma_zwzw.empirical, identity, simplify = "array")
  zw_used <- sapply(zw_used, identity, simplify = "array")
  
  list(onestep=argo.state.p_ALL, twostep=argo2.p_ALL, naive=naive.p_ALL, truth=truth_ALL,
       Y.pred_ALL=Y.pred_ALL, err.twostep=err.twostep_ALL,
       heat.vec=heat.vec, projection.mat=projection.mat, mean.mat=mean.mat,
       sigma_ww.structured=sigma_ww.structured, sigma_ww.empirical=sigma_ww.empirical,
       sigma_zw.structured=sigma_zw.structured, sigma_zw.empirical=sigma_zw.empirical,
       sigma_zwzw.structured=sigma_zwzw.structured, sigma_zwzw.empirical=sigma_zwzw.empirical,
       zw_used=zw_used, zw_overall = cbind(Y,X,X.reg,X.nat,X_1.5,Yt2), fitting_error=fitting_error)
}
