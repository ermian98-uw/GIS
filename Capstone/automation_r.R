library(magick)
library(rvest)
library(RSelenium)
library(pracma)
library(stringr)
library(tidyverse)
library(rhdf5)
library(installr)
library(fitdistrplus)
library(logspline)
library(tseries)
library(forecast)
library(oce) ; require(suncalc)

# Eric Anderson #
# University of Washington - Tacoma #
# August 13th, 2021 #
# Automation Code, R portion only #

##################
# Step ONE: Obtain the flyby times of the ECOSTRESS sensor over Washington State.

# All data were downloaded from the NASA Earthdata server via a list of web addresses (see step 3).
# Within the web addresses was information about each flyby, including time of flyby in UTC.
# The following code extracts these times to determine which temperature reading to pull from the weather station (see step 3).

c_links <- scan("CLOUD Links.txt", character(), sep = ",")
g_links <- scan("GEO Links.txt", character(), sep = ",")
l_links <- scan("LST Links.txt", character(), sep = ",")

# Parse dates
c_date <- substring(c_links,97,104)
c_year <- c(); c_month <- c(); c_day <- c(); c_newdate <- c()

for (i in 1:length(c_date)) {
  c_year <- c(c_year,substring(c_date[i],3,4))
  c_month <- c(c_month,substring(c_date[i],5,6))
  c_day <- c(c_day,substring(c_date[i],7,8))
}
c_newdate <- paste(c_month,'/',c_day,'/',c_year, sep = "")

# Parse times
c_time <- substring(c_links,106,111)
c_hour <- c(); c_minute <- c(); c_second <- c(); c_newtime <- c();

for (i in 1:length(c_time)) {
  c_hour <- c(c_hour,substring(c_time[i],1,2))
  c_minute <- c(c_minute,substring(c_time[i],3,4))
  c_second <- c(c_second,substring(c_time[i],5,6))
}

c_newtime <- paste(c_hour,':',c_minute,':',c_second, sep = "")
c_new <- strptime(paste(c_newdate,c_newtime),"%m/%d/%y %H:%M:%S") # Combine in POSIXct format

# Adjust UTC to PDT or PST (depending on time of year)
for (i in 1:length(c_new)) {
  if (c_new[i] < "2020-11-01 09:00:00 PDT") { c_new[i] <- c_new[i] - 25200 } # seconds
  else { c_new[i] <- c_new[i] - 28000 } # seconds
}

# Store results
write.csv(c_new,paste("C:\\Users\\eric-\\Desktop\\MGST_Final\\Web_Harvest\\flyby_moments\\_flyby_moments.csv", sep=""))


##################
# Step TWO: web harvest of all possible Wunderground PWS (personal weather stations) in the state of Washington. 

# The station IDs are constructed as such: K + [state] + [first 5 letters of community] + ['nth' station in operation].
# Example: KWATACOM14 would be the 14th station established in Tacoma, WA.
# From Wikipedia, I obtained a list of all incorporated/unincorporated communities in the state and extracted the first five letters of each.
# I iterated through each 5-letter string until I was confident all stations were obtained.
# My criterion was: if 25 numbers pass without a new station, that community is finished (there were some exceptions).
# I found 15,116 stations and their latitude/longitudes.

driver1 <- rsDriver(browser=c("chrome"), chromever="87.0.4280.88") # Will need to be updated as Chrome updates
remote_driver1 <- driver1$client
remote_driver1$open()

date_url <- "2020-03-19" ; date_csv <- "Mar192020"
wa_comm <- scan("wa_communities.txt", character(), sep = "\n") ; wa_comm <- wa_comm

start <- 1 ; end <- 5000
count <- 1
heading1 <- date_csv

coords1 <- matrix(data = NA, nrow = (end-start+1)*length(wa_comm), ncol = 3)
coords_head1 <- ""

skip_count <- 0
for (h in 1:length(wa_comm)) {
  skip_count <- 0
  
  for (i in start:end) {
    skip_count <- skip_count + 1
    print(paste(h, " ", i, " ", skip_count))
    if (skip_count > 25) { # to check if all stations have probably been itemized
      count <- count + (end-i) + 1
      print(paste(wa_comm[h],i, sep = ""))
      break       
    }
    else {
      try ( {
        remote_driver1$navigate(paste("https://www.wunderground.com/dashboard/pws/KWA",wa_comm[h],i, sep = ""))
        Sys.sleep(0.8)
        
        html1 <- remote_driver1$getPageSource()[[1]]
        head_text1 <- read_html(html1) %>% # parse HTML
          html_nodes("title") %>%
          html_text()
        
        while (head_text1 == "www.wunderground.com"[1]) {
          print(print(paste(wa_comm[h],i, sep = "")))
          Sys.sleep(20) # every 20 seconds
          remote_driver1$navigate(paste("https://www.wunderground.com/dashboard/pws/KWA",wa_comm[h],i, sep = "")) # re-load page until internet is back
          Sys.sleep(0.4)
          html1 <- remote_driver1$getPageSource()[[1]]
          head_text1 <- read_html(html1) %>% # re-parse HTML
            html_nodes("title") %>%
            html_text()
        }
        icon1 <- remote_driver1$findElement("css", "mat-icon[role='img']") 
        icon1$clickElement()
        
        html1 <- remote_driver1$getPageSource()[[1]]
        head_text1 <- read_html(html1) %>% # parse HTML
          html_nodes("title") %>%
          html_text()
        
        icon_text1 <- read_html(html1) %>% # parse HTML
          html_nodes(".leftcol_wrapper") %>%
          html_text()
        
        heading1 <- str_match(icon_text1, "Weather Station ID: \\s*(.*?)\\s*Station Name:")[,2]
        lat1 <- as.numeric(str_match(icon_text1, "Longitude:\\s*(.*?)\\s*° N")[,2])
        long1 <- as.numeric(paste("-",str_match(icon_text1, "N, \\s*(.*?)\\s*° W")[,2], sep=""))
        
        loc_row1 <- c(heading1, lat1, long1)
        coords1[count,] <- loc_row1
        coords_head1 <- c(coords_head1, heading1)
        skip_count <- 0
      })
      
      if (strcmp(heading1,"")) { coords_head1 <- c(coords_head1, " ") }
      count <- count + 1
    }
  }
}
colnames(coords1) <- c("STATION", "LATITUDE", "LONGITUDE")
coords1 <- coords1[rowSums(is.na(coords1)) != ncol(coords1),]

##################
remote_driver$close()
write.csv(coords1,"C:\\Users\\eric-\\Desktop\\MGST_Final\\Web_Harvest\\stations.csv")


##################
# Step THREE: web harvest of temperature data corresponding to weather stations beneath an ECOSTRESS flyby

# Approximately 5,323 of the 15,116 stations were online during the time of study (June 2020 - June 2021).
# This was estimated by checking if temperature data existed in June 2020, Feb 2021, and June 2021.
# A station was deemed "online" if one of the three time frames yielded data. All other stations were considered "deactivated".
# The code below accomplished this. You'll see it iterating in chronological order, but I ran it according to the above time frames first to get the online stations first.
# The code below checks for all weather readings within 5 minutes of the ECOSTRESS flyby and averages them.
# Then it obtains the surface temperature of the pixel the station sits in (see automation_python.py file).
# A csv file for each flyby is outputted. 179,623 temperature observations around the state were recorded.


# Import txt files with download links from NASA Earthdata
cloud_links <- read.table("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/Data Links/All CLOUD Links.txt")
geo_links <- read.table("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/Data Links/All GEO Links.txt")
lst_links <- read.table("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/Data Links/All LST Links.txt")
keys = read.table("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/Data Links/___All_Links_Key.txt")

# Initate remote driver (RSelenium)
a_driver <- rsDriver(browser=c("chrome"), chromever="91.0.4472.101", port = 4444L)
a_remote_driver <- a_driver$client
a_remote_driver$open()

# Entire automation loop (CHECK LAST.TXT FOR STARTING INCREMENT)
for (k in 1:1200) {
  
  # Download data programatically ( download folder: C:\Users\eric-\Desktop\MGST_Final\ECOSTRESS_processing\GeoRef )
  link_list <- c(as.character(cloud_links[k,1]),as.character(geo_links[k,1]),as.character(lst_links[k,1]))
  filename_len <- c(54,53,53)
  sleep_num <- 0
  for (i in 1:3) {
    dl_check <- FALSE
    a_remote_driver$navigate(link_list[i])
    sleep_num <- 0
    while (!dl_check) {
      if (TRUE %in% (list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef") == 
                     substring((link_list[i]),nchar(link_list[i])-filename_len[i],nchar(link_list[i]))))
        dl_check <- TRUE
      Sys.sleep(3)
      sleep_num <- sleep_num + 1
      if (sleep_num >= 148) {
        a_remote_driver$refresh()
        sleep_num <- 0
      }
    }
  }
  
  # Get mean solar zenith and solar azimuth values from GEO file for later
  my_geo_h5 <- list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef")[1]
  zenith = 90 - h5read(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/", my_geo_h5, sep = ''),"/L1GEOMetadata/AverageSolarZenith")
  azimuth = mean(h5read(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/", my_geo_h5, sep = ''),"/Geolocation/solar_azimuth")) - 180
  latitude <- mean(h5read(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/", my_geo_h5, sep = ''),"/Geolocation/latitude"))
  longitude <- mean(h5read(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/", my_geo_h5, sep = ''),"/Geolocation/longitude"))
  cloud_cover <- list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef")[2]
  cloud_cover <- h5read(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/", cloud_cover, sep = ''),"/L2 CLOUD Metadata/QAPercentCloudCover")
  
  print(k)
  print(zenith)
  print(azimuth)
  print(cloud_cover)
  print(latitude)
  print(longitude)
  
  # Execute Python program (ECOSTRESS_swath2grid.py) to create GeoTIFF files
  py_check = TRUE
  while (py_check) {
    if ("python.exe" %in% get_tasklist()[,1]) {
      print("Waiting 1...")
      Sys.sleep(30)
    }
    else { 
      print("Starting 1...")
      py_check = FALSE 
    }
  }
  shell("C:/Users/eric-/AppData/Local/Programs/Python/Python36/python C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/ECOSTRESS_swath2grid.py --proj UTM --dir C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef --sds LST,CloudMask")
  
  # Execute custom Python program (python_automation.py) to do arcpy geoprocessing
  # Takes hillshade values as arguments
  py_check = TRUE
  while (py_check) {
    if ("python.exe" %in% get_tasklist()[,1]) {
      print("Waiting 2...")
      Sys.sleep(30)
    }
    else { 
      print("Starting 2...")
      py_check = FALSE 
    }
  }
  shell(paste("C:/Users/eric-/AppData/Local/Programs/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python C:/Users/eric-/Desktop/MGST_Final/python_automation.py", azimuth, zenith))
  
  if (length(list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output")) >= 27) {
    
    # Begin implementing web harvest
    tf <- list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output")[1]
    tf_shade <- list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output")[3]
    time <- scan("C:/Users/eric-/Desktop/MGST_Final/Web_Harvest/times.txt", character(), sep = ",")
    time_am_pm <- scan("C:/Users/eric-/Desktop/MGST_Final/Web_Harvest/times_am_pm.txt", character(), sep = ",")
    
    # Read station files (one table has LST values, the other has shade values, they will be bound later)
    STATIONS_TO_READ <- read.table(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/",tf,sep=""), header= FALSE, sep = ",")
    STATIONS_TO_READ <- as.numeric(as.vector(STATIONS_TO_READ[2:length(STATIONS_TO_READ[,3]),3])) + 1
    SHADE_STATUS <- read.table(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/",tf_shade,sep=""), header= FALSE, sep = ",")
    SHADE_STATUS <- SHADE_STATUS[2:length(SHADE_STATUS[,3]),2:3]
    SHADE_STATUS[,2] <- as.numeric(as.vector(SHADE_STATUS[,2])) + 1
    if (zenith <= 0) { colnames(SHADE_STATUS) <- c('Deg ALL DARK','SrcID') }
    if (zenith > 0) { colnames(SHADE_STATUS) <- c('Deg','SrcID') }
    station_date <- paste(substring(tf,5,8),substring(tf,1,4), sep = "")
    if (nchar(tf) == 16) { station_time <- paste("0",substring(tf,10,12),sep='') }
    if (nchar(tf) == 15) { station_time <- paste("00",substring(tf,10,11),sep='') }
    if (nchar(tf) < 15 | nchar(tf) > 16) { station_time <- substring(tf,10,13) }
    station_as_date <- strptime(paste(station_date,station_time), format = "%M%d%Y %H%M")
    station_as_time <- strptime(station_time, format = "%H%M")
    date_url <- paste(substring(tf,1,4),"-",substring(tf,5,6),"-",substring(tf,7,8),sep="")
    
    flyby <- read.table("C:/Users/eric-/Desktop/MGST_Final/Web_Harvest/flyby_moments/flyby_moments.txt", header= FALSE, sep = "\t")
    if (nchar(tf) == 16) { flyby_compare <- paste(substring(tf,6,6),"/", substring(tf,8,8),"/", substring(tf,1,4), " 0", substring(tf,10,10), ":", substring(tf,11,12), sep = "") }
    if (nchar(tf) == 15) { flyby_compare <- paste(substring(tf,6,6),"/", substring(tf,8,8),"/", substring(tf,1,4), " 00:", substring(tf,10,11), sep = "") }
    if (nchar(tf) < 15 | nchar(tf) > 16) { flyby_compare <- paste(substring(tf,6,6),"/", substring(tf,8,8),"/", substring(tf,1,4), " ", substring(tf,10,11), ":", substring(tf,12,13), sep = "") }
    flyby_num <- gsub(" ", "", flyby[flyby[,2] == flyby_compare,][1])
    
    all_stations <- read.table("C:/Users/eric-/Desktop/MGST_Final/Web_Harvest/all_stations_online.txt", header = FALSE, sep = "\t")
    colnames(all_stations) <- c("STATION", "LATITUDE", "LONGITUDE")
    tab <- matrix(data = NA, nrow = 290, ncol = 1 + length(STATIONS_TO_READ))
    tab <- as.data.frame(tab)
    tab_head <- c()
    tab_oid <- c()
    count <- 2
    remove_list <- c()
    
    for (h in STATIONS_TO_READ) { # populate all available temperatures
      try ( {
        a_remote_driver$navigate(paste("https://www.wunderground.com/dashboard/pws/",all_stations[h,1],"/table/",date_url,"/",date_url,"/daily", sep = ""))
        
        html <- a_remote_driver$getPageSource()[[1]]
        head_text <- read_html(html) %>% # parse HTML
          html_nodes("title") %>%
          html_text()
        
        skip_count <- 0
        while (head_text == "Oops! There's been an error | Weather Underground"[1] & skip_count == 6) { # if page does not load / does not exist
          print(print(paste(all_stations[h,1], " Error", sep = "")))
          Sys.sleep(15) # every 15 seconds
          a_remote_driver$navigate(paste("https://www.wunderground.com/dashboard/pws/",all_stations[h,1],"/table/",date_url,"/",date_url,"/daily", sep = "")) # re-load page until internet is back
          Sys.sleep(0.4)
          skip_count <- skip_count + 1
          html <- a_remote_driver$getPageSource()[[1]]
          head_text1 <- read_html(html) %>% # re-parse HTML
            html_nodes("title") %>%
            html_text()
        }
        if (skip_count > 5) { remove_list <- c(remove_list,as.character(all_stations[h,1])) }
        
        site1 <- read_html(paste("https://www.wunderground.com/dashboard/pws/",all_stations[h,1],"/table/",date_url,"/",date_url,"/daily", sep = "")) %>%
          html_nodes("tr.ng-star-inserted") %>%
          html_text()
        site2 <- NA
        
        if (length(site1) != 0) {
          site2 <- site1[nchar(site1[]) < 12]
          site1 <- substring(site1[nchar(site1[]) > 12],0,17)
          temps <- c()
          site_time <- NA
          comptime <- NA
          
          t_count <- 1
          for (i in 1:length(site1)) { # get timing and temps
            comp_time <- strptime(substring(time_am_pm[t_count], 3, nchar(time_am_pm[t_count])), format = "%I:%M %p")
            site_time <- strptime(site2[i], format = "%I:%M %p")
            difftime <- as.numeric(difftime(comp_time, site_time, units = "min"))
            difftime_for_compare <- as.numeric(difftime(comp_time, station_as_time, units = "min"))
            
            while (abs(difftime) > 4) { # ensure the time on Wunderground is within 5 minutes of my standard time vector (5-min intervals)
              if (length(temps) < 287) { temps <- append(temps,"N/A",after=t_count) }
              t_count = t_count + 1
              comp_time <- strptime(substring(time_am_pm[t_count], 3, nchar(time_am_pm[t_count])), format = "%I:%M %p")
              difftime <- as.numeric(difftime(comp_time, site_time, units = "min"))
            }
            t_count = t_count + 1
            
            if (abs(difftime_for_compare) <= 5) {
              if (is.na(str_match(site1[i], "PM\\s*(.*?)\\s*°F")[2])) { temps <- c(temps, as.numeric(str_match(site1[i], "AM\\s*(.*?)\\s*°F")[2])) }
              if (!is.na(str_match(site1[i], "PM\\s*(.*?)\\s*°F")[2])) { temps <- c(temps, as.numeric(str_match(site1[i], "PM\\s*(.*?)\\s*°F")[2])) }
            } else { temps <- c(temps, "N/A") }
          }
          Sys.sleep(0.25)
          while (length(temps) < 287) {
            temps <- append(temps,"N/A", after = length(temps))
          }
          temps <- c(temps, STATIONS_TO_READ[count-1])
          if (length(temps) == 289) { tab[1:289,count] <- as.numeric(temps) }
          tab[290,count] <- as.character(all_stations[h,1])
          tab_head <- c(tab_head, tab[290,count])
          tab_oid <- c(tab_oid, STATIONS_TO_READ[count-1])
        }
      })
      print(paste(count-1, "of", length(STATIONS_TO_READ)))
      count <- count + 1
    }
    tab <- tab[1:288,2:length(tab)]
    if (!is.null(nrow(tab))) {
      tab_cols <- length(tab[1,])
      tab_rows <- length(tab[,1])
      tab <- t(tab)
      tab <- mapply(tab, FUN=as.numeric)
      tab <- matrix(data=tab, ncol=tab_rows, nrow=tab_cols)
      rownames(tab) <- STATIONS_TO_READ
      tab <- tab[,colSums(is.na(tab)) < nrow(tab)]
      
      newcol <- paste(substring(station_date,1,8), "_", station_time, "_", cloud_cover, sep = "")
      if (class(tab) == "matrix") {
        tab <- as.data.frame(rowMeans(tab,na.rm = T))
        tab[tab == "NaN"] <- -100
      }
      if (class(tab) == "numeric") {
        tab <- as.data.frame(tab)
        tab[is.na(tab)] <- -100
      }
      
      # Combine data frames for single dataset per flyby (with all variables present except land cover type, to be manually added much later)
      tab <- cbind(rep(newcol, times = tab_cols), tab)
      tab <- tibble::rownames_to_column(tab, "SrcID") 
      colnames(tab) <- c("SrcID", "Flyby", "Air_Temp")
      tab2 <- merge(SHADE_STATUS, tab, by = 'SrcID', all.y = TRUE)
      
      LST_tab <- read.table(paste("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/",tf,sep=""), header= FALSE, sep = ",")
      LST_tab <- LST_tab[2:length(LST_tab[,3]),2:3]
      LST_tab[,2] <- as.numeric(as.vector(LST_tab[,2])) + 1
      colnames(LST_tab) <- c('Surf_Temp','SrcID')
      tab3 <- merge(LST_tab, tab2, by = 'SrcID', all.y = TRUE)
      
      write.csv(tab3,paste("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/_temp_harvests/",
                           substring(station_date,1,9),"_",station_time,"_",cloud_cover,".csv", sep=""), row.names=FALSE)
    }
    
    last_one <- data.frame(flyby_num,k,substring(link_list[i],nchar(link_list[i])-25,nchar(link_list[i])))
    colnames(last_one) <- c('Flyby_Num',"k",'Last Flyby Downloaded')
    write.table(last_one,"C:/Users/eric-/Desktop/MGST_Final/last.txt", row.names = FALSE)    
  }
  
  # Delete .h5 files downloaded at the beginning, and the output files from this flyby
  do.call(file.remove, list(list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef", full.names = TRUE)))
  do.call(file.remove, list(list.files("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output", full.names = TRUE)))
}


##################
# Step FOUR: Remove outliers from step three.

# After amalgamating all the csv files from step three and calculating penalties (surface minus air temperature), I needed to remove penalty outliers.
# The code below does this by transforming the penalties to the closest known distribution (in this case, Gamma).
# Then, it removes observations that score too high a Cook's Distance (used to identify influential outliers in regression analysis).
# 95.5% of the observations were kept (totalling 171,697).


r = read.csv("_MASTER.csv")
penalties = r$Penalty

descdist(penalties, discrete = FALSE) # Excess kurtosis = kurtosis - 3
BoxCox.lambda(penalties, method = "guerrero")
trans_p = BoxCox(penalties,0.693) # Transformation where lambda = 0.693

descdist(as.vector(trans_p), discrete = FALSE) # Fits a gamma distribution Gamma~(5.85,0.35)
# summary statistics
# ------
#   min:  -31.79197   max:  47.03808 
# median:  -4.203574 
# mean:  -2.060279 
# estimated sd:  6.930028 
# estimated skewness:  0.8270493 
# estimated kurtosis:  4.018003 

p2 = rgamma(179623,shape = 5.85,rate = 0.35) 
t.test(trans_p,p2)
hist(trans_p, breaks = 50)
hist(p2, breaks = 50)

p_res = glm(trans_p~p2)
# p_res <- (p_res$residuals/(1 - p_res$fitted.values))^2 * p_res$fitted.values/(1 * 179623)  # Alternative calculation to Cook's Distance
p_c = cooks.distance(p_res)
thres = 4/179623 # Remove outliers with Cook's Distance, using the traditional 4/n criterion
outcome = names(p_c[p_c <= thres])

delete = as.data.frame(r[as.integer(outcome),])
write.csv(delete,"_MASTER_Cleaned_No_Outliers.csv", row.names = FALSE)


##################
# Step FIVE: Obtain angle of sun for each observation from step 4.

# This was essential for classifying penalties by sun altitude.
# The suncalc library was used to take each observation's lat/long coordinates, and weather observation time, to determine altitude of sun.
# These data were appended to the MASTER csv.

mydata <- read.table("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__MASTER/_MASTER_Cleaned.txt", header= TRUE, sep = "\t")

time = mydata[1:17]
time = as.data.frame(time)
new_time = as.POSIXct(paste(as.Date(time$Date,"%m/%d/%y"), format(strptime(time$Time, "%I:%M %p"), "%H:%M:%S")))

sun_Angle = as.data.frame(sunAngle(new_time,time$Long,time$Lat,useRefraction = TRUE))
write.csv(sun_Angle,"sun_angle.csv", row.names=FALSE)

##############################
# Calculate high noon
gst_list = c()
for (i in 1:171697) { # There were 171,697 observations
  gst_list = c(gst_list,getSunlightTimes(as.Date(new_time[i]),time[i,2],time[i,3])[4])
}

g = do.call("c", gst_list)
write.csv(g,"solar_noon.csv", row.names=FALSE)


##################
# Step SIX: Verify precision of model (data validation)

# By this point, I've grouped all penalties by variable (all combinations of season x sun altitude x land cover x shade status)
# I want to see if these grouped penalties are precise (seeing if I can replicate them again, or if my model has too much variance).
# This step does not improve the model accuracy, but rather verifies that the results reflect what should happen if implemented in real life (reduces variance).
# This method is called "bagging" - bootstrap aggregating. The following is "bootstrapped" (done) 1,000 times:
# A random training sample from the original data is collected, with penalties calculated for that sample. This is tested on the remaining samples.
# Statistics demonstrating how much the penalties corrected surface temperature are calculated.
# The mean of the entire bagging process is taken and used as my penalties.


mydata = read.csv("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__MASTER/_MASTER_Cleaned_No_Outliers.csv")
my_pen = read.csv("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__MASTER/_Penalties.csv")

mydata[,"Air_Temp_w_Real_Pen"] = NA
mydata[,"Diff"] = NA

merged_data = merge(mydata, my_pen, by.x = c( "Season", "Land.Cover.Type","Altitude.Category","Shade.Status"),
                    by.y = c( "ï..Season", "Land.Cover.Type","Altitude.Category","Shade.Status"))

merged_data[,"Air_Temp_w_Real_Pen"] = merged_data[,"Surf_Temp.x"] - merged_data[,"Penalty.y"]
merged_data[,"Diff"] = (merged_data[,"Air_Temp.x"] - merged_data[,"Air_Temp_w_Real_Pen"])*-1
# This was manually added to #_MASTER_Cleaned_No_Outliers.csv"

orig_data = read.csv("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__MASTER/_MASTER_Cleaned_No_Outliers.csv")
orig_data = cbind(orig_data[1:17],orig_data[21])

# Cumulative matrices to hold final averaged penalty values by group
cum_lc_p = matrix(0, ncol = 6, nrow = 15)
cum_sh_p = matrix(0, ncol = 6, nrow = 3)
cum_al_p = matrix(0, ncol = 6, nrow = 9)
cum_se_p = matrix(0, ncol = 6, nrow = 4)
cum_all_p = matrix(0, ncol = 6, nrow = 822)

# Calculate how well the penalty application did
CalcOutcome = function(x) {
  if (((x[18]*x[21]) < 0) & abs(x[21]) <= abs(x[18])) { x[22] = "Better Too Far" }
  else if (((x[18]*x[21]) < 0) & abs(x[21]) > abs(x[18])) { x[22] = "Overshot" }
  else if (((x[18]*x[21]) > 0) & abs(x[21]) <= abs(x[18])) { x[22] = "Better Not Enough" }
  else if (((x[18]*x[21]) > 0) & abs(x[21]) > abs(x[18])) { x[22] = "Wrong Direction" }
  else { x[22] = "Perfect" }
  return (x[22])
} 

# Bagging (bootstrap aggregating) algorithm
for (x in 1:1000) {
  
  # Create training and test sets at a 70-30 ratio
  mydata = orig_data[sample(seq(1:171697),171697,replace=TRUE),]
  train = mydata[1:120188,]
  test = mydata[120188:171697,]
  new_penalties = aggregate(train[,17], list(train$concat), mean)
  mydata = merge(test, new_penalties, by.x = c( "concat"), by.y = c("Group.1"))
  
  mydata[,"Air_Temp_w_Real_Pen"] = mydata[,"Surf_Temp"] - mydata[,"x"]
  mydata[,"Diff"] = (mydata[,"Air_Temp"] - mydata[,"Air_Temp_w_Real_Pen"])*-1
  mydata[,"Outcome"] = NA
  mydata[,"Outcome"] = apply(mydata, 1, function(x) CalcOutcome(as.numeric(x)))
  
  # Tables
  lc = table(mydata[,"Outcome"],mydata[,"Land.Cover.Type"])
  sh = table(mydata[,"Outcome"],mydata[,"Shade.Status"])
  al = table(mydata[,"Outcome"],mydata[,"Altitude.Category"])
  se = table(mydata[,"Outcome"],mydata[,"Season"])
  all = table(mydata[,"Outcome"],mydata[,"concat"])
  
  columns = c("Better Not Enough","Better Too Far","Overshot","Perfect","Wrong Direction","Count","Orig. Error","Corrected Error")
  
  # Get proportions
  lc_p = matrix(NA, ncol = 6, nrow = ncol(lc)) ; rownames(lc_p) = colnames(lc) ; for (i in 1:ncol(lc)) { lc_p[i,] = c(round((prop.table(lc[,i])*100),2),colSums(lc)[[i]]) }
  sh_p = matrix(NA, ncol = 6, nrow = ncol(sh)) ; rownames(sh_p) = colnames(sh) ; for (i in 1:ncol(sh)) { sh_p[i,] = c(round((prop.table(sh[,i])*100),2),colSums(sh)[[i]]) }
  al_p = matrix(NA, ncol = 6, nrow = ncol(al)) ; rownames(al_p) = colnames(al) ; for (i in 1:ncol(al)) { al_p[i,] = c(round((prop.table(al[,i])*100),2),colSums(al)[[i]]) }
  se_p = matrix(NA, ncol = 6, nrow = ncol(se)) ; rownames(se_p) = colnames(se) ; for (i in 1:ncol(se)) { se_p[i,] = c(round((prop.table(se[,i])*100),2),colSums(se)[[i]]) }
  all_p = matrix(NA, ncol = 6, nrow = ncol(all)) ; rownames(all_p) = colnames(all) ; for (i in 1:ncol(all)) { all_p[i,] = c(round((prop.table(all[,i])*100),2),colSums(all)[[i]]) }
  
  lc_p = cbind(as.data.frame(lc_p),aggregate(abs(mydata$Penalty), list(mydata$Land.Cover.Type), mean, drop = FALSE)[2],aggregate(abs(mydata$Diff), list(mydata$Land.Cover.Type), mean, drop = FALSE)[2])
  sh_p = cbind(as.data.frame(sh_p),aggregate(abs(mydata$Penalty), list(mydata$Shade.Status), mean, drop = FALSE)[2],aggregate(abs(mydata$Diff), list(mydata$Shade.Status), mean, drop = FALSE)[2])
  al_p = cbind(as.data.frame(al_p),aggregate(abs(mydata$Penalty), list(mydata$Altitude.Category), mean, drop = FALSE)[2],aggregate(abs(mydata$Diff), list(mydata$Altitude.Category), mean, drop = FALSE)[2])
  se_p = cbind(as.data.frame(se_p),aggregate(abs(mydata$Penalty), list(mydata$Season), mean, drop = FALSE)[2],aggregate(abs(mydata$Diff), list(mydata$Season), mean, drop = FALSE)[2])
  all_p = cbind(as.data.frame(all_p),aggregate(abs(mydata$Penalty), list(mydata$concat), mean, na.action = na.omit, drop = FALSE)[2],aggregate(abs(mydata$Diff), list(mydata$concat), mean, na.omit = na.pass, drop = FALSE)[2])
  all_p[which(is.na(all_p))[1:max(which(which(is.na(all_p)) < 823))],] = 0
  
  colnames(lc_p) = columns
  colnames(sh_p) = columns
  colnames(al_p) = columns
  colnames(se_p) = columns
  colnames(all_p) = columns
  
  cum_lc_p = cum_lc_p + lc_p
  cum_sh_p = cum_sh_p + sh_p
  cum_al_p = cum_al_p + al_p
  cum_se_p = cum_se_p + se_p
  cum_all_p = cum_all_p + all_p
  
  if (x %% 5 == 0) { print(paste(x," out of 1000",sep='')) }
}

# Bootstrapped means
cum_lc_p = round((cum_lc_p / 1000),2) ; cum_lc_p = cbind(cum_lc_p,(cum_lc_p$`Orig. Error` - cum_lc_p$`Corrected Error`) / cum_lc_p$`Orig. Error`) ; cum_lc_p$Count = cum_lc_p$Count * 1000
cum_sh_p = round((cum_sh_p / 1000),2) ; cum_sh_p = cbind(cum_sh_p,(cum_sh_p$`Orig. Error` - cum_sh_p$`Corrected Error`) / cum_sh_p$`Orig. Error`) ; cum_sh_p$Count = cum_sh_p$Count * 1000
cum_al_p = round((cum_al_p / 1000),2) ; cum_al_p = cbind(cum_al_p,(cum_al_p$`Orig. Error` - cum_al_p$`Corrected Error`) / cum_al_p$`Orig. Error`) ; cum_al_p$Count = cum_al_p$Count * 1000
cum_se_p = round((cum_se_p / 1000),2) ; cum_se_p = cbind(cum_se_p,(cum_se_p$`Orig. Error` - cum_se_p$`Corrected Error`) / cum_se_p$`Orig. Error`) ; cum_se_p$Count = cum_se_p$Count * 1000
cum_all_p = round((cum_all_p / 1000),2) ; cum_all_p = cbind(cum_all_p,(cum_all_p$`Orig. Error` - cum_all_p$`Corrected Error`) / cum_all_p$`Orig. Error`) ; cum_all_p$Count = cum_all_p$Count * 1000

columns = c("Better Not Enough","Better Too Far","Overshot","Perfect","Wrong Direction","Count","Orig. Error","Corrected Error","% Change")

colnames(cum_lc_p) = columns
colnames(cum_sh_p) = columns
colnames(cum_al_p) = columns
colnames(cum_se_p) = columns
colnames(cum_all_p) = columns

everything = rbind(cum_lc_p,cum_sh_p,cum_al_p,cum_se_p,cum_all_p)
write.csv(everything,"C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__MASTER/_Bootstrapped.csv")

            
##################
# Step SEVEN: Brief lines of code to create GIFs (animation of raster layers)
                             
## Read the gifs exported from ArcGIS Pro
imgs <- c(list.files("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/gifs_input", full.names = TRUE))
img_list <- lapply(imgs, image_read)

## Join using ImageMagick library
img_joined <- image_join(img_list)

## Animate, one frame per second
img_animated <- image_animate(img_joined, fps = 1)

## Save
image_write(image = img_animated,
            path = "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/gifs_output/[insert GIF name here].gif") 
                             
### END ###
