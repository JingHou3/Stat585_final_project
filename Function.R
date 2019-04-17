library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(reshape2)
library(stringr)
#这里目前一共有5个function，其中有2个是读数据，另外3个是画图。



#Function1:read the data of one location
#id 是机场代码，type是天气要素
#此函数可以读取一个机场的若干天气要素
current_weather <- function(id, type){
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(stringr::str_length(id) == 4)
  a <- xml2::read_xml(paste("https://w1.weather.gov/xml/current_obs/", id, ".xml", sep = ""))
  b <- xml2::xml_children(a)
  name <- xml2::xml_name(b)
  text <- xml2::xml_text(b)
  n <- length(name)
  m <- length(type)
  data <- rep(NA, m)
  mark <- rep(0, m)
  if (m==0){
    for (i in 1:n){
      if(name[i] == "location")
        location <- text[i]
      else if(name[i] == "station_id")
        station_id <- id
      else if(name[i] == "latitude")
        latitude <- as.numeric(text[i])
      else if(name[i] == "longitude")
        longitude  <- as.numeric(text[i])
      else if(name[i] == "observation_time")
        observation_time <- text[i]
      else if(name[i] == "weather"){
        weather <- text[i]
        mark <- 1
      }
      
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time,
                      weather)
  }
  if (m == 1){
    for (i in 1:n){
      if(name[i] == "location")
        location <- text[i]
      else if(name[i] == "station_id")
        station_id <- id
      else if(name[i] == "latitude")
        latitude <- as.numeric(text[i])
      else if(name[i] == "longitude")
        longitude  <- as.numeric(text[i])
      else if(name[i] == "observation_time")
        observation_time <- text[i]
      else if(name[i] == type){
        data <- text[i]
        mark <- 1
      }
      
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time,
                      data)
    colnames(obs)[6] <- type 
  }
  if (m > 1){
    for (i in 1:n){
      if(name[i] == "location")
        location <- text[i]
      else if(name[i] == "station_id")
        station_id <- id
      else if(name[i] == "latitude")
        latitude <- as.numeric(text[i])
      else if(name[i] == "longitude")
        longitude  <- as.numeric(text[i])
      else if(name[i] == "observation_time")
        observation_time <- text[i]
      else {
        for (k in 1:m){
          if(name[i] == type[k]){
            data[k] <- text[i]
            mark[k] <- 1
          }
        }
      }
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time)  
    for (i in 1:m)
      obs <- cbind(obs, data[i])
    names(obs)[6:(6+m-1)] <- type
  }
  if(sum(mark) < m)
    print("Some of your types are not found! They are shown as NA!")
  assertthat::assert_that(is.data.frame(obs))
  obs
}

#Function2:read the data of several different locations
#读取多个机场的多个天气要素
current_weather_more <- function(id_vector, type){
  n <- length(id_vector)
  obs <- NULL
  for (i in 1:n){
    obs <- rbind(obs, current_weather(id_vector[i], type))
  }
  assertthat::assert_that(is.data.frame(obs))
  obs
}
#example for reading data
current_weather("KAMW", c("wind_mph", "temp_f", "haha"))
current_weather("KAMW", "temp_c")
current_weather_more(c("KAMW", "KAIO", "KCID", "KCNC"), c("temp_f"))



#Function3: plot an element
#label 是否标出机场code
#number 此天气要素是否是numeric
#此函数可以画出多个机场的同一个天气要素
plot_weather <- function(id_vector, type, label = T, number = F){
  data <- current_weather_more(id_vector, type)
  colnames(data)[6]<-"element"
  if (number == T){
    data$element <- as.numeric(as.character(data$element))
  }
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = element)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 1.5,
                vjust = -1)+
      theme_bw() +
      labs(title = type, 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = element)) +
      theme_bw() +
      labs(title = type, 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
}

###plot for shiny app
plot_map <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, type = NULL)
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude),color="red") +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                vjust=-1,
                size = 3)+
      theme_bw() 
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude),color="red") +
      theme_bw() 
  }
}


#Function4: plot an numeric element of all US airport in contour lines
#plot = "point": point plot; you can change it to "contour" if number = true
#you.long: your longitude
#you.lat: your latitude
#bin: the binwidth of the contour lines
#plot one weather elements of all US airport 
plot_weather_us <- function(type, you.long, you.lat, bin){
  id <- c("KBHM", "KDHN", "KHSV", "KMOB", "KMGM", "KIFP", "KFLG", "KGCN", "KIWA",
          "KPGA", "KPHX", "KTUS", "KNYL", "KXNA", "KFSM", "KLIT", "KTXK", "KACV",
          "KBFL", "KBUR", "KFAT", "KLGB", "KLAX", "KMMH", "KMRY", "KOAK", "KONT",
          "KPSP", "KRDD", "KSMF", "KSAN", "KSFO", "KSJC", "KSBP", "KSNA", "KSBA",
          "KSMX", "KSTS", "KSCK", "KASE", "KCOS", "KDEN", "KDRO", "KEGE", "KGJT",
          "KGUC", "KHDN", "KMTJ", "KBDL", "KHVN", "KILG", "KDAB", "KFLL", "KRSW",
          "KGNV", "KJAX", "KEYW", "KMLB", "KMIA", "KMCO", "KSFB", "KECP", "KPNS",
          "KPGD", "KSRQ", "KSGJ", "KPIE", "KTLH", "KTPA", "KVPS", "KPBI", "KABY",
          "KATL", "KAGS", "KBQK", "KCSG", "KSAV", "KVLD", "KBOI", "KSUN", "KIDA", 
          "KLWS", "KPIH",
          "KTWF", "KBLV", "KBMI", "KCMI", "KORD", "KMDW", "KMWA", "KMLI", "KPIA",
          "KUIN", "KRFD", "KSPI", "KEVV", "KFWA", "KIND", "KSBN", "KCID", "KDSM",
          "KDBQ", "KSUX", "KALO", "KGCK", "KMHK", "KFOE", "KICT", "KCVG", "KLEX",
          "KSDF", "KOWB", "KPAH", "KAEX", "KBTR", "KLFT", "KLCH", "KMLU", "KMSY",
          "KSHV", "KBGR", "KPWM", "KPQI", "KRKD", "KBWI", "KHGR", "KSBY", "KBOS",
          "KHYA", "KACK", "KPVC", "KMVY", "KORH", "KAPN", "KDTW", "KESC", "KFNT",
          "KGRR", "KCMX", "KIMT", "KAZO", "KLAN", "KSAW", "KMKG", "KPLN", "KMBS",
          "KCIU", "KTVC", "KBJI", "KBRD", "KDLH", "KHIB", "KINL", "KMSP", "KRST",
          "KSTC", "KGTR", "KGPT", "KJAN", "KCOU", "KJLN", "KMCI", "KSGF", "KSTL",
          "KBIL", "KBZN", "KBTM", "KGTF", "KHLN", "KGPI", "KMSO", "KSDY", "KGRI",
          "KLNK", "KOMA", "KBVU", "KEKO", "KLAS", "KVGT", "KRNO", "KLEB", "KMHT",
          "KPSM", "KACY", "KTTN", "KEWR", "KABQ", "KHOB", "KROW", "KSAF", "KALB",
          "KBGM", "KBUF", "KELM", "KFRG", "KISP", "KITH", "KJFK", "KLGA", "KSWF",
          "KIAG", "KPBG", "KROC", "KSYR", "KART", "KHPN", "KAVL", "KCLT", "KJQF",
          "KFAY", "KGSO", "KPGV", "KOAJ", "KEWN", "KRDU", "KILM", "KBIS", "KDIK",
          "KFAR", "KGFK", "KMOT", "KISN", "KCAK", "KLUK", "KCLE", "KCMH", "KLCK",
          "KDAY", "KTOL", "KYNG", "KLAW", "KOKC", "KTUL", "KEUG", "KMFR", "KOTH",
          "KPDX", "KRDM", "KABE", "KERI", "KMDT", "KLBE", "KPHL", "KPIT", "KUNV",
          "KAVP", "KIPT", "KBID", "KPVD", "KWST", "KCHS", "KCAE", "KFLO", "KGSP",
          "KHXD", "KMYR", "KABR", "KRAP", "KFSD", "KTRI", "KCHA", "KTYS", "KMEM",
          "KBNA", "KABI", "KAMA", "KAUS", "KBPT", "KBRO", "KCLL", "KCRP", "KDAL",
          "KDFW", "KELP", "KGRK", "KHRL", "KIAH", "KHOU", "KLRD", "KGGG", "KLBB",
          "KMFE", "KMAF", "KSJT", "KSAT", "KTYR", "KACT", "KSPS", "KCDC", "KOGD",
          "KPVU", "KSLC", "KSGU", "KBTV", "KCHO", "KLYH", "KPHF", "KORF", "KRIC",
          "KROA", "KDCA", "KIAD", "KBLI", "KFHR", "KPSC", "KPUW", "KBFI", "KSEA",
          "KGEG", "KALW", "KEAT", "KYKM", "KCRW", "KCKB", "KHTS", "KMGW", "KATW",
          "KEAU", "KGRB", "KLSE", "KMSN", "KMKE", "KCWA", "KRHI", "KCPR", "KCOD",
          "KGCC", "KJAC", "KLAR", "KRKS")
  data <- current_weather_more(id, type)
  data <- na.omit(data)
  colnames(data)[6]<-"element"
  data$element <- as.numeric(as.character(data$element))
  states <- map_data("state")
  m <- loess(element~longitude*latitude, data = data)
  xgrid <- seq(min(data$longitude), max(data$longitude), 0.01)
  ygrid <- seq(min(data$latitude), max(data$latitude), 0.01)
  data.fit <- expand.grid(longitude = xgrid, latitude = ygrid)
  result <- predict(m, newdata = data.fit)
  mtrx.melt <- melt(result, id.vars=c('longitude','latitude'),measure.vars = 'element')
  colnames(mtrx.melt)[3]<-"element"
  mtrx.melt$longitude <- as.numeric(str_sub(mtrx.melt$longitude, str_locate(mtrx.melt$longitude, '=')[1,1] + 1))
  mtrx.melt$latitude <- as.numeric(str_sub(mtrx.melt$latitude, str_locate(mtrx.melt$latitude, '=')[1,1] + 1))
  ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, group = group),
                 color = "grey", alpha = 0.6) + 
    geom_contour(data = mtrx.melt, binwidth = bin,
                 aes(x = longitude, y = latitude, z = element,colour = ..level..)) +
    geom_point(aes(x= you.long, y=you.lat), colour="red") +
    geom_text(aes(x = you.long - 0.5, y = you.lat - 0.5, label = "You"),size = 2) +
    theme_bw() +
    labs(title = type, 
         subtitle = as.character(data$observation_time[1]))+
    coord_fixed(1.3) +
    guides(fill = FALSE)
}



#Function5: plot an element of selected airport and your position
#you.long: your longitude
#you.long: your latitude
#number = true: this element is numeric
#plot one weather elements of several airport 
plot_position <- function(id_vector, type, you.long, you.lat, number = T){
  data <- current_weather_more(id_vector, type)
  data <- na.omit(data)
  colnames(data)[6]<-"element"
  if (number == T){
    data$element <- as.numeric(as.character(data$element))
  }
  states <- map_data("state")
  ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, group = group),
                 color = "grey", alpha = 0.6) + 
    geom_point(data = data,aes(x = longitude, y = latitude,
                               color = element)) +
    geom_point(aes(x= you.long, y=you.lat), colour="red") +
    geom_text(data = data, 
              aes(x = longitude, y = latitude, label = station_id),
              size = 1.5)+
    geom_text(aes(x = you.long - 0.5, y = you.lat - 0.5, label = "You"),
              size = 2) +
    theme_bw() +
    labs(title = type, 
         subtitle = as.character(data$observation_time[1]))+
    coord_fixed(1.3) +
    guides(fill = FALSE)
}



#example for plots
plot_weather(c("KAMW", "KAIO", "KCID", "KCNC"), 
             type = "weather", 
             label = T, number = F)

plot_weather_us(type = "temp_c",
                you.long = -100, you.lat = 35,
                bin = 0.5)

plot_position(c("KAMW", "KAIO"), 
             type = "temp_f", you.long = -100, you.lat = 35,
             number = T)
    
