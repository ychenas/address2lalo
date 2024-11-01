
# 
# fun_ad2lalo 
# convert the address to latitude and longitude
# 

#In this first example we will geocode a few addresses using the geocode() function and plot them on a map with ggplot.

#library(dplyr, warn.conflicts = FALSE)
#library(tidygeocoder)

# create a dataframe with addresses
#some_addresses <- tibble::tribble(
#~name,                  ~addr,
#"White House",          "1600 Pennsylvania Ave NW, Washington, DC",
#"Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
#"Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606",
#"My Home",              "128 Academia Road, Section 2, Nankang, Taipei 115201"                                  
#)

# geocode the addresses
#lat_longs <- some_addresses %>%
#  geocode(addr, method = 'osm', lat = latitude , long = longitude)
#  geocode(addr, method = 'osm', lat = latitude , long = longitude)
#

#> Passing 3 addresses to the Nominatim single address geocoder
#> Query completed in: 3 seconds
#print(lat_longs)
# method can use arcgis, osm, bing, google 

#> Passing 3 addresses to the Nominatim single address geocoder
#> Query completed in: 3 seconds


#library(ggmap)
#library(dplyr)


# 地址轉換函數
#address_to_latlon <- function(address) {
#  result <- geocode(address, output = "latlon", source = "google", language = "zh-TW")
#  return(result)
#}

# 測試範例地址
#address = "台北市南港區研究院路二段128號"

#ddress <- "Taipei 101, Taipei, Taiwan"
#coordinates <- address_to_latlon(address)

# 顯示結果

#install.packages("httr")
#install.packages("jsonlite")
#install.packages("ggmap")

# 載入套件
library(httr)
library(jsonlite)
library(ggmap)
# 設定 Google Maps API 金鑰
api_key <- read.csv("api.key.txt",header=FALSE)$V1

#地址轉換函數
address_to_latlon <- function(address) {


  # 將地址進行 URL 編碼
  encoded_address <- URLencode(address)
  
  # 建構 API 請求 URL，包含中文語言參數
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", 
                encoded_address,"&key=", api_key, "&language=zh-TW")
  
# 發送 GET 請求
  response <- GET(url)
  
  # 檢查回應狀態
  if (status_code(response) != 200) {
    stop("API 請求失敗")
  }
  
  # 解析 JSON 回傳資料
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data)


  print(json_data)
  # 提取經緯度
  if (json_data$status == "OK") {
    lat <- json_data$results[[3]]$location$lat
    lng <- json_data$results[[3]]$location$lng
    print(paste("Coordinate:",lat,",",lng,sep=""))

    return(data.frame(lat = lat, lng = lng))
  } else {
    stop("地址無法解析")
  }


}

# 測試範例地址 (中文)
#address <- "台北市, 南港區, 研究院路二段, 128號, 台灣"
address <- "100號, 永勝街, 勝安村, 吉安鄉, 花蓮縣, 台灣"
#
#address <- "101, Taipei, Taiwan"

cod <- address_to_latlon(address=address)

library(sp)
#create a sp object with la lo and ref coordinate stsyetm)
cod.wgs84 <- SpatialPoints( coords = cbind(cod$lng, cod$lat),proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') )   

cod.twd97 <- spTransform(cod.wgs84, sp::CRS("+init=epsg:3826"))

# 顯示結果
print(paste("Address:",address," Coordinates(WGS84)/", "Lng:",cod.wgs84@coords[1], "Lat:", cod.wgs84@coords[2], sep=" "))
# 顯示結果
print(paste("Address:",address," Coordinates(TWD97)/", "X:",cod.twd97@coords[1], "Y:", cod.twd97@coords[2], sep=" "))


lon=cod$lng
lat=cod$lat

# 顯示地圖的函數
#show_map <- function(lat, lon, radius_km = 2) {
  # 取得中心位置的地圖
 map <- get_map(location = c(lon = lon, lat = lat), zoom = 15, source = "google", maptype = "satellite")
  
  # 繪製地圖並加上查詢地點的標記
  ggmap(map) +
    geom_point(aes(x = lng, y = lat), data=cod, color = "red", size = 3) +
    labs(title = paste("查詢地址地圖","TWD97座標(",round(x=cod.twd97@coords[1],digits=1),",", round(x=cod.twd97@coords[2],digits=1),")",sep=""),
         x = "經度",
         y = "緯度")
#}

# 顯示該地點附近 2 公里的地圖
#show_map(cod$lat, cod$lng, radius_km = 2)
