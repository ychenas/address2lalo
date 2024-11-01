
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
  library(sp)
  library(sf) 
  library(stars)

#地址轉換函數
addr_to_lalo <- function(addr = c("中央研究院") )
{
 # 載入套件
  library(httr)
  library(jsonlite)
  library(sp)
  library(sf)
  library(stars)
 # 設定 Google Maps API 金鑰
  api_key <- read.csv("api.key.txt",header=FALSE)$V1

  # 將地址進行 URL 編碼
  encoded_address <- URLencode(addr)
  
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
    lon <- json_data$results[[3]]$location$lng
    print(paste("Coordinate:",lat,",",lon,sep=""))
    #Convert coordinate reference system    
    #create a sp object with la lo and ref coordinate stsyetm)
    cod.wgs84 <- SpatialPoints( coords = cbind(lon, lat),proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') )   
    cod.twd97 <- spTransform(cod.wgs84, sp::CRS("+init=epsg:3826"))
    # 顯示結果
    print(paste("Address:",addr," Coordinates(WGS84)/", "Lng:",cod.wgs84@coords[1], "Lat:", cod.wgs84@coords[2], sep=" "))
    # 顯示結果
    print(paste("Address:",addr," Coordinates(TWD97)/", "X:",cod.twd97@coords[1], "Y:", cod.twd97@coords[2], sep=" "))

    return(data.frame(lat = lat, lon = lon, x=cod.twd97@coords[1], y= cod.twd97@coords[2]))
  } else {
    stop("地址無法解析")
  }#end if  

}#end of addr2lalo

# 顯示地圖的函數 輸入台灣的地址
addr_map <- function(addr=c("台北101"), zoom = 15)
 {
 
  addr=c("台北101")
  zoom=15
  #利用Google Sataic Map API 功能 進行地址傳換機緯度的變更
  # 取得中心位置的地圖
  cod <- addr_to_lalo(addr=addr)
  # get map from google
  map <- get_map(location = c(lon = cod$lon, lat = cod$lat), zoom = zoom, source = "google", maptype = "satellite")
  map_proj <- st_crs(map)
  
  base_map <- ggmap(map)
  
  # 加圓框線
  # Convert the location to an sf object
  center_point <- st_point(c(cod$lon, cod$lat)) %>%
  st_sfc(crs = 3857)
  #st_transform(crs = st_crs(3857))  # Transform to match the map's projection
  # Define the radius of the circle (in meters)
  radius <- 0.005  # 500 m
  # Create a circular buffer around the center point
  circle <- st_buffer(center_point, dist = radius)
  circle_sf <- st_transform(circle, crs = 3857) 

  #讀取GEOTIFF 檔案並傳換座標
#  tiff_file <- read_stars("2023_236_210_tree20.tif")
#  st_crs(tiff_file) <- 3826 #TWD97/WGS80
  #coordinate transform 
 # tiff_trans <- st_transform(tiff_file, crs=3857) #WGS84
  # 將 TIFF 投影轉換以符合遮罩
  # Transform to the new CRS

  # 使用圓形遮罩裁剪 TIFF 檔案
 # tiff_cropped <-st_crop(tiff_trans, circle_sf) 
  
  # 繪製地圖並加上查詢地點的標記
  # Plot the circle on the map
  map_with_circle <- base_map +
   geom_sf(data = circle_sf, fill = NA, color = "red",  size=1, linewidth=2,  inherit.aes = FALSE) + #顯示圓邊界 
#   geom_stars(data = tiff_cropped, alpha = 0.6) +
#   scale_fill_viridis_c(option = "plasma") +
   geom_point(aes(x = lon, y = lat), data=cod, color = "red", size=1, shape=21, stroke=2) +
   labs(title = paste("查詢地址:",addr,", TWD97座標(",round(x=cod$x,digits=1),",", round(x=cod$y,digits=1),")",sep=""),
        x = "經度",
        y = "緯度")
  #plot the result
  print(map_with_circle)
 }#addr_map

# Load the raster package
#library(raster)

# Read or create a raster object
#raster_data <- raster("./2023_236_210_tree20.tif")

# Set the initial CRS if necessary (e.g., WGS84)
#crs(raster_data) <- CRS("+init=epsg:3826")

# Define the new CRS (e.g., UTM Zone 33N with EPSG:32633)
#new_crs <- CRS("+init=epsg:3857")

# Transform to the new CRS
#raster_transformed <- projectRaster(raster_data, crs = new_crs)


#plot(base_map) 
#plot(raster_transformed, add = T, legend = F, col = rev(rainbow(10, alpha = 0.35)))


# 顯示地圖的函數 輸入經緯度
lalo_map <- function(lat=c(23), lon=c(121), zoom = 15) 
   {
  # 取得中心位置的地圖
    map <- get_map(location = c(lon = lon, lat = lat), zoom = zoom, source = "google", maptype = "satellite")
    #WGS84 轉 TWD97(WGS80)
    library(sp)
    library(sf)
    #create a sp object with la lo and ref coordinate stsyetm)
    cod.wgs84 <- SpatialPoints( coords = cbind(lon, lat), proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') )   
    cod.twd97 <- spTransform(cod.wgs84, sp::CRS("+init=epsg:3826"))
  
    # 繪製地圖並加上查詢地點的標記
    ggmap(map) +
    geom_point(aes(x = lon, y = lat), data=data.frame(lon=lon,lat=lat), color = "red", size = 3) +
    labs(title = paste("WGS84座標",lon,",",lat,", TWD97座標(",round(x=cod.twd97@coords[1],digits=1),",", round(x=cod.twd97@coords[2],digits=1),")",sep=""),
         x = "經度",
         y = "緯度")
    
   }#end of lalo_map 


# 測試範例地址 (中文)
#address <- "台北市, 南港區, 研究院路二段, 128號, 台灣"
address <- "100號, 永勝街, 勝安村, 吉安鄉, 花蓮縣, 台灣"
#

# 顯示該地點附近的地圖
#addr_map()
#lalo_map()

