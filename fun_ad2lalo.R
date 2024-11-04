
# 
# fun_ad2lalo 
# convert the address to latitude and longitude
# 

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
  api_key <- read.csv("./api.key.txt",header=FALSE)$V1

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
addr_map <- function(addr=c("台北101"), zoom = 14, rad = 0.01, year=2023)
 {
# INIT
#  addr=c("台北101")
#  zoom=14
#  rad=0.01
# 設定 Google Maps API 金鑰
  api_key <- read.csv("./api.key.txt",header=FALSE)$V1
  ggmap::register_google(key=api_key)
  #利用Google Sataic Map API 功能 進行地址傳換機緯度的變更
  # 取得中心位置的地圖
  cod <- addr_to_lalo(addr=addr)
  # get map from google
  map <- get_map(location = c(lon = cod$lon, lat = cod$lat), zoom = zoom, source = "google", maptype = "satellite")
  #map_proj <- st_crs(map)
  
  base_map <- ggmap(map)
  
  # 加圓框線
  # Convert the location to an sf object
  center_point <- st_point(c(cod$lon, cod$lat)) %>%
  st_sfc(crs = 3857)
  #st_transform(crs = st_crs(3857))  # Transform to match the map's projection
  # Define the radius of the circle (in meters)
  radius <- rad # 1000 m
  # Create a circular buffer around the center point
  circle <- st_buffer(center_point, dist = radius)
  circle_sf <- st_transform(circle, crs = 3857) 

  #讀取GEOTIFF 檔案並傳換座標
  tiff_file <- read_stars(paste("./lu_maps/",year,"_all_grid_wgs84.tif",sep=""))
  # tiff_file <- read_stars("./lu_maps/2023_236_210_tree20_wgs84.tif")
  st_crs(tiff_file) <- st_crs(3857)  #TWD97/WGS80
  #coordinate transform 
  #st_warp(src=tiff_file,dest= tiff_trans, crs=st_crs('OGC:CRS84') ) #WGS84
  # 將 TIFF 投影轉換以符合遮罩
  # Transform to the new CRS
  #tiff_trans <- rast(tiff_trans)

  # 使用圓形遮罩裁剪 TIFF 檔案
  tiff_cropped <-st_crop(tiff_file, circle_sf) 
 
  #計算比例
  library(dplyr)
  ## Convert the raster values to a data frame to analyze value frequencies
  raster_values <- as.data.frame(tiff_cropped, as_points = FALSE)
  # Rename the value column for easier access
  names(raster_values)[3] <- "value"
  # Remove NA values
  raster_values <- raster_values %>% filter(!is.na(value)) 
  # Calculate the count and percentage of each unique value
  value_counts <- raster_values %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::mutate(percentage = (count / sum(count)) * 100)

  # 給定調色盤
 if (value_counts[1]$value[1] == 0) {
    colors <- c("0"="lightgray","1"="#439c6e","2"="#e86d5f","3"="#8bd2e8","4"="#f0d86e","5"="#999999","6"="#99ad50")
    print(value_counts)
    rad_for = round(value_counts[2,3],digits=1)
    rad_urb = round(value_counts[3,3],digits=1)
    rad_wat = round(value_counts[4,3],digits=1)
    rad_agr = round(value_counts[5,3],digits=1)
    rad_gra = round((value_counts[6,3] + value_counts[7,3]),digits=1)
    print(paste("森林:", rad_for,"%","建物:", rad_urb,"%", "農田:", rad_agr,"%","綠地:", rad_gra,"%","水體:", rad_wat,"%",sep=" "))
    }else{
    colors <- c("1"="#439c6e","2"="#e86d5f","3"="#8bd2e8","4"="#f0d86e","5"="#999999","6"="#99ad50")
    # Print the result
    print(value_counts)
    rad_for = round(value_counts[1,3],digits=1)
    rad_urb = round(value_counts[2,3],digits=1)
    rad_wat = round(value_counts[3,3],digits=1)
    rad_agr = round(value_counts[4,3],digits=1)
    rad_gra = round((value_counts[5,3] + value_counts[6,3]),digits=1)
    print(paste("森林:", rad_for,"%","建物:", rad_urb,"%", "農田:", rad_agr,"%","綠地:", rad_gra,"%","水體:", rad_wat,"%",sep=" "))
    
    
  }
  # colors <- c("1"="#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999","#99ad50","#383838") 
  # 繪製地圖並加上查詢地點的標記
  # Plot the circle on the map
  map_with_circle <- base_map +
    geom_stars(data = tiff_cropped, alpha = 0.7) +
    geom_sf(data = circle_sf, fill = NA, color = "gray",  size=1, linewidth=1,  inherit.aes = FALSE) + #顯示圓邊界 
   #scale_fill_viridis_c(option = "plasma") +
   # scale_fill_manual(values=value_colors) +
   # scale_fill_manual(values = cols, aesthetics = c("colour", "fill")) +
    scale_fill_gradientn(colours = colors[] ) +
    geom_point(aes(x = lon, y = lat), data=cod, color = "yellow", size=1, shape=21, stroke=2) +
     labs(title =  paste("森林(F):", rad_for,"%", ", 農田/綠地(A/G):", rad_agr+rad_gra,"%",", 水體(W):", rad_wat,"%","裸土/建物(B/B):", rad_urb,"%",sep=""),
          caption = paste("查詢地址:",addr,",圖資年份:",year,", TWD97-參考座標(",round(x=cod$x,digits=1),", ", round(x=cod$y,digits=1),")",sep=""),
          x = "經度(WGS84)",  y = "緯度(WGS84)") +
    theme(plot.caption = element_text(size = 10), legend.position ="none") +# Adjust size here & 移除legend 
    coord_sf(datum = st_crs(circle_sf))
  
  #plot the result
  print(map_with_circle)

 }#addr_map

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

