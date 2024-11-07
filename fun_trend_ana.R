
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
addr_to_lalo <- function(addr = c("中央研究院"))
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
addr_map <- function(lalo=FALSE, lat=23.5, lon=121.0,  addr=c("台北101"), zoom = 14, rad = 0.01, year=2023, map_id=2, plot_ld=TRUE)
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
  
  if (lalo==FALSE) {
   #conver the address to latitude and longitude 
   cod <- addr_to_lalo(addr=addr)
  }else{
   #load the latitude and longitude 
    cod.wgs84 <- SpatialPoints( coords = cbind(lon, lat),proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') )
    cod.twd97 <- spTransform(cod.wgs84, sp::CRS("+init=epsg:3826"))
    cod <- data.frame(lon = lon, lat = lat, x=cod.twd97@coords[1], y= cod.twd97@coords[2] )
  }
  map_types=c("roadmap", "satellite", "hybrid", "terrain")
  used_map = map_types[map_id]
  
#圓框線
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
    rad_urb = round(value_counts[3,3],digits=1)+round(value_counts[6,3],digits=1)
    rad_wat = round(value_counts[4,3],digits=1)
    rad_agr = round(value_counts[5,3],digits=1)
    rad_gra = round(value_counts[7,3],digits=1)
    print(paste("森林:", rad_for,"%","建物:", rad_urb,"%", "農田:", rad_agr,"%","綠地:", rad_gra,"%","水體:", rad_wat,"%",sep=" "))
    }else{
    colors <- c("1"="#439c6e","2"="#e86d5f","3"="#8bd2e8","4"="#f0d86e","5"="#999999","6"="#99ad50")
    # Print the result
    print(value_counts)
    rad_for = round(value_counts[1,3],digits=1)
    rad_urb = round(value_counts[2,3],digits=1)+round(value_counts[5,3],digits=1)
    rad_wat = round(value_counts[3,3],digits=1)
    rad_agr = round(value_counts[4,3],digits=1)
    rad_gra = round(value_counts[6,3],digits=1)
    print(paste("森林:", rad_for,"%","建物:", rad_urb,"%", "農田:", rad_agr,"%","綠地:", rad_gra,"%","水體:", rad_wat,"%",sep=" "))
  }
 
# get map from google
 if (plot_ld == T) {
  map <- get_map(location = c(lon = cod$lon, lat = cod$lat), zoom = zoom, source = "google", maptype = used_map)
  #map_proj <- st_crs(map)  
  base_map <- ggmap(map)
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
  
  # plot the result
   print(map_with_circle)

} #end if

  # return a list of share of land 
   return(list(year=year,forest=rad_for[[1]],agri=rad_agr[[1]], grass=rad_gra[[1]], water=rad_wat[[1]], urban=rad_urb[[1]], radius=rad)) 
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
#address <- "100號, 永勝街, 勝安村, 吉安鄉, 花蓮縣, 台灣"
#

# 顯示該地點附近的地圖
#addr_map()
#lalo_map()





#load library 
library("sp")
library("raster")
library("rgdal") #readOGR
library("rgeos") #gCentroid
library("proj4")
#
#
mesh.500m = readOGR(verbose = FALSE, 
            "/lfs/home/ychen/lfs_dir/GIS/Taiwan_Fishnet/500m/taiwan_raster_t97.shp")
#mesh.500m = readOGR(verbose = FALSE, 
#            "/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/fishnet/mesh/500m/taiwan_raster_WGS84.shp")
#convert the projection into twd97
mesh.500m =  spTransform(mesh.500m, sp::CRS("+init=epsg:4326"))  

# get gelocation of center point  of the grid.
cent.xy.500m = gCentroid(mesh.500m, byid=TRUE)
# create dataframe for xy coordinate
df.500m <- as.data.frame(cent.xy.500m)
# Using add_column()
df.500m.trend <- data.frame(id=mesh.500m$ID ,lon=df.500m$x, lat=df.500m$y, urban_trend=0., agr_trend=0., grass_trend=0.,forest_trend=0.  )

#subset to AOI
#ymax=25.01
#ymin=25.00

# seting aoi area of interest (AOI)
#
#xycen=data.frame(lat=24.927688, lon=121.1840095) 
#xydel=0.015
#addr=c("test")

#addr="高鐵桃園站"
#addr="高鐵台中站"
#addr="高鐵左營站"
#ddr="宜蘭中央公園"
#addr="高鐵彰化站"

#xycen <- addr_to_lalo(addr=addr)  
#xydel=     0.1500 

#North Taiwan
#xycen = data.frame(lon=121.411, lat=24.646) ; addr=c("北台灣") # 中心點約在新竹 尖石  
#xycen = data.frame(lon=120.985, lat=23.969) ; addr=c("中臺灣") # 中心點約在南投 埔里
xycen = data.frame(lon=120.772, lat=23.265) ; addr=c("南臺灣") # 中心點約在高雄 那瑪夏 
#setting  1.0 = 100.0 km 
xydel=0.8
xmax=xycen$lon + xydel
xmin=xycen$lon - xydel 

ymax=xycen$lat + xydel
ymin=xycen$lat - xydel



df.500m.aoi <- df.500m.trend[((df.500m.trend$lon > xmin & df.500m.trend$lon < xmax) & (df.500m.trend$lat > ymin & df.500m.trend$lat <ymax)), ]
print(paste("Total Grids(@500m) within the AOI:", length(df.500m.aoi$lon), "Center:", xycen$lon, ",", xycen$lat ,sep=""))
#open devic for regression plot 
#dev.new()

for (idf in 1:length(df.500m.aoi$id)) {
#for (idf in 2380:length(df.500m.aoi$id)) {


#for (idf in 1:1) {
#loop for year for check the dynamics of land use change
yrs <- c(2015,2016,2017,2019,2020,2021,2022,2023)
#yrs <- c(2013,2023)
# year loop 
  # Initialize an empty data.frame
lu_data <- data.frame(year = numeric(), forest = numeric(), agri_grass = numeric(), water = numeric(), urban = numeric(), radius = numeric() ) 
#
  for (iyr in yrs)
  {
  plot_ld = FALSE
  if (iyr == 2015) plot_ld = FALSE # only plot year 2015
  #addr_value <- addr_map(lalo=TRUE, lat=25.012876, lon=121.2144021, year=iyr, addr="桃園高鐵站", plot_ld=TRUE, rad=0.01)
  addr_value <- addr_map(lalo=TRUE, lat=df.500m.aoi$lat[idf] , lon=df.500m.aoi$lon[idf] , year=iyr, addr=df.500m.aoi$id[idf] , plot_ld=plot_ld, rad=0.0075)
  #combine the results 
  lu_data <- rbind(lu_data, as.data.frame(addr_value, stringsAsFactors = FALSE))
   }

y1=lu_data$urban
y2=lu_data$agri
y3=lu_data$grass
y4=lu_data$forest
x0=lu_data$year

# Fit the linear model
x=x0
y=y1
if (is.na(y[1])!=TRUE) { 
model <- lm(y ~ x)
# plot(x, y, main = "Scatter Plot with Regression Line", xlab = "X", ylab = "Y", pch = 19, col = "blue")
# abline(model, col = "red", lwd = 2)
coefficients <- coef(model)
cat("Urban:","\n")
cat("Intercept:", coefficients[1], "\n")
cat("Slope:", coefficients[2], "\n")
# update the slope information in the table
df.500m.aoi$urban_trend[idf] <- coefficients[2]
}else{
df.500m.aoi$urban_trend[idf] <- 0 
}

# Fit the linear model
x=x0
y=y2
if (is.na(y[1])!=TRUE) { 
model <- lm(y ~ x)
# plot(x, y, main = "Scatter Plot with Regression Line", xlab = "X", ylab = "Y", pch = 19, col = "blue")
# abline(model, col = "red", lwd = 2)
coefficients <- coef(model)
cat("Agri:","\n")
cat("Intercept:", coefficients[1], "\n")
cat("Slope:", coefficients[2], "\n")
# update the slope information in the table
df.500m.aoi$agri_trend[idf] <- coefficients[2]
}else{
df.500m.aoi$agri_trend[idf] <- 0 
}

# Fit the linear model
x=x0
y=y3
if (is.na(y[1])!=TRUE) { 
model <- lm(y ~ x)
# plot(x, y, main = "Scatter Plot with Regression Line", xlab = "X", ylab = "Y", pch = 19, col = "blue")
# abline(model, col = "red", lwd = 2)
coefficients <- coef(model)
cat("Grass:","\n")
cat("Intercept:", coefficients[1], "\n")
cat("Slope:", coefficients[2], "\n")
# update the slope information in the table
df.500m.aoi$grass_trend[idf] <- coefficients[2]
}else{
df.500m.aoi$grass_trend[idf] <- 0 
}

# Fit the linear model
x=x0
y=y4
if (is.na(y[1])!=TRUE) { 
model <- lm(y ~ x)
# plot(x, y, main = "Scatter Plot with Regression Line", xlab = "X", ylab = "Y", pch = 19, col = "blue")
# abline(model, col = "red", lwd = 2)
coefficients <- coef(model)
cat("Forest:","\n")
cat("Intercept:", coefficients[1], "\n")
cat("Slope:", coefficients[2], "\n")
# update the slope information in the table
df.500m.aoi$forest_trend[idf] <- coefficients[2]
}else{
df.500m.aoi$forest_trend[idf] <- 0 
}


#write out the dataframw  
write.table(x=df.500m.aoi,file=paste(addr,"_",xydel,"_df_out.txt",sep=""))

print(paste("Working on grid:",idf,"/Totoal:",length(df.500m.aoi$lon),sep=""))


gc()
}

#aa <- TRUE
#while (aa==TRUE) {
#detach("package:jsonlite")
#detach("package:rgeos")
#detach("package:rgdal")
#detach("package:sf")
#detach("package:proj4")
#detach("package:stars")

#create a raster map 
# Load libraries
library(raster)
library(sp)
#library(terra)
# Create data frame
##df <- data.frame(
#  lon = c(100.5, 101.0, 101.5, 101.6),
#  lat = c(13.5, 13.6, 13.7,13.8),
#  value = c(5, 10, 15, 20)
#)
df <- df.500m.aoi
# Convert to spatial points
coordinates(df) <- ~lon+lat
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
# Create empty raster with specified extent and resolution 500m
r <- raster(extent(df), resolution = 0.005)
proj4string(r) <- CRS("+proj=longlat +datum=WGS84")
# Rasterize the data
r <- rasterize(df, r, field = "urban_trend", fun = mean, na.rm = TRUE)
writeRaster(r, filename=paste(addr,"_",xydel,"urban_trend.tif",sep=""), format="GTiff", overwrite=TRUE)
# Plot
plot(r, main = "Raster Map from Data Frame Points")

#} #end of while
