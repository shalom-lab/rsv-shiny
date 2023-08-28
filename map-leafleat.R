library(leaflet)
library(tidyverse)
library(rgdal)
library(rio)

cat('---map-leafleat.R---\n')
#suzhou<-rgdal::readOGR('shiny/suzhou.json')
#suzhou<-rgdal::readOGR('suzhou.json')
#saveRDS(suzhou,'shiny/suzhou.rds')
suzhou<-readRDS('suzhou.rds')
data.sch<-tribble(~long,~lat,~name,
                  120.630458, 31.305697,'Jingde Road Branch of SCH',
                  120.763533, 31.316233,'Industrial Park Branch of SCH')

hosIcon <- awesomeIcons(
  icon = 'home',
  iconColor = 'black',
  library = 'glyphicon',
  markerColor = 'red'
)

data.dis<-data.frame(district=c('Huqiu','Wuzhong','Xiangcheng','Gusu','Wujiang','Industrial Park',
                                'Changshu', 'Zhangjiagang','Kunshan','Taicang')) %>%
  mutate(area=ifelse(district %in% c('Huqiu','Wuzhong','Xiangcheng','Gusu','Industrial Park'),
                     'Downtown Suzhou','Suzhou Suburb')) %>%
  mutate(color=ifelse(area=='Downtown Suzhou','#f03b20','#2c7fb8'))

getLabels<-function(data){
  paste(sep='<br/>',
        sprintf('<span style="color:%s">%s</span>',data$color,data$area),
        data$district
  ) %>%
    lapply(function(x){
      HTML(x)
    })
}
getLabels(data.dis)

base <-leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.HOT,options = providerTileOptions(minZoom = 8)) %>%
  #setView(121.1650897553,lat =  31.2976024261,zoom = 9.3) %>%
  setMaxBounds(lng1 = 119,
               lat1 = 30.5,
               lng2 = 122,
               lat2 = 32.5)  %>%
  addPolygons(data = suzhou,stroke = T, weight = 2,color = data.dis$color,smoothFactor = 0.3, fillOpacity = 0.3,
              label = getLabels(data.dis),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", color = 'black'),
                textsize = "15px", direction = "auto"),
              highlightOptions = highlightOptions(color ="red", weight = 1,fillOpacity = 0.6,
                                                  bringToFront = TRUE)) %>%
  addAwesomeMarkers(data=data.sch,~long, ~lat,icon=hosIcon,
                    label = ~name,
                    labelOptions = labelOptions(direction = 'top')
                    #label=~name,labelOptions = labelOptions(noHide = T, textsize = "15px")
  )

