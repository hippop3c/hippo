library(ggplot2)#載入套件
library(dplyr)
library(grid)
library(gridExtra)
library(magrittr)
#先從http://seanlahman.com/baseball-archive/statistics下載資料
teamStat<-read.csv("C:/Users/hippo/Desktop/CSV/球隊戰績.csv",header = TRUE,sep=",", stringsAsFactors = FALSE)
teamStatHit<-teamStat[,c(2,3,5,6,8,9,10,17,18,19,20,21)]

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
cols <- ggplotColours(2)

combns_lg_div <- distinct(teamStat, lgID, divID) %>% as.matrix#將teamStat中的lgID與dvID取出並轉成矩陣
graphs <- apply(combns_lg_div, 1, function(v){
  cols_fill <- ifelse(v[1] == "AL", cols[1], cols[2])
  teamStat %>% filter(lgID == v[1], divID == v[2]) %>%
    ggplot(aes(x = teamID, y = W)) +
    geom_bar(position="stack",stat="identity", fill = cols_fill)+
    geom_text(aes(label= W), vjust = 1.5, colour = "black",
              position = position_dodge(0.9), size=5) + 
    labs(x = "隊伍", y = "勝場數")
})

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
getLegendGraph <- ggplot(teamStatHit,aes(x=teamID, y=W, fill=lgID)) +
  geom_bar(position="stack",stat="identity")

grid.arrange(graphs[[1]], graphs[[2]], graphs[[3]], get_legend(getLegendGraph),
             graphs[[4]], graphs[[5]], graphs[[6]], ncol = 4, nrow = 2, 
             layout_matrix = cbind(c(1, 5), c(2, 6), c(3, 7), 4), 
             widths = c(2.3, 2.3, 2.3, 0.8))

