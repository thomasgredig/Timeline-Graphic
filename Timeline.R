##################################
# Resources: 
# http://benalexkeen.com/creating-a-timellibrary(ggplot2)
#
# Building a timeline using ggplot
##################################

#devtools::install_github("thomasgredig/checkRAWfolder")
library(checkRAWfolder)
source('config.R')
library(ggplot2)
library(scales)
library(lubridate)

# loading data
raw.checkNoSubfolders(path.RAW)
user.name = raw.getUsername(path.RAW)
n = raw.getTable(path.RAW)
n = na.omit(n)


n$month = as.numeric(substr(n$Date.formatted,6,7))
n$year = as.numeric(substr(n$Date.formatted,1,4))
plot(n$year)
plot(n$month)
str(n)

n <- n[with(n, order(year, month, Instrument)), ]
n$yearmonth = n$year*100+n$month
n$month_count = ave(n$yearmonth==n$yearmonth, n$yearmonth, FUN=cumsum)
n$instr = as.integer(n$Instrument)*0.2

# arrange data for timeline
names(n)

n$position = round(runif(nrow(n), -4, 4))
n$direction = rep(c(-1,1), length.out=nrow(n))
head(n)
plot(n$position)

# making months labels
month_buffer <- 2
month_date_range <- seq(min(n$Date.formatted) - months(month_buffer), max(n$Date.formatted) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)
month_df$Date.formatted = ''
head(month_df)

# making year labels
year_date_range <- seq(min(n$Date.formatted) - months(month_buffer), max(n$Date.formatted) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)



status_levels <- levels(n$Instrument) #c("Complete", "On Target", "At Risk", "Critical")
status_colors <- c("#0070C0", "#00B050", "#C00000", "#FFCC00")[1:length(status_levels)]

ggplot(n,aes(x=Date.formatted,y=0, col=Instrument)) + 
  labs(col="Instrument:") +
  scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
  theme_classic(base_size=12) +
  geom_hline(yintercept=0,  color = "black", size=0.3)+
  #geom_segment(data=month_df, aes(xend=month_date_range, yend=0, y=0.2),color='black', size=0.2) + 
  geom_segment(data=n[n$month_count == 1,], aes(y=-0.05,yend=0,xend=Date.formatted), color='black', size=0.8) +
  geom_point(aes(y=0), col='black', alpha=0.3, size=4) +
  geom_point(aes(y=instr), alpha=0.8, size=3) +
  theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom") +
  geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=4,vjust=0.5, color='blue', angle=90) +
  geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=4, color='blue') + 
  ggtitle(paste('Timeline: ',user.name))
ggsave(file.path(path.FIGS, paste0(user.name, '-Timeline.png')),width=12,height=6, dpi=300)




