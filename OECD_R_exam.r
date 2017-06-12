
library(dplyr)
library(ggplot2)


oecd<-read.csv2("oecddata.csv", sep=";")

head(oecd)

graph1 <-  oecd %>%
            filter(Year==2014)%>%
            select(Country,Cost,LE)

graph1

ggplot(graph1, aes(x = Cost, y = LE)) +geom_point() + geom_smooth(method=lm)

Countries = c("Netherlands", "Latvia", "United States", "Iceland", "Greece")



graph2 <- oecd %>%
            select(Country,Year,Cost)%>%
            filter(Country == Countries,Year == 2005:2015)


graph2

ggplot(graph2, aes(x = Year, y = Cost, color = Country)) + geom_line()

#The figure doesn't look quite the same
#because they determined the Score : the lowest ratio / the ratio per country
                       #instead of : the ratio per country / the lowest ratio

eff <- graph1 %>%
        na.omit()%>%
        mutate(Ratio = Cost / LE) %>%
        mutate(Score = Ratio / min(Ratio))%>%
        arrange(Score)

eff

ggplot(eff, aes(x=reorder(Country,Score), y=Score, fill=Country)) + 
          geom_bar(stat='identity', fill="blue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        labs(x="Country",y="Efficiency scores according to Saskia")
