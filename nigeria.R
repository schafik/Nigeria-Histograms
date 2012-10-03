setwd("/Users/salahchafik/Documents/Modi Labs/Nigeria/Data Collection Status/survey data")
getwd()
library('ggplot2')
library('plyr')

health <- read.csv("Health_05_06_2012_2012_08_16.csv", header = TRUE)
education <- read.csv("Education_05_06_2012_2012_08_16.csv", header = TRUE)
localities <- read.csv("Localities_05_0hea6_2012_2012_08_16.csv", header = TRUE)
water <- read.csv("Water_05_06_2012_2012_08_16.csv", header = TRUE)
health2 <- read.csv("Health_17_04_2012_2012_08_21.csv", header = TRUE)
health3 <- read.csv("Health_22_05_2012_2012_08_21.csv", header = TRUE)
education2 <- read.csv("Education_17_04_2012_2012_08_21.csv", header = TRUE)
education3 <- read.csv("Education_22_05_2012_2012_08_21.csv", header = TRUE)
localities2 <- read.csv("Localities_16_04_2012_2012_08_21.csv", header = TRUE)
localities3 <- read.csv("Localities_22_05_2012_2012_08_21.csv", header = TRUE)
water2 <- read.csv("Water_22_05_2012_2012_08_21.csv", header = TRUE)
water3 <- read.csv("Water_24_04_2012_2012_08_21.csv", header = TRUE) 

h <- health[c('mylga', 'mylga_state', 'sector')]
e <- education[c('mylga', 'mylga_state', 'sector')]
l <- localities[c('mylga', 'mylga_state', 'sector')]
w <- water[c('mylga', 'mylga_state', 'sector')]
h2 <- health2[c('mylga', 'mylga_state', 'sector')]
h3 <- health3[c('mylga', 'mylga_state', 'sector')] 
e2 <- education2[c('mylga', 'mylga_state', 'sector')]
e3 <- education3[c('mylga', 'mylga_state', 'sector')]
l2 <- localities2[c('mylga', 'mylga_state', 'sector')]
l3 <- localities3[c('mylga', 'mylga_state', 'sector')]
w2 <- water2 [c('mylga', 'mylga_state', 'sector')]
w3 <- water3 [c('mylga', 'mylga_state', 'sector')]

lga <- rbind(h,e,l,w,h2,h3,e2,e3,l2,l3,w2,w3)

#####CLEANING #####

#cleaning states from erroneous values
cleanlga <- subset(lga, (mylga_state!='FALSE') & (mylga_state!='8.40512514') & (mylga_state!='ERROR') & (mylga_state!='fed_gov') & (mylga_state!='0') 
                   & (mylga_state!='grid_further_500_m') & (mylga_state!='yes') & (mylga_state!='TRUE') & (mylga_state!='11.74312798') & (mylga_state!='7.98938334')
                   & (mylga_state!='priv_profit') & (mylga_state!='1339841753707.jpg') & (mylga_state!='4') & (mylga_state!='Xxc') & (mylga_state!='kf77-8933326db127') 
                   & (mylga_state!='no') & (mylga_state!="n/a") & (mylga_state!='') & (mylga_state!='12.64876804 8.88785315 412.70001220703125 5.0') & (mylga_state!='365.5') 
                   & (mylga_state!='49618f42-b970-4ce0-baf5-93372b2782d4') & (mylga_state!='imla. Muhd') & (mylga_state!='kwami')
                   & (mylga_state!='240') & (mylga_state!='boat') & (mylga_state!='AUDU MUSA') & (mylga_state!='north_central') & (mylga_state!='northwest') & (mylga_state!='Vembe'))


#cleaning dataset from duplicate sectors
levels(cleanlga$sector)[levels(cleanlga$sector)=="education "] <- "education"

#####GRAPHING #####
#total surveys 
#=> qplot(sector, data=cleanlga, geom="histogram") 

#surveys by state (doesn't give helpful visual representation)
#The issue here is that it doesn't give a helpful visual representaiton of data
#hp <- ggplot(cleanlga, aes(x=sector, color=sector)) + geom_histogram(binwidth=2)
#hp + facet_grid(. ~ mylga_state, scales="free", space="free")


#histogram of survey data by state
print("Writing Data")
write.csv(cleanlga, file="state_survey_data.csv")
draw <- function(fname="state_survey_data.pdf", testmylga_state=NULL) {
  if(!is.null(testmylga_state)) {
    mycleanlga = subset(cleanlga, mylga_state==testmylga_state)
  } else {
    mycleanlga = cleanlga
  }  
	print(paste("Writing charts to", fname))
	pdf(fname)
	d_ply(cleanlga, .(mylga_state), .print=TRUE, function(df){
		ggplot(df, mapping=aes(x=sector)) + geom_histogram(aes(fill=sector)) +
    opts(legend.title = theme_text(size=16, face="bold")) + opts(legend.position="top")
	})
  dev.off()
}   
draw()

write.csv(cleanlga, "cleanlga.csv", row.names=FALSE)
