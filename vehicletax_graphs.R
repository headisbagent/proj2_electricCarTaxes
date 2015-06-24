rm(list=ls())

#####################################################################################################
#Load packages
library(maps)
library(mapproj)
library(mapdata)
library(mapplots)
library(maptools)
library(ggplot2)
library(reshape)

#####################################################################################################

#####################################################################################################
#National level data summary
fed.highway <- read.csv("inputs/highwayfinances.csv",header=TRUE)

#Plot of federal highway funding/spending
pdf('figures/highway_finances.pdf')
par(mar=c(6,8,2,2))
plot(fed.highway$year,fed.highway$fueltax/10^6,type="n",xlab="Year",ylab="Annual revenue and expenditure\n for highways (billions of $)",ylim=c(0,max(fed.highway)/10^6),cex.lab=2,xaxt='n',yaxt='n')
grid()
axis(1,las=0,tck=.02,cex.axis=2)
axis(2,las=2,tck=.02,cex.axis=2)
lines(fed.highway$year,fed.highway$total/10^6,lwd=3,col="blue")
lines(fed.highway$year,fed.highway$expenditure/10^6,lwd=3,col="red")
text(x=c(1998.5,2008),y=c(35,43.5),pos=c(2,2),labels=c("Revenue","Expenditures"))
dev.off()
file.copy('figures/highway_finances.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/highway_finances.pdf')

pdf('figures/forslides/highway_finances.pdf',height=8,width=12)
par(mar=c(6,6,2,2))
plot(fed.highway$year,fed.highway$fueltax/10^6,type="n",xlab="Year",ylab="Annual revenue/spending\n for highways (billions of $)",ylim=c(0,max(fed.highway)/10^6),cex.lab=1.5,xaxt='n',yaxt='n')
grid()
axis(1,las=0,tck=.02)
axis(2,las=2,tck=.02)
lines(fed.highway$year,fed.highway$total/10^6,lwd=3,col="blue")
lines(fed.highway$year,fed.highway$expenditure/10^6,lwd=3,col="red")
text(x=c(1998.5,2008),y=c(35,43.5),pos=c(2,2),labels=c("Revenue","Expenditures"))
dev.off()
#####################################################################################################

#####################################################################################################
#State level data summary

#data import
state.finances <- read.csv("inputs/statefinances.csv",header=TRUE)
state.revenues <- state.finances[,1:8]
state.expenditures <- state.finances[,c(1,10:12,14:18)]

#re-ordering data so that the bar plot goes highest to smallest
state.revenues$state <- reorder(state.revenues$state,rowSums(state.revenues[-1]))
state.expenditures$state <- reorder(state.expenditures$state,rowSums(state.expenditures[-1]))
state.revenues.total <- rowSums(state.revenues[,c(2:length(state.revenues))])
state.expenditures.total <- rowSums(state.expenditures[,c(2:length(state.expenditures))])
state.diff <- data.frame('state'=state.revenues$state,'diff'=state.revenues.total-state.expenditures.total)

#function returning top 25
top25 <- function(input) {
	input$sum <- rowSums(input[-1])
	input <- input[order(-input[,ncol(input)]),]
	return(input[-ncol(input)])
}

state.revenues1 <- top25(state.revenues)[1:26,]
state.revenues2 <- top25(state.revenues)[27:51,]
state.expenditures1 <- state.expenditures[state.expenditures$state%in%state.revenues1$state,]
state.expenditures2 <- state.expenditures[state.expenditures$state%in%state.revenues2$state,]
state.diff1 <- state.diff[state.diff$state%in%state.revenues1$state,]
state.diff2 <- state.diff[state.diff$state%in%state.revenues2$state,]
state.diff1 <- state.diff1[order(match(state.diff1$state,state.revenues1$state)),]
state.diff2 <- state.diff2[order(match(state.diff2$state,state.revenues2$state)),]
state.diff1$type = 'Difference'
state.diff2$type = 'Difference'

#Melt data tables together for ggplot input
m.state.revenues1 <- melt(state.revenues1,id.vars='state')
m.state.expenditures1 <- melt(state.expenditures1,id.vars='state')
m.state.revenues2 <- melt(state.revenues2,id.vars='state')
m.state.expenditures2 <- melt(state.expenditures2,id.vars='state')

empty.hold1 <- data.frame('state'=unique(m.state.expenditures1$state),'variable'='none',value=0,type='Expenditure')
empty.hold2 <- data.frame('state'=unique(m.state.expenditures2$state),'variable'='none',value=0,type='Expenditure')

m.state.revenues1$type <- 'Revenue'
m.state.expenditures1$type <- 'Expenditure'
m.state.diff1$type <- 'Difference'
m.state.all1 <- rbind(m.state.revenues1,empty.hold1,m.state.expenditures1)
m.state.revenues2$type <- 'Revenue'
m.state.expenditures2$type <- 'Expenditure'
m.state.diff2$type <- 'Difference'
m.state.all2 <- rbind(m.state.revenues2,empty.hold2,m.state.expenditures2)


color.vector <- c('navy','blue3','blue1','dodgerblue3','dodgerblue1','lightblue3','lightblue1','white','brown4','lightsalmon4','firebrick1','indianred2','lightpink3','lightpink2','lightpink','mistyrose2')

#plot of top 25 states broken down by components, separated by rev and exp
pdf('figures/top25_states.pdf',height=9,width=9)
#dev.new(height=6,width=9)
ggplot(m.state.all1,aes(x=state,y=value/10^6,fill=variable,width=.8))+
	geom_bar(stat='identity',color='black')+
	facet_wrap(~type)+
	coord_flip()+
	xlab("State")+
	ylab("Billions of Dollars ($)")+
	theme(axis.text.x=element_text(colour="black"),
		axis.text.y=element_text(colour="black"),
		panel.background=element_rect(fill='white',colour='white'),
		panel.grid.major.x=element_line(colour="black"),
		panel.grid.minor.x=element_line(colour="white"))+
	scale_fill_manual(name="Source",values=color.vector,labels=c("Use Taxes","Tolls","Gen Funds","Misc", "Bonds","Fed Gov","Local Gov","","Federal Highway","Other","Roads/Streets","Maintenance","Admin/Police","Interest","Bonds","Grants"))
dev.off()
file.copy('figures/top25_states.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/top25_states.pdf')

pdf('figures/top25_states_diff.pdf',height=9,width=6)
ggplot(state.diff1,aes(x=state,y=diff/10^6,width=.8))+
	geom_bar(stat='identity')+
	facet_wrap(~type)+
	coord_flip()+
	xlab("State")+
	ylab("Billions of Dollars ($)")+
	theme(axis.text.x=element_text(colour="black"),
		axis.text.y=element_text(colour="black"),
		panel.background=element_rect(fill='white',colour='white'),
		panel.grid.major.x=element_line(colour="black"),
		panel.grid.minor.x=element_line(colour="white"))
dev.off()
file.copy('figures/top25_states_diff.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/top25_states_diff.pdf')

#plot of bottom 25 states broken down by components, separated by rev and exp
pdf('figures/bottom25_states.pdf',height=9,width=9)
#dev.new(height=6,width=9)
ggplot(m.state.all2,aes(x=state,y=value/10^6,fill=variable,width=.8))+
	geom_bar(stat='identity',color='black')+
	facet_wrap(~type)+
	coord_flip()+
	xlab("State")+
	ylab("Billions of Dollars ($)")+
	theme(axis.text.x=element_text(colour="black"),
		axis.text.y=element_text(colour="black"),
		panel.background=element_rect(fill='white',colour='white'),
		panel.grid.major.x=element_line(colour="black"),
		panel.grid.minor.x=element_line(colour="white"))+
	scale_fill_manual(name="Source",values=color.vector,labels=c("Use Taxes","Tolls","Gen Funds","Misc", "Bonds","Fed Gov","Local Gov","","Federal Highway","Other","Roads/Streets","Maintenance","Admin/Police","Interest","Bonds","Grants"))
dev.off()
file.copy('figures/bottom25_states.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/bottom25_states.pdf')

pdf('figures/bottom25_states_diff.pdf',height=9,width=6)
ggplot(state.diff2,aes(x=state,y=diff/10^6,width=.8))+
	geom_bar(stat='identity')+
	facet_wrap(~type)+
	coord_flip()+
	xlab("State")+
	ylab("Billions of Dollars ($)")+
	theme(axis.text.x=element_text(colour="black"),
		axis.text.y=element_text(colour="black"),
		panel.background=element_rect(fill='white',colour='white'),
		panel.grid.major.x=element_line(colour="black"),
		panel.grid.minor.x=element_line(colour="white"))
dev.off()
file.copy('figures/bottom25_states_diff.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/bottom25_states_diff.pdf')
#####################################################################################################

#####################################################################################################
#EV Sales and projections

#load electric vehicle sales
ev.sales <- read.csv('inputs/ev_sales.csv')
ev.sales[is.na(ev.sales)] <- 0
ev.sales$Date <- as.Date(ev.sales$Date,"%m-%d-%Y")

#plot electric vehicle sales by model
colors.plot <- c("black","blue4","darkgreen","red2","orchid4","yellow1","orange","lightblue2","green","lightpink","violetred","gold","brown","dodgerblue2")

pdf('figures/ev_sales_bymodel.pdf')
par(mar=c(6,6,2,2))
plot(Chevrolet.Volt~Date,ev.sales,type="n",xlab="Date (monthly)",ylab="EV Sales (monthly)",ylim=c(min(ev.sales[,-1]),max(ev.sales[-1])),cex.lab=1.5,xaxt='n',yaxt='n')
axis(1,las=0,tck=.02)
axis(2,las=2,tck=.02)
#axis(1,ev.sales[,1],format(ev.sales$Date,"%m-%Y"),las=2,cex.axis=.5)
for(i in 2:length(ev.sales)) {
	lines(ev.sales[,1],ev.sales[,i],col=colors.plot[i-1],lwd=2.5)
}
legend("topleft",c("Volt (PHEV)","Leaf (BEV)","SmartED (BEV)","i-MiEV (BEV)","ActiveE (BEV)","Prius (PHEV)","Focus (BEV)","Fit (BEV)","Model S (BEV)","RAV4 (BEV)","C-Max Energi (PHEV)","Accord (PHEV)","Fusion Energi (PHEV)","Spark (BEV)"),col=colors.plot,lwd=2,cex=.7)
dev.off()
file.copy('figures/ev_sales_bymodel.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/ev_sales_bymodel.pdf')

#####################################################################################################
#Popular sales
pop.evsales <- data.frame('date'=ev.sales$Date,'volt'=ev.sales$Chevrolet.Volt,'leaf'=ev.sales$Nissan.Leaf,'prius'=ev.sales$Toyota.Prius.PHV,'model.s'=ev.sales$Tesla.Model.S,'other'=rowSums(ev.sales[,c(4,5,6,8,9,11,12,13,14,15)]))

#plot popular electric vehicle sales by model
colors.plot2 <- c('black','blue4','darkgreen','red2','orange')

pdf('figures/popular_ev_sales_bymodel.pdf',height=5,width=8)
par(mar=c(6,6,2,2))
plot(volt~date,pop.evsales,type="n",xlab="Date (monthly)",ylab="EV Sales (monthly)",ylim=c(min(pop.evsales[,-1]),max(pop.evsales[-1])),cex.lab=1,xaxt='n',yaxt='n')
axis(1,las=0,at=pop.evsales$date[format(pop.evsales$date,"%m")=='01'|format(pop.evsales$date,"%m")=='07'],labels=format(pop.evsales$date[format(pop.evsales$date,"%m")=='01'|format(pop.evsales$date,"%m")=='07'],"%m-%Y"),cex=.8,tck=.02)
axis(2,las=2,tck=.02)
#axis(1,ev.sales[,1],format(ev.sales$Date,"%m-%Y"),las=2,cex.axis=.5)
for(i in 2:length(pop.evsales)) {
	lines(pop.evsales[,1],pop.evsales[,i],col=colors.plot2[i-1],lwd=2.5)
}
legend("topleft",c("Volt (PHEV)","Leaf (BEV)","Prius (PHEV)","Model S (BEV)","Other"),col=colors.plot2,lwd=2,cex=.7)
dev.off()
file.copy('figures/popular_ev_sales_bymodel.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/popular_ev_sales_bymodel.pdf')

pdf('figures/forslides/popular_ev_sales_bymodel.pdf',height=6,width=6)
par(mar=c(6,6,2,2))
plot(volt~date,pop.evsales,type="n",xlab="Date (monthly)",ylab="EV Sales (monthly)",ylim=c(min(pop.evsales[,-1]),max(pop.evsales[-1])),cex.lab=1,xaxt='n',yaxt='n')
axis(1,las=0,at=pop.evsales$date[format(pop.evsales$date,"%m")=='01'|format(pop.evsales$date,"%m")=='07'],labels=format(pop.evsales$date[format(pop.evsales$date,"%m")=='01'|format(pop.evsales$date,"%m")=='07'],"%m-%Y"),cex=.8,tck=.02)
axis(2,las=2,tck=.02)
#axis(1,ev.sales[,1],format(ev.sales$Date,"%m-%Y"),las=2,cex.axis=.5)
for(i in 2:length(pop.evsales)) {
	lines(pop.evsales[,1],pop.evsales[,i],col=colors.plot2[i-1],lwd=2.5)
}
legend("topleft",c("Volt (PHEV)","Leaf (BEV)","Prius (PHEV)","Model S (BEV)","Other"),col=colors.plot2,lwd=2,cex=.7)
dev.off()
#####################################################################################################
#Extra

#plot total electric vehicle sales
pdf('figures/ev_sales_total.pdf')
plot(Chevrolet.Volt~Date,ev.sales,type="n",xlab="Date (monthly)",ylab="Total EV Sales",ylim=c(0,max(rowSums(ev.sales[,2:length(ev.sales)]))),xaxt='n',yaxt='n')
axis(1,las=0)
axis(2,las=2)
lines(ev.sales[,1],rowSums(ev.sales[,2:length(ev.sales)]))
dev.off()
file.copy('figures/ev_sales_total.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/ev_sales_total.pdf')

#load hybrid vehicle sales
hev.sales <- read.csv('inputs/hybrid_sales.csv')
hev.sales[is.na(hev.sales)] <- 0
hev.sales$Date <- as.Date(hev.sales$Date,"%m/%d/%Y")

#plot hybrid vehicle sales by model
colors.plot3 <- c("blue4","red2")
pdf('figures/hev_sales_bymodel.pdf')
plot(Toyota.Prius~Date,hev.sales,type="n",xlab="Date (monthly)",ylab="HEV Sales",ylim=c(min(hev.sales[,-1]),max(ev.sales[-1])),xaxt='n',yaxt='n')
axis(1,las=0)
axis(2,las=2)
#axis(1,ev.sales[,1],format(ev.sales$Date,"%m-%Y"),las=2,cex.axis=.5)
for(i in 2:length(hev.sales)) {
	lines(hev.sales[,1],hev.sales[,i],col=colors.plot3[i-1],lwd=2.5)
}
legend("topleft",c("Prius","Insight"),col=colors.plot3,lwd=2,cex=.7)
dev.off()
file.copy('figures/hev_sales_bymodel.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/hev_sales_bymodel.pdf')

#plot total electric vehicle sales vs hybrid electric vehicle sales
pdf('figures/combined_sales_total.pdf')
plot(1:length(ev.sales$Chevrolet.Volt),ev.sales$Chevrolet.Volt,type="n",xlab="Months Since Introduction",ylab="Total EV Sales",ylim=c(0,max(rowSums(ev.sales[,2:length(ev.sales)]))),xaxt='n',yaxt='n')
axis(1,las=0)
axis(2,las=2)
lines(1:length(rowSums(ev.sales[,2:length(ev.sales)])),rowSums(ev.sales[,2:length(ev.sales)]),col="blue4",lwd=2.5)
lines(1:length(rowSums(hev.sales[,2:length(hev.sales)])),rowSums(hev.sales[,2:length(hev.sales)]),col="red2",lwd=2.5)
legend("topleft",c("EVs","HEVs"),col=c("blue4","red2"),lwd=2)
dev.off()
file.copy('figures/combined_sales_total.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/combined_sales_total.pdf')

#####################################################################################################
#eia projection
projections <- read.csv('inputs/eia_projection.csv')
eia.2012 <- projections[,1:5]
eia.2013 <- projections[,c(1,6:9)]
colnames(eia.2012)=c("Year","BEV100","PHEV10","PHEV40","FFV")
colnames(eia.2013)=c("Year","BEV100","PHEV10","PHEV40","FFV")

#annual historical EV sales
annual.bevsales <- data.frame('year'=c(2011,2012),'sales'=c(sum(rowSums(ev.sales[-1][,c(2:5,7:14)])[1:13]),sum(rowSums(ev.sales[-1][,c(2:5,7:14)])[14:25])))
annual.phev10sales <- data.frame('year'=c(2011,2012),'sales'=c(sum(ev.sales[,7][1:13]),sum(ev.sales[,7][14:25])))
annual.phev40sales <- data.frame('year'=c(2011,2012),'sales'=c(sum(ev.sales[,2][1:13]),sum(ev.sales[,2][14:25])))

pdf('figures/eia_forecast2013.pdf')
par(mar=c(6,6,2,2))
plot(1,type="n",xlab="Year",ylab="",lwd=3.5,col="dodgerblue4",xlim=c(2011,2025),ylim=c(0,max(eia.2013[,2:4]/1000)),xaxt="n",yaxt="n",cex.lab=2)
grid()
axis(1,cex.axis=1,las=0,at=2011:2025,labels=2011:2025,tck=.02,cex.axis=2)
axis(2,cex.axis=1,las=2,tck=.02,cex.axis=2)
mtext('Sales (thousands)',side=2,line=4,cex=2)
abline(v=2012)
lines(eia.2013$Year,eia.2013$BEV100/1000,lty=1,lwd=3.5,col="dodgerblue4")
lines(eia.2013$Year,eia.2013$PHEV10/1000,lty=5,lwd=3.5,col="dodgerblue4")
lines(eia.2013$Year,eia.2013$PHEV40/1000,lty=3,lwd=3.5,col="dodgerblue4")
lines(2011:2012,annual.bevsales$sales/1000,lty=1,lwd=3.5,col='red')
lines(2011:2012,annual.phev10sales$sales/1000,lty=5,lwd=3.5,col='red')
lines(2011:2012,annual.phev40sales$sales/1000,lty=3,lwd=3.5,col='red')
text(x=c(2019,2015,2021.8),y=c(15,30,120),pos=c(4,4,4),labels=c("BEV-100","PHEV-10","PHEV-40"))
text(x=2012,y=150,pos=2,labels="Historical Sales",srt=90)
text(x=2012,y=150,pos=4,labels="Projected Sales",srt=270)
arrows(2012,100,2011,100,length=.1,lwd=1.5)
arrows(2012,155,2013,155,length=.1,lwd=1.5)
dev.off()
file.copy('figures/eia_forecast2013.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/eia_forecast2013.pdf')

pdf('figures/forslides/eia_forecast2013.pdf',height=6,width=6)
par(mar=c(6,6,2,2))
plot(1,type="n",xlab="Year",ylab="Sales (thousands)",lwd=3.5,col="dodgerblue4",xlim=c(2011,2025),ylim=c(0,max(eia.2013[,2:4]/1000)),xaxt="n",yaxt="n",cex.lab=1.5)
grid()
axis(1,cex.axis=1,las=0,at=2011:2025,labels=2011:2025)
axis(2,cex.axis=1,las=2)
abline(v=2012)
lines(eia.2013$Year,eia.2013$BEV100/1000,lty=1,lwd=3.5,col="dodgerblue4")
lines(eia.2013$Year,eia.2013$PHEV10/1000,lty=5,lwd=3.5,col="dodgerblue4")
lines(eia.2013$Year,eia.2013$PHEV40/1000,lty=3,lwd=3.5,col="dodgerblue4")
lines(2011:2012,annual.bevsales$sales/1000,lty=1,lwd=3.5,col='red')
lines(2011:2012,annual.phev10sales$sales/1000,lty=5,lwd=3.5,col='red')
lines(2011:2012,annual.phev40sales$sales/1000,lty=3,lwd=3.5,col='red')
text(x=c(2019,2015,2021.8),y=c(15,30,120),pos=c(4,4,4),labels=c("BEV-100","PHEV-10","PHEV-40"))
text(x=2012,y=150,pos=2,labels="Historical Sales",srt=90)
text(x=2012,y=150,pos=4,labels="Projected Sales",srt=270)
arrows(2012,100,2011,100,length=.1,lwd=1.5)
arrows(2012,155,2013,155,length=.1,lwd=1.5)
dev.off()
#####################################################################################################
#Policy forecasts

forecasts2 <- read.csv('inputs/forecasts_v2.csv')

grey.shade = rgb(0,0,0,alpha=0.2,maxColorValue=1)

pdf('figures/forecasts.pdf',height=6,width=9)
par(mar=c(4,6,2,2))
plot(x=forecasts2$Year,y=1:nrow(forecasts2),type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(2008,2055),ylim=c(0,1))
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=2.5,cex=1.5)
mtext('Market Share',side=2,line=3.5,cex=1.5)
polygon(x=c(2012,2020,2050,2050,2020,2010),y=c(0,.55,1,.17,0,0),col=grey.shade,border=NA)
for(column in 2:ncol(forecasts2)){
	lines(x=na.omit(forecasts2[,c(1,column)])$Year,y=na.omit(forecasts2[,column]),lwd=2,lty=column,col='grey30')
	if(column==3) {
		text(x=tail(na.omit(forecasts2[,c(1,column)])$Year,n=1),y=tail(na.omit(forecasts2[,column]),n=1)-.03,labels=labels(forecasts2)[[2]][column],cex=.8,pos=1)
	}
	else {
		text(x=tail(na.omit(forecasts2[,c(1,column)])$Year,n=1),y=tail(na.omit(forecasts2[,column]),n=1),labels=labels(forecasts2)[[2]][column],cex=.8,pos=4)
	}
}
text(x=2040,y=.4,pos=4,labels='FORECASTS',font=2)
dev.off()

#####################################################################################################
#Chloropleth Maps
#import data
camry <- read.csv("inputs/camry.csv")
civic <- read.csv("inputs/civic.csv")
f150 <- read.csv("inputs/f150.csv")
leaf <- read.csv("inputs/leaf.csv")
prius <- read.csv("inputs/prius.csv")
prius.phev <- read.csv("inputs/priusphev.csv")
volt <- read.csv("inputs/volt.csv")

#state names
mapnames <- map("state",plot=FALSE)$names
mapnames.state <- ifelse(regexpr(":",mapnames) < 0,mapnames,
	substr(mapnames, 1, regexpr(":",mapnames)-1))

#convert states to lowercase
camry$State <- tolower(camry$State)
civic$State <- tolower(civic$State)
f150$State <- tolower(f150$State)
leaf$State <- tolower(leaf$State)
prius$State <- tolower(prius$State)
prius.phev$State <- tolower(prius.phev$State)
volt$State <- tolower(volt$State)

#chloropleth quantities
color.split <- c(1500,3000,4500,6000)

#camry choropleth map
for(i in 1:nrow(camry)) {
	if(camry$Total[i]>color.split[4]) {
		camry$color[i] <- "blue4"
	}
	else if(camry$Total[i]>color.split[3]) {
		camry$color[i] <- "blue2"
	}
	else if(camry$Total[i]>color.split[2]) {
		camry$color[i] <- "dodgerblue"
	}
	else if(camry$Total[i]>color.split[1]) {
		camry$color[i] <- "lightskyblue"
	}
	else {
		camry$color[i] <- "white"
	}
}

pdf('figures/camry_fees.pdf')
map("state",fill=TRUE,col=camry$color[match(mapnames.state,camry$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(camry[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/camry_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/camry_fees.pdf')

#civic choropleth map
for(i in 1:nrow(civic)) {
	if(civic$Total[i]>color.split[4]) {
		civic$color[i] <- "blue4"
	}
	else if(civic$Total[i]>color.split[3]) {
		civic$color[i] <- "blue2"
	}
	else if(civic$Total[i]>color.split[2]) {
		civic$color[i] <- "dodgerblue"
	}
	else if(civic$Total[i]>color.split[1]) {
		civic$color[i] <- "lightskyblue"
	}
	else {
		civic$color[i] <- "white"
	}
}

pdf('figures/civic_fees.pdf')
map("state",fill=TRUE,col=civic$color[match(mapnames.state,civic$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(civic[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/civic_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/civic_fees.pdf')

#f150 choropleth map
for(i in 1:nrow(f150)) {
	if(f150$Total[i]>color.split[4]) {
		f150$color[i] <- "blue4"
	}
	else if(f150$Total[i]>color.split[3]) {
		f150$color[i] <- "blue2"
	}
	else if(f150$Total[i]>color.split[2]) {
		f150$color[i] <- "dodgerblue"
	}
	else if(f150$Total[i]>color.split[1]) {
		f150$color[i] <- "lightskyblue"
	}
	else {
		f150$color[i] <- "white"
	}
}

pdf('figures/f150_fees.pdf')
map("state",fill=TRUE,col=f150$color[match(mapnames.state,f150$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(f150[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/f150_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/f150_fees.pdf')

#leaf choropleth map
for(i in 1:nrow(leaf)) {
	if(leaf$Total[i]>color.split[4]) {
		leaf$color[i] <- "blue4"
	}
	else if(leaf$Total[i]>color.split[3]) {
		leaf$color[i] <- "blue2"
	}
	else if(leaf$Total[i]>color.split[2]) {
		leaf$color[i] <- "dodgerblue"
	}
	else if(leaf$Total[i]>color.split[1]) {
		leaf$color[i] <- "lightskyblue"
	}
	else {
		leaf$color[i] <- "white"
	}
}

pdf('figures/leaf_fees.pdf')
map("state",fill=TRUE,col=leaf$color[match(mapnames.state,leaf$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(leaf[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/leaf_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/leaf_fees.pdf')

#prius choropleth map
for(i in 1:nrow(prius)) {
	if(prius$Total[i]>color.split[4]) {
		prius$color[i] <- "blue4"
	}
	else if(prius$Total[i]>color.split[3]) {
		prius$color[i] <- "blue2"
	}
	else if(prius$Total[i]>color.split[2]) {
		prius$color[i] <- "dodgerblue"
	}
	else if(prius$Total[i]>color.split[1]) {
		prius$color[i] <- "lightskyblue"
	}
	else {
		prius$color[i] <- "white"
	}
}

pdf('figures/prius_fees.pdf')
map("state",fill=TRUE,col=prius$color[match(mapnames.state,prius$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(prius[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/prius_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/prius_fees.pdf')

#prius phev choropleth map
for(i in 1:nrow(prius.phev)) {
	if(prius.phev$Total[i]>color.split[4]) {
		prius.phev$color[i] <- "blue4"
	}
	else if(prius.phev$Total[i]>color.split[3]) {
		prius.phev$color[i] <- "blue2"
	}
	else if(prius.phev$Total[i]>color.split[2]) {
		prius.phev$color[i] <- "dodgerblue"
	}
	else if(prius.phev$Total[i]>color.split[1]) {
		prius.phev$color[i] <- "lightskyblue"
	}
	else {
		prius.phev$color[i] <- "white"
	}
}

pdf('figures/priusphev_fees.pdf')
map("state",fill=TRUE,col=prius.phev$color[match(mapnames.state,prius.phev$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(prius.phev[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/priusphev_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/priusphev_fees.pdf')

#volt choropleth map
for(i in 1:nrow(volt)) {
	if(volt$Total[i]>color.split[4]) {
		volt$color[i] <- "blue4"
	}
	else if(volt$Total[i]>color.split[3]) {
		volt$color[i] <- "blue2"
	}
	else if(volt$Total[i]>color.split[2]) {
		volt$color[i] <- "dodgerblue"
	}
	else if(volt$Total[i]>color.split[1]) {
		volt$color[i] <- "lightskyblue"
	}
	else {
		volt$color[i] <- "white"
	}
}

pdf('figures/volt_fees.pdf')
map("state",fill=TRUE,col=volt$color[match(mapnames.state,volt$State)])
raincol <- c("purple","red","darkgreen","black","yellow")
draw.pie(state.center$x,state.center$y,as.matrix(volt[,c(3,4,5,6,7)]),radius=.65,col=raincol,scale=FALSE)
#legend("bottomleft",leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown")
#legend("bottomright",leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),	fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7)
dev.off()
file.copy('figures/volt_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/volt_fees.pdf')

pdf('figures/map_legend.pdf')
plot(x=0:50,y=0:50,xlab='',ylab='',type='n',xaxt='n',yaxt='n',bty='n')
legend(x=0,y=50,leg=c("Federal Fuel Tax","State Fuel Tax","Registration Fees","Title Fees","Inspection Fees"),
	fill=c("purple","red","darkgreen","black","yellow"),cex=0.7,title="Fee Breakdown",box.lwd = 0,box.col = "white",bg = "white")
legend(x=15,y=50,leg=c('0-1499','1500-2999','3000-4499','4500-5999','>6000'),
	fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Total Fees ($)",cex=.7,box.lwd = 0,box.col = "white",bg = "white",ncol=2)
dev.off()
file.copy('figures/map_legend.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/map_legend.pdf')

#####################################################################################################
#Differences map

diff.f150.leaf <- data.frame('State'=f150$State,'Total'=f150$Total-leaf$Total)
color.split2 <- c(1000,2000,3000,4000)
for(i in 1:nrow(diff.f150.leaf)) {
	if(diff.f150.leaf$Total[i]>color.split2[4]) {
		diff.f150.leaf$color[i] <- "red4"
	}
	else if(diff.f150.leaf$Total[i]>color.split2[3]) {
		diff.f150.leaf$color[i] <- "red1"
	}
	else if(diff.f150.leaf$Total[i]>color.split2[2]) {
		diff.f150.leaf$color[i] <- "indianred2"
	}
	else if(diff.f150.leaf$Total[i]>color.split2[1]) {
		diff.f150.leaf$color[i] <- "pink1"
	}
	else {
		diff.f150.leaf$color[i] <- "white"
	}
}

pdf('figures/diff_fees.pdf')
map("state",fill=TRUE,col=diff.f150.leaf$color[match(mapnames.state,diff.f150.leaf$State)])
legend("bottomright",leg=c('0-999','1000-1999','2000-2999','3000-3999','>4000'),
	fill=rev(c("red4","red1","indianred2","pink1","white")),title="Revenue Difference ($)",bty='n',cex=.7)
dev.off()
file.copy('figures/diff_fees.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/diff_fees.pdf')

#####################################################################################################

#####################################################################################################
#Prius distribution for appendix

#load state sale distribution data
state.percents <- read.csv('inputs/prius_statepercentages.csv')

#total and percent diff by state
state.percents.total <- read.csv('inputs/total_statepercentages.csv')
state.percents.diff <- read.csv('inputs/percentdiff_bystate.csv',header=FALSE)

#chloropleth map of prius state sales distribution
priussales <- data.frame("state"=camry$State,"percent"=colMeans(state.percents))
priussales.quantiles <- quantile(colMeans(state.percents),probs=c(.2,.4,.6,.8))
for(i in 1:nrow(priussales)) {
	if(priussales$percent[i]>priussales.quantiles[4]) {
		priussales$color[i] <- "blue4"
	}
	else if(priussales$percent[i]>priussales.quantiles[3]) {
		priussales$color[i] <- "blue2"
	}
	else if(priussales$percent[i]>priussales.quantiles[2]) {
		priussales$color[i] <- "dodgerblue"
	}
	else if(priussales$percent[i]>priussales.quantiles[1]) {
		priussales$color[i] <- "lightskyblue"
	}
	else {
		priussales$color[i] <- "white"
	}
}

pdf('figures/priussales.pdf')
map("state",fill=TRUE,col=priussales$color[match(mapnames.state,priussales$state)])
legend("bottomright",leg=c(as.expression(paste(0," - ",signif(priussales.quantiles[1],3))),
	as.expression(paste(signif(priussales.quantiles[1]*100,3)," - ",signif(priussales.quantiles[2]*100,3))),
	as.expression(paste(signif(priussales.quantiles[2]*100,3)," - ",signif(priussales.quantiles[3]*100,3))),
	as.expression(paste(signif(priussales.quantiles[3]*100,3)," - ",signif(priussales.quantiles[4]*100,3))),
	as.expression(paste(signif(priussales.quantiles[4]*100,3)," - ",signif(max(priussales$percent)*100,3)))),
	fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Sales Distribution (%)",cex=.7)
dev.off()
file.copy('figures/priussales.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/priussales.pdf')

#chloropleth map of total state sales distribution
totalsales <- data.frame("state"=camry$State,"percent"=colMeans(state.percents.total))
totalsales.quantiles <- quantile(colMeans(state.percents.total),probs=c(.2,.4,.6,.8))
for(i in 1:nrow(totalsales)) {
	if(totalsales$percent[i]>totalsales.quantiles[4]) {
		totalsales$color[i] <- "blue4"
	}
	else if(totalsales$percent[i]>totalsales.quantiles[3]) {
		totalsales$color[i] <- "blue2"
	}
	else if(totalsales$percent[i]>totalsales.quantiles[2]) {
		totalsales$color[i] <- "dodgerblue"
	}
	else if(totalsales$percent[i]>totalsales.quantiles[1]) {
		totalsales$color[i] <- "lightskyblue"
	}
	else {
		totalsales$color[i] <- "white"
	}
}

pdf('figures/totalsales.pdf')
map("state",fill=TRUE,col=totalsales$color[match(mapnames.state,totalsales$state)])
legend("bottomright",leg=c(as.expression(paste(0," - ",signif(totalsales.quantiles[1],3))),
	as.expression(paste(signif(totalsales.quantiles[1]*100,3)," - ",signif(totalsales.quantiles[2]*100,3))),
	as.expression(paste(signif(totalsales.quantiles[2]*100,3)," - ",signif(totalsales.quantiles[3]*100,3))),
	as.expression(paste(signif(totalsales.quantiles[3]*100,3)," - ",signif(totalsales.quantiles[4]*100,3))),
	as.expression(paste(signif(totalsales.quantiles[4]*100,3)," - ",signif(max(totalsales$percent)*100,3)))),
	fill=c("white","lightskyblue","dodgerblue","blue2","blue4"),title="Sales Distribution (%)",cex=.7)
dev.off()
file.copy('figures/totalsales.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/totalsales.pdf')

#chloropleth map of difference in state sales distribution
diffsales <- data.frame("state"=camry$State,"percent"=state.percents.diff*-1)
colnames(diffsales) <- c("state","percent")
diffsales.quantiles <- quantile(diffsales$percent,probs=c(.2,.4,.6,.8))
for(i in 1:nrow(diffsales)) {
	if(diffsales$percent[i]>diffsales.quantiles[4]) {
		diffsales$color[i] <- "lightskyblue"
	}
	else if(diffsales$percent[i]>diffsales.quantiles[3]) {
		diffsales$color[i] <- "white"
	}
	else if(diffsales$percent[i]>diffsales.quantiles[2]) {
		diffsales$color[i] <- "lightpink"
	}
	else if(diffsales$percent[i]>diffsales.quantiles[1]) {
		diffsales$color[i] <- "lightcoral"
	}
	else {
		diffsales$color[i] <- "red"
	}
}

pdf('figures/diffsales.pdf')
map("state",fill=TRUE,col=diffsales$color[match(mapnames.state,diffsales$state)])
legend("bottomright",leg=c(as.expression(paste(signif(min(diffsales$percent)*100,3)," - ",signif(diffsales.quantiles[1],3))),
	as.expression(paste(signif(diffsales.quantiles[1]*100,3)," - ",signif(diffsales.quantiles[2]*100,3))),
	as.expression(paste(signif(diffsales.quantiles[2]*100,3)," - ",signif(diffsales.quantiles[3]*100,3))),
	as.expression(paste(signif(diffsales.quantiles[3]*100,3)," - ",signif(diffsales.quantiles[4]*100,3))),
	as.expression(paste(signif(diffsales.quantiles[4]*100,3)," - ",signif(max(diffsales$percent)*100,3)))),
	fill=c("red","lightcoral","lightpink","white","lightskyblue"),title="Sales Distribution (%)",cex=.7)
dev.off()
file.copy('figures/diffsales.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/diffsales.pdf')

#####################################################################################################

#####################################################################################################
#EIA AEO by region
region.list <- c('east.north.central','east.south.central','middle.atlantic','mountain','newengland','pacific','south.atlantic','west.north.central','west.south.central')
extract.2013proj <- function(inputtable) {
	holdtable <- read.csv(inputtable,header=FALSE)
	bev <- numeric(0)
	phev10 <- numeric(0)
	phev40 <- numeric(0)
	ffv <- numeric(0)
	for(i in seq(5,31,by=2)) {
		bev <- c(bev,as.numeric(levels(as.ordered(holdtable[[i]][15]))))
		phev10 <- c(phev10,as.numeric(levels(as.ordered(holdtable[[i]][17]))))
		phev40 <- c(phev40,as.numeric(levels(as.ordered(holdtable[[i]][18]))))
		ffv <- c(ffv,as.numeric(levels(as.ordered(holdtable[[i]][14]))))
	}
	out <- data.frame('year'=2012:2025,'BEV100'=round(bev*1000,0),'PHEV10'=round(phev10*1000,0),'PHEV40'=round(phev40*1000,0),'FFV'=round(ffv*1000,0))
	return(out)
}

for(i in 1:length(region.list)) {
	assign(paste(region.list[i]),extract.2013proj(paste('inputs/aeo_byregion/',region.list[i],'.csv',sep='')))
}

#####################################################################################################

#####################################################################################################
#Projected sales by state

#Prius sales for proxy, matrix for drawing values
prius.sales <- read.csv('inputs/priussales_bystate.csv',header=TRUE)
prius.sales <- prius.sales[-nrow(prius.sales),]

future.sales.bystate <- function() {
	#states in each region
	pacific.states <- c('ALASKA','WASHINGTON','OREGON','CALIFORNIA','HAWAII')
	mountain.states <- c('MONTANA','IDAHO','NEVADA','UTAH','WYOMING','COLORADO','ARIZONA','NEW.MEXICO')
	west.north.central.states <- c('NORTH.DAKOTA','MINNESOTA','SOUTH.DAKOTA','NEBRASKA','KANSAS','IOWA','MISSOURI')
	west.south.central.states <- c('TEXAS','OKLAHOMA','ARKANSAS','LOUISIANA')
	east.north.central.states <- c('WISCONSIN','MICHIGAN','ILLINOIS','INDIANA','OHIO')
	east.south.central.states <- c('KENTUCKY','TENNESSEE','MISSISSIPPI','ALABAMA')
	south.atlantic.states <- c('WEST.VIRGINIA','MARYLAND','DELAWARE','VIRGINIA','NORTH.CAROLINA','SOUTH.CAROLINA','GEORGIA','FLORIDA')
	middle.atlantic.states <- c('PENNSYLVANIA','NEW.YORK','NEW.JERSEY')
	newengland.states <- c('MAINE','VERMONT','NEW.HAMPSHIRE','MASSACHUSETTS','RHODE.ISLAND','CONNECTICUT')
	#creating empty arrays for final list output
	BEV100.out <- array(dim=c(nrow(eia.2013),50,nrow(prius.sales)))
	PHEV10.out <- array(dim=c(nrow(eia.2013),50,nrow(prius.sales)))
	PHEV40.out <- array(dim=c(nrow(eia.2013),50,nrow(prius.sales)))
	FFV.out <- array(dim=c(nrow(eia.2013),50,nrow(prius.sales)))
	for(j in 1:nrow(prius.sales)) {
		hold.sales <- prius.sales[j,]
		#creating empty matrices to be filled, one for each row of prius sales
		BEV100.bystate <- matrix(numeric(0),nrow=nrow(eia.2013))
		PHEV10.bystate <- matrix(numeric(0),nrow=nrow(eia.2013))
		PHEV40.bystate <- matrix(numeric(0),nrow=nrow(eia.2013))
		FFV.bystate <- matrix(numeric(0),nrow=nrow(eia.2013))
		for(i in region.list) {
			#takes prius sales and keeps only states within the region
			hold.statesales <- hold.sales[colnames(hold.sales)%in%get(paste(i,'.states',sep=''))]
			#transformation of the sales to the proportional distribution in each region
			hold.statetotal <- sum(hold.statesales)
			hold.statedist <- hold.statesales/hold.statetotal
			for(k in colnames(eia.2013[-1])) {
				#gives table of sales by state for each technology
				assign(paste('hold.',k,'.bystate',sep=''),(as.matrix('[['(get(i),k),ncol=1)%*%as.matrix(hold.statedist,nrow=1)))
				#appends the table to the matrix holding all other regions that have been run
				assign(paste(k,'.bystate',sep=''),cbind(get(paste(k,'.bystate',sep='')),get(paste('hold.',k,'.bystate',sep=''))))
			}
		}
		#assigns completed table (of each technology sales across 50 states) to the empty array
		BEV100.out[,,j] <- BEV100.bystate
		PHEV10.out[,,j] <- PHEV10.bystate
		PHEV40.out[,,j] <- PHEV40.bystate
		FFV.out[,,j] <- FFV.bystate
	}
	state.order <<- colnames(BEV100.bystate)
	out <- list(BEV100.out,PHEV10.out,PHEV40.out,FFV.out)
	return(out)
}
projectedsales.bystate <- future.sales.bystate()

state.order <- tolower(state.order)
state.order <- gsub('\\.',' ',state.order)

for(i in 1:ncol(eia.2013[-1])) {
	assign(paste('mean.projected',colnames(eia.2013[i+1]),'.bystate',sep=''),as.data.frame(apply(projectedsales.bystate[[i]],c(1,2),mean)))
	assign(paste('q25.projected',colnames(eia.2013[i+1]),'.bystate',sep=''),apply(projectedsales.bystate[[i]],c(1,2),quantile,probs=.025))
	assign(paste('q975.projected',colnames(eia.2013[i+1]),'.bystate',sep=''),apply(projectedsales.bystate[[i]],c(1,2),quantile,probs=.975))
}
all.cases <- list(mean.projectedBEV100.bystate, mean.projectedPHEV10.bystate, mean.projectedPHEV40.bystate, mean.projectedFFV.bystate, q25.projectedBEV100.bystate, q25.projectedPHEV10.bystate, q25.projectedPHEV40.bystate, q25.projectedFFV.bystate, q975.projectedBEV100.bystate, q975.projectedPHEV10.bystate, q975.projectedPHEV40.bystate, q975.projectedFFV.bystate)
name.it <- function(table) {
	colnames(table) <- state.order
	table <- table[,order(colnames(table))]
	return(table)
}
all.cases <- lapply(all.cases,name.it)
#####################################################################################################

#####################################################################################################
#Calculating projected revenue losses

future.revenue.decrease <- function(salesinput,bevrev,phev10rev,phev40rev) {
	out <- list()
	counter = 1
	for(i in 1:length(salesinput)) {
		#mod to distinguish technologies
		if(i%%4 == 1) {
			hold <- t(salesinput[[i]])*(camry$Total-bevrev)
			out[[counter]] <- hold
			counter = counter+1
		}
		else if(i%%4 == 2) {
			hold <- t(salesinput[[i]])*(camry$Total-phev10rev)
			out[[counter]] <- hold
			counter = counter+1
		}
		else if (i%%4 ==3) {
			hold <- t(salesinput[[i]])*(camry$Total-phev40rev)
			out[[counter]] <- hold
			counter = counter+1
		}
	}
	return(out)
}
#list of revenue by year, state, and technology and distribution (row=state, column=year, list num=technology and dist: by 3's BEV, PHEV10, PHEV40, sets of 3 mean, q25, q975)
all.revenues.decrease <- future.revenue.decrease(all.cases,leaf$Total,prius.phev$Total,volt$Total)

#####################################################################################################

#####################################################################################################
#State level map (only needs state and technology)
#data reduction to state and technology (use only list 1-3, sum columns)
total.state.revenueloss <- data.frame('bev.loss'=rowSums(all.revenues.decrease[[1]]),'phev10.loss'=rowSums(all.revenues.decrease[[2]]),'phev40.loss'=rowSums(all.revenues.decrease[[3]]))
total.state.revenueloss$totalloss <- rowSums(total.state.revenueloss)
total.state.revenueloss$state <- rownames(total.state.revenueloss)

#assigning colors by quantiles of loss
loss.quantiles <- quantile(total.state.revenueloss$totalloss ,probs=c(.2,.4,.6,.8))
for(i in 1:nrow(total.state.revenueloss)) {
	if(total.state.revenueloss$totalloss [i]>loss.quantiles[4]) {
		total.state.revenueloss$color[i] <- "red4"
	}
	else if(total.state.revenueloss$totalloss [i]>loss.quantiles[3]) {
		total.state.revenueloss$color[i] <- "red1"
	}
	else if(total.state.revenueloss$totalloss [i]>loss.quantiles[2]) {
		total.state.revenueloss$color[i] <- "indianred2"
	}
	else if(total.state.revenueloss$totalloss [i]>loss.quantiles[1]) {
		total.state.revenueloss$color[i] <- "pink1"
	}
	else {
		total.state.revenueloss$color[i] <- "white"
	}
}

pdf('figures/revenue_loss_bystate.pdf')
map("state",fill=TRUE,col=total.state.revenueloss$color[match(mapnames.state,total.state.revenueloss$state)])
legend("bottomright",leg=c(as.expression(paste("< ",abs(signif(loss.quantiles[1]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles[1]/10^6,2))," - ",abs(signif(loss.quantiles[2]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles[2]/10^6,2))," - ",abs(signif(loss.quantiles[3]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles[3]/10^6,2))," - ",abs(signif(loss.quantiles[4]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles[4]/10^6,2))," - ",abs(signif(max(total.state.revenueloss$totalloss)/10^6,2))))),
	fill=rev(c("red4","red1","indianred2","pink1","white")),title="Revenue Loss \n(millions of $)",bty='n',cex=.7)
dev.off()
file.copy('figures/revenue_loss_bystate.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/revenue_loss_bystate.pdf')
#####################################################################################################
#Normalized losses (to 2012 population)
#reading in data on 2012 population
population.bystate.2012 <- read.csv('inputs/2012_population.csv',header=TRUE)
total.state.revenueloss$normalizedloss <- total.state.revenueloss$totalloss/population.bystate.2012$Population

 #assigning color2s by quantiles of loss
normalizedloss.quantiles <- quantile(total.state.revenueloss$normalizedloss ,probs=c(.2,.4,.6,.8))
for(i in 1:nrow(total.state.revenueloss)) {
	if(total.state.revenueloss$normalizedloss [i]>normalizedloss.quantiles[4]) {
		total.state.revenueloss$color2[i] <- "red4"
	}
	else if(total.state.revenueloss$normalizedloss [i]>normalizedloss.quantiles[3]) {
		total.state.revenueloss$color2[i] <- "red1"
	}
	else if(total.state.revenueloss$normalizedloss [i]>normalizedloss.quantiles[2]) {
		total.state.revenueloss$color2[i] <- "indianred2"
	}
	else if(total.state.revenueloss$normalizedloss [i]>normalizedloss.quantiles[1]) {
		total.state.revenueloss$color2[i] <- "pink1"
	}
	else {
		total.state.revenueloss$color2[i] <- "white"
	}
}

pdf('figures/revenue_loss_bystate_normalized.pdf')
map("state",fill=TRUE,col=total.state.revenueloss$color2[match(mapnames.state,total.state.revenueloss$state)])
legend("bottomright",leg=c(as.expression(paste("< ",abs(signif(normalizedloss.quantiles[1],2)))),
	as.expression(paste(abs(signif(normalizedloss.quantiles[1],2))," - ",abs(signif(normalizedloss.quantiles[2],2)))),
	as.expression(paste(abs(signif(normalizedloss.quantiles[2],2))," - ",abs(signif(normalizedloss.quantiles[3],2)))),
	as.expression(paste(abs(signif(normalizedloss.quantiles[3],2))," - ",abs(signif(normalizedloss.quantiles[4],2)))),
	as.expression(paste(abs(signif(normalizedloss.quantiles[4],2))," - ",abs(signif(max(total.state.revenueloss$normalizedloss),2))))),
	fill=rev(c("red4","red1","indianred2","pink1","white")),title="Per Person \nRevenue Loss ($)",bty='n',cex=.7)
dev.off()
file.copy('figures/revenue_loss_bystate_normalized.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/revenue_loss_bystate_normalized.pdf')

#####################################################################################################

#####################################################################################################
#Calculating revenue loss over time
cumulative.loss.byyear <- data.frame('mean.loss.bev'=colSums(all.revenues.decrease[[1]]),'mean.loss.phev10'=colSums(all.revenues.decrease[[2]]),'mean.loss.phev40'=colSums(all.revenues.decrease[[3]]),'q25.loss.bev'=colSums(all.revenues.decrease[[4]]),'q25.loss.phev10'=colSums(all.revenues.decrease[[5]]),'q25.loss.phev40'=colSums(all.revenues.decrease[[6]]),'q975.loss.bev'=colSums(all.revenues.decrease[[7]]),'q975.loss.phev10'=colSums(all.revenues.decrease[[8]]),'q975.loss.phev40'=colSums(all.revenues.decrease[[9]]))

#distributing losses evenly
stagger.add <- function(input,lifetime=12) {
	temp <- input/lifetime
	hold <- rep(0,length(temp))
	for(i in 1:lifetime) {
		hold <- temp+hold
		hold <- c(hold,0)
		temp <- c(0,temp)
	}
	hold <- hold[-length(hold)]
	return(hold)
}
annualized.totalloss <- lapply(cumulative.loss.byyear,stagger.add)
annualized.totalloss.all <- data.frame('mean.total'=annualized.totalloss[[1]]+annualized.totalloss[[2]]+annualized.totalloss[[3]],'q25.total'=annualized.totalloss[[4]]+annualized.totalloss[[5]]+annualized.totalloss[[6]],'q975.total'=annualized.totalloss[[7]]+annualized.totalloss[[8]]+annualized.totalloss[[9]])

#plotting total revenue decreases
pdf('figures/annual_revenueloss.pdf')
par(mar=c(6,4,2,2))
plot(2012:2025,annualized.totalloss[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss (millions of $)')
grid()
axis(1,las=0,tck=.02)
axis(2,las=2,tck=.02)
lines(2012:2025,annualized.totalloss.all[1:14,1]/10^6,lwd=2)
lines(2012:2025,annualized.totalloss.all[1:14,2]/10^6,lwd=1.5,lty=2)
lines(2012:2025,annualized.totalloss.all[1:14,3]/10^6,lwd=1.5,lty=2)
for(i in 1:length(annualized.totalloss)) {
	if(1<=i&i<=3) {
		line.type <- 1
		if(i%%3==1) {
			color.assignment <- 'red'
			lines(2012:2025,annualized.totalloss[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=2)
		}
		else if(i%%3==2) {
			color.assignment <- 'blue'
			lines(2012:2025,annualized.totalloss[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=2)
		}
		else {
			color.assignment <- 'forestgreen'
			lines(2012:2025,annualized.totalloss[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=2)
		}
	}
	else {
		line.type <- 2
		if(i%%3==1) {
			color.assignment <- 'red'
			lines(2012:2025,annualized.totalloss[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=1.5)
		}
		else if(i%%3==2) {
			color.assignment <- 'blue'
			lines(2012:2025,annualized.totalloss[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=1.5)
		}
		else {
			color.assignment <- 'forestgreen'
			lines(2012:2025,annualized.totalloss[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=1.5)
		}
	}
}
legend("topleft",c('Total','BEV','PHEV-10','PHEV-40'),lty=c(1,1,1,1),col=c('black','red','blue','forestgreen'),bg='white')
dev.off()
file.copy('figures/annual_revenueloss.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/annual_revenueloss.pdf')

#for 2x2 grid
pdf('figures/annual_revenueloss_total.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss.all[1:14,1]/10^6,lwd=2)
lines(2012:2025,annualized.totalloss.all[1:14,2]/10^6,lwd=1.5,lty=2)
lines(2012:2025,annualized.totalloss.all[1:14,3]/10^6,lwd=1.5,lty=2)
dev.off()
file.copy('figures/annual_revenueloss_total.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/annual_revenueloss_total.pdf')

pdf('figures/annual_revenueloss_bev.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss[[1]][1:14]/10^6,lwd=2,col='red')
lines(2012:2025,annualized.totalloss[[4]][1:14]/10^6,lwd=1.5,lty=2,col='red')
lines(2012:2025,annualized.totalloss[[7]][1:14]/10^6,lwd=1.5,lty=2,col='red')
dev.off()
file.copy('figures/annual_revenueloss_bev.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/annual_revenueloss_bev.pdf')

pdf('figures/annual_revenueloss_phev10.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss[[2]][1:14]/10^6,lwd=2,col='blue')
lines(2012:2025,annualized.totalloss[[5]][1:14]/10^6,lwd=1.5,lty=2,col='blue')
lines(2012:2025,annualized.totalloss[[8]][1:14]/10^6,lwd=1.5,lty=2,col='blue')
dev.off()
file.copy('figures/annual_revenueloss_phev10.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/annual_revenueloss_phev10.pdf')

pdf('figures/annual_revenueloss_phev40.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,cex.axis=1.5)
axis(2,las=2,cex.axis=1.5)
lines(2012:2025,annualized.totalloss[[3]][1:14]/10^6,lwd=2,col='forestgreen')
lines(2012:2025,annualized.totalloss[[6]][1:14]/10^6,lwd=1.5,lty=2,col='forestgreen')
lines(2012:2025,annualized.totalloss[[9]][1:14]/10^6,lwd=1.5,lty=2,col='forestgreen')
dev.off()
file.copy('figures/annual_revenueloss_phev40.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/annual_revenueloss_phev40.pdf')
#####################################################################################################

#####################################################################################################
#Sensitivity Analysis

#function to generate revenue from alternative policy: registration fee
alt1 <- function(input,msrp,percentage,lifetime) {
	out <- input
	out$Registration.Fees <- msrp*percentage*lifetime
	out$Total <- rowSums(out[,3:7])
	return(out)
}
alt1.leaf <- alt1(leaf,21300,.01,12)
alt1.volt <- alt1(volt,31645,.01,12)
alt1.priusphev <- alt1(prius.phev,32000,.01,12)

#function to generate revenue from alternative policy: use fee
alt2 <- function(input,electricmode.percent,vmt,lifetime,taxrate) {
	out <- input
	out$State.Fuel.Tax <- out$State.Fuel.Tax+lifetime*vmt*electricmode.percent*taxrate
	out$Total <- rowSums(out[,3:7])
	return(out)
}
alt2.leaf <- alt2(leaf,1,12000,12,.01)
alt2.volt <- alt2(volt,.64,12000,12,.01)
alt2.priusphev <- alt2(prius.phev,.288,12000,12,.01)

#sensitivity on registration fee
sensitivity1 <- function(input) {
	hold <- future.revenue.decrease(all.cases,alt1(leaf,21300,input,12)$Total,alt1(prius.phev,32000,input,12)$Total,alt1(volt,31645,input,12)$Total)
	out <- as.matrix(data.frame('mean.totalrev'=sum(hold[[1]],hold[[2]],hold[[3]]),'q25.totalrev'=sum(hold[[4]],hold[[5]],hold[[6]]),'q975.totalrev'=sum(hold[[7]],hold[[8]],hold[[9]])))
	return(out)
}
alt1.sensitivity <- sapply(seq(0,.01,by=.001),sensitivity1)

#sensitivity on use fee
sensitivity2 <- function(input) {
	hold <- future.revenue.decrease(all.cases,alt2(leaf,1,12000,12,input)$Total,alt2(prius.phev,.288,12000,12,input)$Total,alt2(volt,.64,12000,12,input)$Total)
	out <- as.matrix(data.frame('mean.totalrev'=sum(hold[[1]],hold[[2]],hold[[3]]),'q25.totalrev'=sum(hold[[4]],hold[[5]],hold[[6]]),'q975.totalrev'=sum(hold[[7]],hold[[8]],hold[[9]])))
	return(out)
}
alt2.sensitivity <- sapply(seq(0,.1,by=.01),sensitivity2)

#plot of sensitivity on registration fee
pdf('figures/registrationfee_sensitivity.pdf')
par(mar=c(8,8,1,1)+0.1)
plot(seq(0,.01,by=.001)*100,alt1.sensitivity[1,]/10^9,type="n",lwd=2.5,xlab="",ylab="",ylim=c(min(alt1.sensitivity/10^9),max(alt1.sensitivity/10^9)),xaxt='n',yaxt='n')
axis(1,cex.axis=1,las=0,tck=.02,cex.axis=2)
axis(2,cex.axis=1,las=2,tck=.02,cex.axis=2)
mtext('Annual Registration Fee \n Percentage of MSRP',side=1,line=5,cex=2)
mtext('Cumulative Revenue Decrease from \nEVs by 2025 (billions of dollars)',side=2,line=4,cex=2)
grid()
abline(h=0,col='red')
lines(seq(0,.01,by=.001)*100,alt1.sensitivity[1,]/10^9,lwd=2.5)
lines(c(0,0.01)*100,c(alt1.sensitivity[3,1],alt1.sensitivity[2,11])/10^9,lwd=2.5,lty=2)
lines(c(0,0.01)*100,c(alt1.sensitivity[2,1],alt1.sensitivity[3,11])/10^9,lwd=2.5,lty=2)
dev.off()
file.copy('figures/registrationfee_sensitivity.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/registrationfee_sensitivity.pdf')

#plot of sensitivity on use fee
pdf('figures/usefee_sensitivity.pdf')
par(mar=c(6,8,1,1)+0.1)
plot(seq(0,.1,by=.01)*100,alt2.sensitivity[1,]/10^9,type="n",lwd=2.5,xlab="",ylab="",ylim=c(min(alt2.sensitivity/10^9),max(alt2.sensitivity/10^9)),xaxt='n',yaxt='n')
axis(1,cex.axis=1,las=0,tck=.02,cex.axis=2)
axis(2,cex.axis=1,las=2,tck=.02,cex.axis=2)
mtext('Use Fee Tax (cents per mile)',side=1,line=3,cex=2)
mtext('Cumulative Revenue Decrease from \nEVs by 2025 (billions of dollars)',side=2,line=4,cex=2)
grid()
abline(h=0,col='red')
lines(seq(0,.1,by=.01)*100,alt2.sensitivity[1,]/10^9,lwd=2.5)
lines(c(0,0.1)*100,c(alt2.sensitivity[3,1]/10^9,alt2.sensitivity[2,11]/10^9),lwd=2.5,lty=2)
lines(c(0,0.1)*100,c(alt2.sensitivity[2,1]/10^9,alt2.sensitivity[3,11]/10^9),lwd=2.5,lty=2)
dev.off()
file.copy('figures/usefee_sensitivity.pdf','~/Dropbox/CarnegieMellon/Dissertation/chapters/chapter2/figures/usefee_sensitivity.pdf')

###
#upper-bound analysis
###

epri.nrdc.2007 <- eia.2013
epri.nrdc.2007$BEV100 <- epri.nrdc.2007$BEV*forecasts2$EPRI.NRDC2007[5:18]/forecasts2$AEO2013[5:18]
epri.nrdc.2007$PHEV10 <- epri.nrdc.2007$PHEV10*forecasts2$EPRI.NRDC2007[5:18]/forecasts2$AEO2013[5:18]
epri.nrdc.2007$PHEV40 <- epri.nrdc.2007$PHEV40*forecasts2$EPRI.NRDC2007[5:18]/forecasts2$AEO2013[5:18]
epri.nrdc.2007$FFV <- epri.nrdc.2007$FFV*forecasts2$EPRI.NRDC2007[5:18]/forecasts2$AEO2013[5:18]


future.sales.bystate.upper <- function() {
	#states in each region
	pacific.states <- c('ALASKA','WASHINGTON','OREGON','CALIFORNIA','HAWAII')
	mountain.states <- c('MONTANA','IDAHO','NEVADA','UTAH','WYOMING','COLORADO','ARIZONA','NEW.MEXICO')
	west.north.central.states <- c('NORTH.DAKOTA','MINNESOTA','SOUTH.DAKOTA','NEBRASKA','KANSAS','IOWA','MISSOURI')
	west.south.central.states <- c('TEXAS','OKLAHOMA','ARKANSAS','LOUISIANA')
	east.north.central.states <- c('WISCONSIN','MICHIGAN','ILLINOIS','INDIANA','OHIO')
	east.south.central.states <- c('KENTUCKY','TENNESSEE','MISSISSIPPI','ALABAMA')
	south.atlantic.states <- c('WEST.VIRGINIA','MARYLAND','DELAWARE','VIRGINIA','NORTH.CAROLINA','SOUTH.CAROLINA','GEORGIA','FLORIDA')
	middle.atlantic.states <- c('PENNSYLVANIA','NEW.YORK','NEW.JERSEY')
	newengland.states <- c('MAINE','VERMONT','NEW.HAMPSHIRE','MASSACHUSETTS','RHODE.ISLAND','CONNECTICUT')
	#creating empty arrays for final list output
	BEV100.out <- array(dim=c(nrow(epri.nrdc.2007),50,nrow(prius.sales)))
	PHEV10.out <- array(dim=c(nrow(epri.nrdc.2007),50,nrow(prius.sales)))
	PHEV40.out <- array(dim=c(nrow(epri.nrdc.2007),50,nrow(prius.sales)))
	FFV.out <- array(dim=c(nrow(epri.nrdc.2007),50,nrow(prius.sales)))
	for(j in 1:nrow(prius.sales)) {
		hold.sales <- prius.sales[j,]
		#creating empty matrices to be filled, one for each row of prius sales
		BEV100.bystate <- matrix(numeric(0),nrow=nrow(epri.nrdc.2007))
		PHEV10.bystate <- matrix(numeric(0),nrow=nrow(epri.nrdc.2007))
		PHEV40.bystate <- matrix(numeric(0),nrow=nrow(epri.nrdc.2007))
		FFV.bystate <- matrix(numeric(0),nrow=nrow(epri.nrdc.2007))
		for(i in region.list) {
			#takes prius sales and keeps only states within the region
			hold.statesales <- hold.sales[colnames(hold.sales)%in%get(paste(i,'.states',sep=''))]
			#transformation of the sales to the proportional distribution in each region
			hold.statetotal <- sum(hold.statesales)
			hold.statedist <- hold.statesales/hold.statetotal
			for(k in colnames(epri.nrdc.2007[-1])) {
				#gives table of sales by state for each technology
				assign(paste('hold.',k,'.bystate',sep=''),(as.matrix('[['(get(i),k),ncol=1)%*%as.matrix(hold.statedist,nrow=1)))
				#appends the table to the matrix holding all other regions that have been run
				assign(paste(k,'.bystate',sep=''),cbind(get(paste(k,'.bystate',sep='')),get(paste('hold.',k,'.bystate',sep=''))))
			}
		}
		#assigns completed table (of each technology sales across 50 states) to the empty array
		BEV100.out[,,j] <- BEV100.bystate
		PHEV10.out[,,j] <- PHEV10.bystate
		PHEV40.out[,,j] <- PHEV40.bystate
		FFV.out[,,j] <- FFV.bystate
	}
	state.order <<- colnames(BEV100.bystate)
	out <- list(BEV100.out,PHEV10.out,PHEV40.out,FFV.out)
	return(out)
}
projectedsales.bystate.upper <- future.sales.bystate.upper()

for(i in 1:length(projectedsales.bystate.upper)){
	for(j in 1:dim(projectedsales.bystate.upper[[i]])[3])
		for(k in 1:ncol(projectedsales.bystate.upper[[i]][,,j]))
		projectedsales.bystate.upper[[i]][,k,j] <- projectedsales.bystate.upper[[i]][,k,j]*forecasts2$EPRI.NRDC2007[5:18]/forecasts2$AEO2013[5:18]
}

state.order <- tolower(state.order)
state.order <- gsub('\\.',' ',state.order)

for(i in 1:ncol(epri.nrdc.2007[-1])) {
	assign(paste('mean.projected',colnames(epri.nrdc.2007[i+1]),'.bystate.upper',sep=''),as.data.frame(apply(projectedsales.bystate.upper[[i]],c(1,2),mean)))
	assign(paste('q25.projected',colnames(epri.nrdc.2007[i+1]),'.bystate.upper',sep=''),apply(projectedsales.bystate.upper[[i]],c(1,2),quantile,probs=.025))
	assign(paste('q975.projected',colnames(epri.nrdc.2007[i+1]),'.bystate.upper',sep=''),apply(projectedsales.bystate.upper[[i]],c(1,2),quantile,probs=.975))
}
all.cases.upper <- list(mean.projectedBEV100.bystate.upper, mean.projectedPHEV10.bystate.upper, mean.projectedPHEV40.bystate.upper, mean.projectedFFV.bystate.upper, q25.projectedBEV100.bystate.upper, q25.projectedPHEV10.bystate.upper, q25.projectedPHEV40.bystate.upper, q25.projectedFFV.bystate.upper, q975.projectedBEV100.bystate.upper, q975.projectedPHEV10.bystate.upper, q975.projectedPHEV40.bystate.upper, q975.projectedFFV.bystate.upper)
name.it <- function(table) {
	colnames(table) <- state.order
	table <- table[,order(colnames(table))]
	return(table)
}
all.cases.upper <- lapply(all.cases.upper,name.it)
#####################################################################################################

#####################################################################################################
#Calculating projected revenue losses
#list of revenue by year, state, and technology and distribution (row=state, column=year, list num=technology and dist: by 3's BEV, PHEV10, PHEV40, sets of 3 mean, q25, q975)
all.revenues.decrease.upper <- future.revenue.decrease(all.cases.upper,leaf$Total,prius.phev$Total,volt$Total)

#####################################################################################################

#####################################################################################################
#State level map (only needs state and technology)
#data reduction to state and technology (use only list 1-3, sum columns)
total.state.revenueloss.upper <- data.frame('bev.loss'=rowSums(all.revenues.decrease.upper[[1]]),'phev10.loss'=rowSums(all.revenues.decrease.upper[[2]]),'phev40.loss'=rowSums(all.revenues.decrease.upper[[3]]))
total.state.revenueloss.upper$totalloss <- rowSums(total.state.revenueloss.upper)
total.state.revenueloss.upper$state <- rownames(total.state.revenueloss.upper)

#assigning colors by quantiles of loss
loss.quantiles.upper <- quantile(total.state.revenueloss.upper$totalloss ,probs=c(.2,.4,.6,.8))
for(i in 1:nrow(total.state.revenueloss.upper)) {
	if(total.state.revenueloss.upper$totalloss [i]>loss.quantiles.upper[4]) {
		total.state.revenueloss.upper$color[i] <- "red4"
	}
	else if(total.state.revenueloss.upper$totalloss [i]>loss.quantiles.upper[3]) {
		total.state.revenueloss.upper$color[i] <- "red1"
	}
	else if(total.state.revenueloss.upper$totalloss [i]>loss.quantiles.upper[2]) {
		total.state.revenueloss.upper$color[i] <- "indianred2"
	}
	else if(total.state.revenueloss.upper$totalloss [i]>loss.quantiles.upper[1]) {
		total.state.revenueloss.upper$color[i] <- "pink1"
	}
	else {
		total.state.revenueloss.upper$color[i] <- "white"
	}
}

pdf('figures/upperbound_revenue_loss_bystate.pdf')
map("state",fill=TRUE,col=total.state.revenueloss.upper$color[match(mapnames.state,total.state.revenueloss.upper$state)])
legend("bottomright",leg=c(as.expression(paste("< ",abs(signif(loss.quantiles.upper[1]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles.upper[1]/10^6,2))," - ",abs(signif(loss.quantiles.upper[2]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles.upper[2]/10^6,2))," - ",abs(signif(loss.quantiles.upper[3]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles.upper[3]/10^6,2))," - ",abs(signif(loss.quantiles.upper[4]/10^6,2)))),
	as.expression(paste(abs(signif(loss.quantiles.upper[4]/10^6,2))," - ",abs(signif(max(total.state.revenueloss.upper$totalloss)/10^6,2))))),
	fill=rev(c("red4","red1","indianred2","pink1","white")),title="Revenue Loss \n(millions of $)",bty='n',cex=.7)
dev.off()
#####################################################################################################

#####################################################################################################
#Calculating revenue loss over time
cumulative.loss.byyear.upper <- data.frame('mean.loss.bev'=colSums(all.revenues.decrease.upper[[1]]),'mean.loss.phev10'=colSums(all.revenues.decrease.upper[[2]]),'mean.loss.phev40'=colSums(all.revenues.decrease.upper[[3]]),'q25.loss.bev'=colSums(all.revenues.decrease.upper[[4]]),'q25.loss.phev10'=colSums(all.revenues.decrease.upper[[5]]),'q25.loss.phev40'=colSums(all.revenues.decrease.upper[[6]]),'q975.loss.bev'=colSums(all.revenues.decrease.upper[[7]]),'q975.loss.phev10'=colSums(all.revenues.decrease.upper[[8]]),'q975.loss.phev40'=colSums(all.revenues.decrease.upper[[9]]))

annualized.totalloss.upper <- lapply(cumulative.loss.byyear.upper,stagger.add)
annualized.totalloss.all.upper <- data.frame('mean.total'=annualized.totalloss.upper[[1]]+annualized.totalloss.upper[[2]]+annualized.totalloss.upper[[3]],'q25.total'=annualized.totalloss.upper[[4]]+annualized.totalloss.upper[[5]]+annualized.totalloss.upper[[6]],'q975.total'=annualized.totalloss.upper[[7]]+annualized.totalloss.upper[[8]]+annualized.totalloss.upper[[9]])

#plotting total revenue decreases
pdf('figures/upperbound_annual_revenueloss.pdf')
par(mar=c(6,4,2,2))
plot(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all.upper)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss (millions of $)')
grid()
axis(1,las=0,tck=.02)
axis(2,las=2,tck=.02)
lines(2012:2025,annualized.totalloss.all.upper[1:14,1]/10^6,lwd=2)
lines(2012:2025,annualized.totalloss.all.upper[1:14,2]/10^6,lwd=1.5,lty=2)
lines(2012:2025,annualized.totalloss.all.upper[1:14,3]/10^6,lwd=1.5,lty=2)
for(i in 1:length(annualized.totalloss.upper)) {
	if(1<=i&i<=3) {
		line.type <- 1
		if(i%%3==1) {
			color.assignment <- 'red'
			lines(2012:2025,annualized.totalloss.upper[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=2)
		}
		else if(i%%3==2) {
			color.assignment <- 'blue'
			lines(2012:2025,annualized.totalloss.upper[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=2)
		}
		else {
			color.assignment <- 'forestgreen'
			lines(2012:2025,annualized.totalloss.upper[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=2)
		}
	}
	else {
		line.type <- 2
		if(i%%3==1) {
			color.assignment <- 'red'
			lines(2012:2025,annualized.totalloss.upper[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=1.5)
		}
		else if(i%%3==2) {
			color.assignment <- 'blue'
			lines(2012:2025,annualized.totalloss.upper[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=1.5)
		}
		else {
			color.assignment <- 'forestgreen'
			lines(2012:2025,annualized.totalloss.upper[[i]][1:14]/10^6,col=color.assignment,lty=line.type,lwd=1.5)
		}
	}
}
legend("topleft",c('Total','BEV','PHEV-10','PHEV-40'),lty=c(1,1,1,1),col=c('black','red','blue','forestgreen'),bg='white')
dev.off()

#for 2x2 grid
pdf('figures/upperbound_annual_revenueloss_total.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all.upper)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss.all.upper[1:14,1]/10^6,lwd=2)
lines(2012:2025,annualized.totalloss.all.upper[1:14,2]/10^6,lwd=1.5,lty=2)
lines(2012:2025,annualized.totalloss.all.upper[1:14,3]/10^6,lwd=1.5,lty=2)
dev.off()

pdf('figures/upperbound_annual_revenueloss_bev.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all.upper)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^6,lwd=2,col='red')
lines(2012:2025,annualized.totalloss.upper[[4]][1:14]/10^6,lwd=1.5,lty=2,col='red')
lines(2012:2025,annualized.totalloss.upper[[7]][1:14]/10^6,lwd=1.5,lty=2,col='red')
dev.off()

pdf('figures/upperbound_annual_revenueloss_phev10.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all.upper)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss.upper[[2]][1:14]/10^6,lwd=2,col='blue')
lines(2012:2025,annualized.totalloss.upper[[5]][1:14]/10^6,lwd=1.5,lty=2,col='blue')
lines(2012:2025,annualized.totalloss.upper[[8]][1:14]/10^6,lwd=1.5,lty=2,col='blue')
dev.off()

pdf('figures/upperbound_annual_revenueloss_phev40.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^6,ylim=c(0,max(annualized.totalloss.all.upper)/10^6),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(millions of $)',cex.lab=2)
grid()
axis(1,las=0,cex.axis=1.5)
axis(2,las=2,cex.axis=1.5)
lines(2012:2025,annualized.totalloss.upper[[3]][1:14]/10^6,lwd=2,col='forestgreen')
lines(2012:2025,annualized.totalloss.upper[[6]][1:14]/10^6,lwd=1.5,lty=2,col='forestgreen')
lines(2012:2025,annualized.totalloss.upper[[9]][1:14]/10^6,lwd=1.5,lty=2,col='forestgreen')
dev.off()

pdf('figures/upperbound_annual_total_comparison.pdf')
par(mar=c(6,8,2,2))
plot(2012:2025,annualized.totalloss.upper[[1]][1:14]/10^9,ylim=c(0,max(annualized.totalloss.all.upper)/10^9),type='n',xaxt='n',yaxt='n',xlab='Year',ylab='Annual Revenue Loss\n(billions of $)',cex.lab=2)
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(2012:2025,annualized.totalloss.all.upper[1:14,1]/10^9,lwd=2,col='blue4')
lines(2012:2025,annualized.totalloss.all.upper[1:14,2]/10^9,lwd=1.5,lty=2,col='blue4')
lines(2012:2025,annualized.totalloss.all.upper[1:14,3]/10^9,lwd=1.5,lty=2,col='blue4')
text(x=2023,y=.9,pos=2,labels='EPRI/NRDC EV Sales')
lines(2012:2025,annualized.totalloss.all[1:14,1]/10^9,lwd=2)
lines(2012:2025,annualized.totalloss.all[1:14,2]/10^9,lwd=1.5,lty=2)
lines(2012:2025,annualized.totalloss.all[1:14,3]/10^9,lwd=1.5,lty=2)
text(x=2024.5,y=.03,pos=2,labels='AEO2013 EV Sales')
dev.off()
#####################################################################################################
