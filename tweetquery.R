## Society of Freshwater Science
##
## Social media informatics
##
## Code was compiled by Paul Julian
## contact info: pauljulianPhD@gmail.com

# Some helpful sites
#https://rtweet.info
#https://www.johnlittle.info/project/custom/rtweet/network_graph_more_examples.nb.html
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#http://www.rdatamining.com/docs/twitter-analysis-with-r

#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

library(rtweet)
library(plyr)
library(httpuv)
library(AnalystHelper)

setwd("D:/_GitHub/SFSPosterUp")
plot.path=paste0(getwd(),"/plots/")

sfs.cols=c(rgb(131/255,196/255,59/255,1,names="SFS.green"),rgb(162/255,228/255,249/255,1,names="SFS.ltblue"),rgb(40/255,152/255,179/255,1,names="SFS.blue"))

# data --------------------------------------------------------------------

#rt.sfs=search_tweets("#2019SFS OR #SFS2019",include_rts = TRUE,lang = "en",type = "mixed",n=20000)
rt.sfs=search_tweets2("#2019SFS",include_rts = TRUE,lang = "en",type = "mixed",n=20000)
rt.sfsposter=search_tweets2("#2019SFSPosterUp OR #2019SFSPostUp",include_rts = TRUE,lang = "en",type = "mixed",n=20000)

tsplot.dat.sfs=ts_plot(rt.sfs,"1 hours")
tsplot.dat.sfs.post=ts_plot(rt.sfsposter,"1 hours")

tot.dat=ts_plot(rt.sfsposter,"1 days")
tot.dat$data

cum.dat.sfs=data.frame(tsplot.dat.sfs$data)
cum.dat.sfs$cum_count=cumsum(cum.dat.sfs$n)
cum.dat.sfs$time=date.fun(cum.dat.sfs$time,form="%F %X",tz="UTC")

cum.dat.sfs.post=data.frame(tsplot.dat.sfs.post$data)
cum.dat.sfs.post$cum_count=cumsum(cum.dat.sfs.post$n)
cum.dat.sfs.post$time=date.fun(cum.dat.sfs.post$time,form="%F %X",tz="UTC")

xlim.val=date.fun(c("2019-05-17 05:00:00",as.character(Sys.time()+ddays(1))),form="%F %X",tz="UTC")
xmaj=seq(xlim.val[1],xlim.val[2],"24 hours");xmin=seq(xlim.val[1],xlim.val[2],"12 hours")
#png(filename=paste0(plot.path,format(Sys.Date(),"%Y"),format(Sys.Date(),"%m"),format(Sys.Date(),"%d"),"_tweetstas.png"),width=6,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(1.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(1:2,1,2,byrow = T))

ylim.val=c(0,3000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cum_count~time,cum.dat.sfs,xlim=xlim.val,ylim=ylim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cum.dat.sfs,time!=date.fun(Sys.Date(),form="%F",tz="UTC")),shaded.range(time,rep(0,length(time)),cum_count,sfs.cols[1],lty=1))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%b-%d"))
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Cumulative Total")
mtext(side=3,expression(italic("#2019SFS")))
#legend("topleft",legend=c(expression(italic("#2019SFS")),expression(italic("#2019SFSPostUp"))),pch=22,pt.bg=adjustcolor(sfs.cols[1:2],0.5),pt.cex=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
text(x=xlim.val[1],y=ylim.val[2]-100,"Source: Data collected from Twitter's REST API via rtweet",col="red",font=3,adj=0,xpd=NA,cex=0.5)

ylim.val=c(0,350);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cum_count~time,cum.dat.sfs,xlim=xlim.val,ylim=ylim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cum.dat.sfs.post,time!=date.fun(Sys.Date(),form="%F",tz="UTC")),shaded.range(time,rep(0,length(time)),cum_count,sfs.cols[3],lty=1))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%b-%d"))
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,expression(italic("#2019SFSPostUp")))
mtext(side=1,outer=T,"Date (Month-Day)")

dev.off()

rt.sfsposter$date=date.fun(rt.sfsposter$created_at,form="%F",tz="UTC")
#posters=data.frame(subset(rt.sfsposter,is_retweet=="FALSE"&media_type=="photo"))
posters=data.frame(subset(rt.sfsposter,media_type=="photo"&date==date.fun("2019-05-22",tz="UTC")))

#for(i in 1:nrow(posters)){
#  download.file(posters$media_url[[i]],
#                paste0("D:/_GitHub/SFSPosterUp/2019SFSPostUp_Posters/",i,"_",posters$screen_name[i],".jpg"),
#                mode="wb")
#}



# SFS2019 versus 2019SFS --------------------------------------------------

rt.2019sfs=search_tweets2("#2019SFS",include_rts = TRUE,lang = "en",type = "mixed",n=20000,retryonratelimit = T)
rt.2019sfs$Hashtag="#2019SFS"
rt.sfs2019=search_tweets("#SFS2019",include_rts = TRUE,lang = "en",type = "mixed",n=20000)
rt.sfs2019$Hashtag="#SFS2019"

rt.2019SFS.agg=data.frame(ts_plot(rt.2019sfs,"1 days")$data)
rt.SFS2019.agg=data.frame(ts_plot(rt.sfs2019,"1 days")$data)

xlim.val=date.fun(c("2019-05-09",as.character(Sys.Date()+ddays(1))),form="%F",tz="UTC")
xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

#png(filename=paste0(plot.path,format(Sys.Date(),"%Y"),format(Sys.Date(),"%m"),format(Sys.Date(),"%d"),"_2019SFS_v_SFS2019.png"),width=5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(1.5,2,1,0.25),mar=c(1.5,2,0.5,1))
plot(n~time,rt.2019SFS.agg,xlim=xlim.val,ylim=ylim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(rt.2019SFS.agg,time!=date.fun(Sys.Date(),form="%F",tz="UTC")),pt_line(time,n,2,sfs.cols[1],2,21,sfs.cols[1],cex=1.5,pt.lwd=0.1))
with(subset(rt.SFS2019.agg,time!=date.fun(Sys.Date(),form="%F",tz="UTC")),pt_line(time,n,2,sfs.cols[2],2,23,sfs.cols[2],cex=1.5,pt.lwd=0.1))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%b-%d"))
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Date (Month-Day)")
mtext(side=2,line=2.5,"Daily Count")
legend("topleft",legend=c(expression(italic("#2019SFS")),expression(italic("#SFS2019"))),pch=c(21,23),lwd=2,pt.bg=sfs.cols[1:2],lty=2,col=sfs.cols[1:2],pt.cex=1.25,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend("topleft",legend=c(expression(italic("#2019SFS")),expression(italic("#SFS2019"))),pch=c(21,23),lty=0,lwd=0.1,pt.bg=sfs.cols[1:2],pt.cex=1.25,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
text(x=xlim.val[2],y=ylim.val[2],"Source: Data collected from\nTwitter's REST API via rtweet",col="red",font=3,adj=1,xpd=NA,cex=0.5)
dev.off()


## GGPlot version (experimenting)
library(ggplot2)

ggplot()+
  geom_line(col=sfs.cols[1],lwd=2,lty=2,data=rt.2019SFS.agg,aes(x=time,y=n))+geom_point(bg=sfs.cols[1],size=4,pch=21,data=rt.2019SFS.agg,aes(x=time,y=n))+
  geom_line(col=sfs.cols[2],lwd=2,lty=2,data=rt.SFS2019.agg,aes(x=time,y=n))+geom_point(bg=sfs.cols[2],size=4,pch=23,data=rt.SFS2019.agg,aes(x=time,y=n))+
  scale_color_manual(name="Twitter Hashtags",
                     limits=c("#2019SFS","#SFS2019"),
                      values=sfs.cols)+
  guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
  theme_bw()
