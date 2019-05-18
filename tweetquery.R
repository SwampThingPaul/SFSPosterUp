#https://rtweet.info
#https://www.johnlittle.info/project/custom/rtweet/network_graph_more_examples.nb.html
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/

#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

library(rtweet)
library(httpuv)
library(AnalystHelper)

setwd("D:/_GitHub/SFSPosterUp")
plot.path=paste0(getwd(),"/plots/")
# data --------------------------------------------------------------------

#rt.sfs=search_tweets("#2019SFS OR #SFS2019",include_rts = TRUE,lang = "en",type = "mixed",n=20000)
rt.sfs=search_tweets("#2019SFS",include_rts = TRUE,lang = "en",type = "mixed",n=20000)
rt.sfsposter=search_tweets("#2019SFSPostUp",include_rts = TRUE,lang = "en",type = "mixed",n=20000)

tsplot.dat.sfs=ts_plot(rt.sfs,"1 hours")
tsplot.dat.sfs.post=ts_plot(rt.sfsposter,"1 hours")

cum.dat.sfs=data.frame(tsplot.dat.sfs$data)
cum.dat.sfs$cum_count=cumsum(cum.dat.sfs$n)
cum.dat.sfs$time=date.fun(cum.dat.sfs$time,form="%F %X",tz="UTC")

cum.dat.sfs.post=data.frame(tsplot.dat.sfs.post$data)
cum.dat.sfs.post$cum_count=cumsum(cum.dat.sfs.post$n)
cum.dat.sfs.post$time=date.fun(cum.dat.sfs.post$time,form="%F %X",tz="UTC")

xlim.val=date.fun(c("2019-05-17 05:00:00",as.character(Sys.time()+ddays(1))),form="%F %X",tz="America/New_York")
xmaj=seq(xlim.val[1],xlim.val[2],"24 hours");xmin=seq(xlim.val[1],xlim.val[2],"12 hours")
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

png(filename=paste0(plot.path,format(Sys.Date(),"%Y"),format(Sys.Date(),"%m"),format(Sys.Date(),"%d"),"_tweetstas.png"),width=5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(1.5,2,1,0.25),mar=c(1.5,2,0.5,1))
plot(cum_count~time,cum.dat.sfs,xlim=xlim.val,ylim=ylim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(cum.dat.sfs,shaded.range(time,rep(0,nrow(cum.dat.sfs)),cum_count,"grey",lty=1))
with(cum.dat.sfs.post,shaded.range(time,rep(0,nrow(cum.dat.sfs.post)),cum_count,"indianred1",lty=1))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%b-%d %H:%M"))
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Date (Month-Day Hour (UTC))")
mtext(side=2,line=2,"Cumulative Total")
legend("topleft",legend=c(expression(italic("#2019SFS")),expression(italic("#2019SFSPosterUp"))),pch=22,pt.bg=adjustcolor(c("grey","indianred1"),0.5),pt.cex=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
text(x=xlim.val[2],y=ylim.val[2]-25,"Source: Data collected from\nTwitter's REST API via rtweet",col="red",font=3,adj=1,xpd=NA,cex=0.5)
dev.off()

