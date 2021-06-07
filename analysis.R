
# reproduction results plotting

#setwd(...)

library(dplyr)
library(ggplot2)
library(reshape2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))


#res <- as.tbl(read.csv(file='exploration/2017_03_24_15_50_31_REPRO_EXPERIMENT.csv',skip = 1,header = FALSE,sep = ','))
#names(res)<-c("disallowedRatio",paste0(1:225,"finalEconomicStatus"),"id",paste0(1:225,"initialEconomicStatus"),"logBetaMove","replication","segregation","sigmaWealth","winc")

res <- as.tbl(read.csv(file='exploration/2017_03_23_15_34_36_GRID.csv',header = T,sep = ','))

bypatchfinal = as.tbl(melt(res,id.vars = c(1,227,453,454,455,456,457),measure.vars = c(2:226)))
bypatchinitial = as.tbl(melt(res,id.vars = c(1,227,453,454,455,456,457),measure.vars = c(228:452)))

bypatch = as.tbl(cbind(bypatchfinal[,-8],bypatchinitial[,9]))
names(bypatch)[8:9]<-c("finalES","initialES")
#bypatch = left_join(bypatchfinal,bypatchinitial,by=c('id','replication'))


g=ggplot(bypatch[bypatch$logBetaMove%in%seq(from=-3,to=5,by=1),],aes(x=initialES,y=finalES,color=segregation))
g+geom_point(size=0.5)+facet_grid(sigmaWealth~logBetaMove)

sres <- bypatch %>% group_by(id) %>% summarise(disallowedRatio=mean(disallowedRatio),logBetaMove=mean(logBetaMove),segregation=mean(segregation),sigmaWealth=mean(sigmaWealth),winc=mean(winc),initialES=mean(initialES),finalES=mean(finalES))

g=ggplot(sres,aes(x=disallowedRatio,y=segregation,color=as.character(sigmaWealth)))
g+geom_point()


####
## res retaining status

resTransitionMatrix <- function(initial,final,q){
  # q : quantile step
  initquantiles = quantile(initial,seq(from=q,to=1,by=q))
  finalquantiles = quantile(final,seq(from=q,to=1,by=q))
  resinit = sapply(initial,function(x){which(x<=initquantiles&x>c(-1,initquantiles[1:(length(initquantiles)-1)]))})
  resfinal =sapply(final,function(x){which(x<=finalquantiles&x>c(-1,finalquantiles[1:(length(finalquantiles)-1)]))})
  res = matrix(0,length(initquantiles),length(initquantiles))
  #for(i in 1:nrow(res)){for(j in 1:ncol(res)){res[i,j]=length(which(resinit==(nrow(res)-i+1)&resfinal==j))/length(initial)}}
  for(i in 1:length(resinit)){res[resinit[i],resfinal[i]]=res[resinit[i],resfinal[i]]+1}
  return(res/length(initial))
}

#bypatch %>% group_by(id,replication) %>% summarise(count=n())
resTransitionMatrix(bypatch$initialES[bypatch$id==0&bypatch$replication==1885040710],bypatch$finalES[bypatch$id==0&bypatch$replication==1885040710],0.25)

#bypatch %>% group_by(id,replication) %>% summarise(transmat= resTransitionMatrix(initialES,finalES,0.25))

transmats=list()
for(id in unique(bypatch$id)){show(id);for(replication in unique(bypatch$replication[bypatch$id==id])){
  transmats[[paste0(id,"-",replication)]]=resTransitionMatrix(bypatch$initialES[bypatch$id==id&bypatch$replication==replication],bypatch$finalES[bypatch$id==id&bypatch$replication==replication],0.1)
}}

# take average on all ids
meantransmats=list()
dftransmats=data.frame()
for(id in unique(bypatch$id)){show(id);for(replication in unique(bypatch$replication[bypatch$id==id])){
    if(!(as.character(id)%in%names(meantransmats))){meantransmats[[as.character(id)]]=matrix(0,nrow(transmats[[1]]),ncol(transmats[[1]]))}
    meantransmats[[as.character(id)]] = meantransmats[[as.character(id)]] + transmats[[paste0(id,"-",replication)]]
  }
  meantransmats[[as.character(id)]]=meantransmats[[as.character(id)]]/length(unique(bypatch$replication[bypatch$id==id]))
  dftransmats=rbind(dftransmats,cbind(melt(meantransmats[[as.character(id)]],varnames = c("init","final"),value.name = "rate"),id=rep(id,nrow(transmats[[1]])*ncol(transmats[[1]]))))
}


g=ggplot(left_join(dftransmats,bypatch%>%group_by(id)%>%summarise(logBetaMove=mean(logBetaMove),sigmaWealth=mean(sigmaWealth)))%>%filter(logBetaMove%in%seq(from=-3,to=5,by=1)),aes(x=init,y=final,fill=rate))
g+geom_raster()+facet_grid(sigmaWealth~logBetaMove)


############
############

# grid exploration of extended parameter space
sres = res %>% group_by(id)%>%summarise(disallowedRatio=mean(disallowedRatio),segregation=mean(segregation),logBetaMove=mean(logBetaMove),sigmaWealth=mean(sigmaWealth),winc=mean(winc))

g=ggplot(sres,aes(x=logBetaMove,y=segregation))
g+geom_point()+geom_line()+facet_grid(sigmaWealth~winc)

g=ggplot(sres,aes(x=logBetaMove,y=segregation,color=sigmaWealth,group=sigmaWealth))
g+geom_line()+facet_wrap(~winc)+stdtheme+ theme(legend.position=c(0.9, 0.1))
ggsave(file='res/segreg-logbetamove_facet-sigma-winc.png',width=30,height=20,units='cm')

g=ggplot(sres[sres$winc==3,],aes(x=logBetaMove,y=segregation,color=sigmaWealth,group=sigmaWealth))
g+geom_line()+stdtheme
ggsave(file='res/segreg-logbetamove_facet-sigma_winc3.png',width=25,height=20,units='cm')







