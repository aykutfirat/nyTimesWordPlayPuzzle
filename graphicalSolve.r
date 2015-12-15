with({g<-list(title=unique(tolower(strsplit(“HowTheLowlyCyborgsWon”,””)[[1]])),
story=lapply(strsplit(c(‘OhGoshShowCogsHow’,
‘WhenTheChosenOneChewsGhostlyCowHerb’,
‘HogsGown’,’BlottyBony’,’GelLegCore’,
‘RetoolBetter’,’SnobbyBoots’,
‘HerRococoEgo’,’TrollRobotry’,
‘RebelCoercer’,’EchoHogWoe’,’WhySonWhy’,
‘StonyTownsSoonSwoon’),””),function(y) unique(tolower(y))));
qList<<-NULL;
dt<-data.frame(
x=c(1,2,2,2,2,3,3,3,4,4,4,4,5),
y=c(4,1,3,5,7,2,4,6,1,3,5,7,4),
l=strsplit(‘YSNTBWOLHGERC’,”)[[1]])},
z<-lapply(lapply(strsplit(
grep(‘([technologybeclothescogwheeler])\\1′,
grep(‘^[chewcowherbstrongly]{6}$’,readLines(‘/usr/share/dict/words’),value=T),value=T),”),
function(x,y=x) if(length(x)==1) return(T) else return(ifelse(x[2] %in% g$s[[match(x[1],g$t)]] & sys.function(0)(x[-1],y=x),ifelse(length(x)==length(y),return(x),T),F))),
function(q) {require(ggplot2)
if(length(q)!=6) return(NULL)
ind<-match(q,tolower(dt$l))
qList<<-c(qList,paste(q,collapse=””));
p<-ggplot(data=data.frame(dt))+geom_text(aes(x=x,y=y,label=l))+geom_point(aes(x=x,y=y,label=l),shape=1,size=10)+labs(title=toupper(paste(q,collapse=””)),x=””,y=””)+
theme(legend.position=”none”,panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.background = element_blank())
print(p+geom_segment(data=data.frame(x=c(rep(1,6),rep(2,12),3,rep(4,3)),y=c(rep(4,6),1,1,3,1,3,5,7,5,7,5,1,7,2,1,1,7),xend=c(4,5,2,2,4,2,5,4,4,2,4,2,4,4,5,4,4,4,3,4,5,5),yend=c(7,4,3,7,1,1,4,1,5,7,3,4,7,3,4,5,7,1,6,7,4,4)),aes(x=x,y=y,xend=xend,yend=yend),size=0.1,color=”red”,linetype=2)+
geom_path(data=data.frame(dt[ind,]),aes(x=x,y=y,color=”red”),size=1)+geom_text(data=data.frame(x=c(rep(0.5,33),rep(5.5,33)), y=rep(seq(7,1.2,-0.18),2),l=c(qList,rep(“”,66-length(qList)))),aes(x=x,y=y,label=l),size=3)+geom_text(data=data.frame(dt[ind,]),aes(x=x-0.1,y=y+seq(-0.25,0.25,0.1),label=1:6,size=1)))
Sys.sleep(0)}))
