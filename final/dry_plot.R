d <- read.csv("final/dryspell.csv")
d <- d[match(c("wet","dry 1-3","dry 4-7","dry 8+"),d$band),]
png("final/dry_chart.png",width=1400,height=670,res=150)
par(mar=c(4,4,1,1))
barplot(rbind(d$coverage5,d$coverage20),beside=TRUE,names.arg=c("Wet day","Dry days 1-3","Dry days 4-7","Dry day 8+"),col=c("#96BAC0","#1C576C"),border=NA,ylim=c(0,100),ylab="Demand covered (%)")
legend("topright",legend=c("5 cubic metres","20 cubic metres"),fill=c("#96BAC0","#1C576C"),bty="n")
dev.off()