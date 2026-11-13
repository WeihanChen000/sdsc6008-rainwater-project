# Additional exploratory decisions following the primary analysis. Run from project root.
if(!exists("retain_rain")) source("run_groupwork.R")
capacities <- sort(unique(c(5:20,12.5)))
detail <- list(); dry_results <- list(); k <- 0
for (y in 2021:2024) {
 w <- weather[weather$year %in% c(y-1,y),]; use <- w$year==y
 inflow <- .8*500*retain_rain(w$rain,.75)/1000
 streak <- numeric(nrow(w))
 for (i in seq_along(streak)) if(w$rain[i]<.1)
   streak[i] <- 1 + if(i==1) 0 else streak[i-1]
 band <- factor(ifelse(streak==0,"wet",ifelse(streak<=3,"dry 1-3",ifelse(streak<=7,"dry 4-7","dry 8+"))),levels=c("wet","dry 1-3","dry 4-7","dry 8+"))
 for(rep in 1:30){
  seed <- 500000+(y-2021)*100+rep;set.seed(seed);d <- 1.2*runif(nrow(w),.8,1.2)
  state <- numeric(length(capacities));s <- matrix(0,nrow(w),length(capacities))
  for(i in seq_along(d)){
   s[i,] <- pmin(state,d[i]);state <- pmin(capacities,state-s[i,]+inflow[i])
  }
  k <- k+1
  detail[[k]] <- data.frame(year=y,rep=rep,seed=seed,tank=capacities,
                    coverage=100*colSums(s[use,,drop=FALSE])/sum(d[use]))
  for(b in levels(band)){
   ix <- use & band==b; gain <- sum(s[ix,length(capacities)]-s[ix,1])
   dry_results[[length(dry_results)+1]] <- data.frame(year=y,rep=rep,band=b,days=sum(ix),
     demand=sum(d[ix]),supply5=sum(s[ix,1]),supply20=sum(s[ix,length(capacities)]),gain=gain)
  }
 }
}
z <- do.call(rbind,detail); dr <- do.call(rbind,dry_results)
write.csv(z,"final/capacity_sweep.csv",row.names=FALSE)
g <- aggregate(coverage~year+tank,z,mean)
# Check the fitted surface at the two candidate sizes, using 2021 rainfall.
local_sizes <- subset(g, year == 2021 & tank %in% c(14, 15))
coded_sizes <- transform(local_sizes, A = 0, B = (tank - 12.5) / 7.5, C = 0)
local_comparison <- data.frame(
  year = local_sizes$year, roof_m2 = 500, tank_m3 = local_sizes$tank, flush_mm = 0.75,
  surface_pct = as.numeric(predict(surface, newdata = coded_sizes)),
  direct_mean_pct = local_sizes$coverage
)
local_comparison$surface_meets_55 <- local_comparison$surface_pct >= 55
local_comparison$direct_meets_55 <- local_comparison$direct_mean_pct >= 55
write.csv(local_comparison, "final/surface_vs_direct_2021.csv", row.names = FALSE)

tr <- aggregate(coverage~tank,subset(g,year<2024),min)
tr$check2024 <- subset(g,year==2024)$coverage[match(tr$tank,subset(g,year==2024)$tank)]
write.csv(tr,"final/capacity_decision.csv",row.names=FALSE)
# Compare neighbouring sizes within the same year and demand path.
size14 <- subset(z, tank == 14, select = c(year, seed, coverage))
size15 <- subset(z, tank == 15, select = c(year, seed, coverage))
pairs <- merge(size14, size15, by = c("year", "seed"), suffixes = c("14", "15"))
stopifnot(nrow(pairs) == nrow(size14), nrow(pairs) == nrow(size15),
          !anyDuplicated(pairs[c("year", "seed")]))

mean_interval <- function(x, year, quantity) {
  margin <- qt(0.975, df = length(x) - 1) * sd(x) / sqrt(length(x))
  data.frame(year = year, quantity = quantity, n = length(x), mean = mean(x),
             lower = mean(x) - margin, upper = mean(x) + margin)
}
boundary <- do.call(rbind, lapply(split(pairs, pairs$year), function(group) {
  year <- group$year[1]
  rbind(mean_interval(group$coverage14, year, "coverage_14_pct"),
        mean_interval(group$coverage15, year, "coverage_15_pct"),
        mean_interval(group$coverage15 - group$coverage14, year, "paired_gain_pp"))
}))
write.csv(boundary, "final/capacity_boundary_intervals.csv", row.names = FALSE)

byband <- aggregate(cbind(days,demand,supply5,supply20,gain)~band,dr,sum)
byband$coverage5 <- 100*byband$supply5/byband$demand
byband$coverage20 <- 100*byband$supply20/byband$demand
byband$gain_share <- 100*byband$gain/sum(byband$gain)
byband$day_share <- 100*byband$days/sum(byband$days)
write.csv(byband,"final/dryspell.csv",row.names=FALSE)
print(tr);print(byband[,c("band","coverage5","coverage20","gain_share","day_share")])
png("final/capacity_decision.png",width=1400,height=720,res=150)
plot(tr$tank,tr$coverage,type="b",pch=16,col="#24638B",ylim=range(g$coverage,65),
 xlab="Usable tank capacity (m3)",ylab="Mean annual coverage (%)")
lines(tr$tank,tr$check2024,type="b",pch=1,col="#A25C28");abline(h=c(55,60,65),lty=3,col="grey55")
legend("topleft",c("Lowest mean across 2021-2023","2024 mean"),col=c("#24638B","#A25C28"),pch=c(16,1),lty=1)
dev.off()
stopifnot(nrow(z)==2040,all(diff(tr$coverage)>= -1e-9),all(diff(tr$check2024)>= -1e-9))
