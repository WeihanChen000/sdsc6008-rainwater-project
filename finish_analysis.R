# Run from the Groupwork.Rproj root. This script also reruns the pilot and main design.
source("run_groupwork.R")
out <- function(x,f) write.csv(x,file.path("final",f),row.names=FALSE)
dir.create("final",showWarnings=FALSE)
# Conditional uncertainty: variance of each independent cell mean, Welch df.
contrast <- function(dat,weights) {
  key <- paste(dat$setting,dat$year)
  m <- tapply(dat$coverage_pct,key,mean); v <- tapply(dat$coverage_pct,key,var)/table(key)
  w <- weights[names(m)]; w[is.na(w)] <- 0
  vv <- as.numeric(w^2*v); se <- sqrt(sum(vv))
  df <- sum(vv)^2/sum(vv^2/(as.numeric(table(key))-1))
  est <- sum(w*m)
  c(estimate=est,low=est-qt(.975,df)*se,high=est+qt(.975,df)*se)
}
weights_for <- function(ids,weights,years) {
  g <- expand.grid(setting=ids,year=years)
  setNames(rep(weights,length(years))/length(years),paste(g$setting,g$year))
}
effects <- list()
for (y in list(2021L,2022L,2023L,2021:2023)){
  panel <- if(length(y)==1) as.character(y) else "mean"
  add <- function(term,ids,w) {
    q <- contrast(main,weights_for(ids,w,y))
    data.frame(panel=panel,term=term,estimate=q[1],low=q[2],high=q[3])
  }
  a <- c(rep(-1,4),rep(1,4)); b <- rep(c(-1,-1,1,1),2); cc <- rep(c(-1,1),4)
  weights <- list(roof=a/4,tank=b/4,flush=cc/4,roof_tank_DID=a*b/2,
                  roof_tank=a*b/4,roof_flush=a*cc/4,tank_flush=b*cc/4)
  effects[[panel]] <- do.call(rbind,c(lapply(names(weights),function(nm)add(nm,1:8,weights[[nm]])),list(
    add("tank_first",c(11,15),c(-1,1)),add("tank_second",c(15,12),c(-1,1)),
    add("diminishing",c(11,15,12),c(-1,2,-1)),add("flush_first",c(13,15),c(-1,1)),
    add("flush_second",c(15,14),c(-1,1)))))
}
effects <- do.call(rbind,effects);out(effects,"hypotheses.csv")
year_factorial <- lm(coverage_pct ~ factor(year) * A * B * C, data = corners)
corner_comparison <- anova(factorial_fit, year_factorial)
capture.output(anova(factorial_fit), summary(factorial_fit), corner_comparison,
               file = "final/factorial_ANOVA.txt")
out(as.data.frame(corner_comparison), "corner_year_comparison.csv")
# Residuals from the 24 corner-by-year cell means, separate from the response surface.
corner_cells <- interaction(corners$setting, corners$year, drop = TRUE)
cell_sd <- aggregate(coverage_pct ~ year + setting, corners, sd)
names(cell_sd)[3] <- "within_cell_sd_pp"
out(cell_sd, "corner_cell_sd.csv")
abs_deviation <- abs(residuals(year_factorial) - ave(residuals(year_factorial), corner_cells, FUN = median))
spread_test <- anova(lm(abs_deviation ~ corner_cells))
out(data.frame(residual_df = df.residual(year_factorial),
               min_cell_sd = min(cell_sd$within_cell_sd_pp), max_cell_sd = max(cell_sd$within_cell_sd_pp),
               spread_F = spread_test[1,"F value"], df1 = spread_test[1,"Df"], df2 = spread_test[2,"Df"],
               spread_p = spread_test[1,"Pr(>F)"], max_abs_std_residual = max(abs(rstandard(year_factorial)))),
    "corner_diagnostic_summary.csv")
png("final/corner_residuals.png", width = 1500, height = 760, res = 140)
par(mfrow = c(1,2), mar = c(4.4,4.4,2.1,0.9))
plot(fitted(year_factorial), rstandard(year_factorial), pch = 16, cex = 0.7,
     xlab = "Fitted mean annual coverage (%)", ylab = "Standardized residual",
     main = "Corner model by year")
abline(h = 0, lty = 2)
qqnorm(rstandard(year_factorial), pch = 16, cex = 0.7, main = "Normal Q-Q plot",
       ylab = "Standardized residual")
qqline(rstandard(year_factorial), lty = 2)
dev.off()
byyear <- lm(coverage_pct ~ factor(year)*((A+B+C)^2+I(A^2)+I(B^2)+I(C^2)),main)
capture.output(anova(surface,byyear,cell_fit),file="final/year_model_tests.txt")

# Preselected interior settings. Validation draws occupy a separate seed range.
points <- data.frame(setting=16:19,roof=c(400,600,450,650),tank=c(8,16,17,7),flush=c(.4,1.1,.35,1.2))
runs_for <- function(des,years,offset) {
  g <- merge(des,expand.grid(year=years,rep=1:10),by=NULL);g$seed <- offset+seq_len(nrow(g));g
}
v <- simulate_schedule(runs_for(points,2021:2023,300000))
v$A <- (v$roof-500)/200;v$B <- (v$tank-12.5)/7.5;v$C <- (v$flush-.75)/.75
v$pred <- predict(surface,v);v$pred_year <- predict(byyear,v)
vm <- aggregate(cbind(coverage_pct,pred,pred_year)~setting+year,v,mean)
vm$err <- vm$pred-vm$coverage_pct;vm$err_year <- vm$pred_year-vm$coverage_pct
out(vm,"interior_validation.csv")
held <- simulate_schedule(runs_for(design,2024,400000))
hm <- aggregate(coverage_pct~setting,held,mean);names(hm)[2] <- "year2024"
hm$historical <- as.numeric(tapply(main$coverage_pct,main$setting,mean))
out(hm,"weather_2024.csv")
# Paired scenarios replay the original daily random numbers for each run.
scenarios <- data.frame(name=c("baseline","efficiency_070","efficiency_090","demand_096","demand_144","wider_variation","two_dry_days","inflow_first"),
 efficiency=c(.8,.7,.9,rep(.8,5)),mu=c(1.2,1.2,1.2,.96,1.44,1.2,1.2,1.2),
 delta=c(rep(.2,5),.4,.2,.2),gap=c(rep(1,6),2,1),inflow_first=c(rep(FALSE,7),TRUE))
scenario_cover <- function(x,s) {
  w <- weather[weather$year %in% c(x$year-1,x$year),]
  set.seed(x$seed); demand <- s$mu*runif(nrow(w),1-s$delta,1+s$delta)
  st <- 0; dry <- s$gap; rem <- 0; supply <- numeric(nrow(w))
  for(i in seq_along(demand)){
    r <- w$rain[i]; net <- 0
    if(r>=.1){if(dry>=s$gap) rem <- x$flush;dry <- 0
      loss <- min(rem,r); rem <- rem-loss;net <- r-loss
    }else dry <- dry+1
    inflow <- s$efficiency*x$roof*net/1000
    if(s$inflow_first) st <- min(x$tank,st+inflow)
    supply[i] <- min(st,demand[i]);st <- st-supply[i]
    if(!s$inflow_first) st <- min(x$tank,st+inflow)
  }
  sel <- w$year==x$year;100*sum(supply[sel])/sum(demand[sel])
}
sm <- do.call(rbind,lapply(seq_len(nrow(scenarios)),function(j){
  x <- main
  x$coverage_pct <- vapply(seq_len(nrow(main)),function(i)scenario_cover(main[i,],scenarios[j,]),numeric(1))
  if(j==1)stopifnot(max(abs(x$coverage_pct-main$coverage_pct))<1e-8)
  f <- coef(lm(coverage_pct~factor(year)+A*B*C,subset(x,setting<=8)))
  data.frame(scenario=scenarios$name[j],mean_coverage=mean(x$coverage_pct),roof=2*f["A"],tank=2*f["B"],flush=2*f["C"])
}));out(sm,"sensitivity.csv")
draw <- function(file,fun){png(paste0("final/",file,".png"),width=1400,height=760,res=150);fun();dev.off()}
draw("year_coverage",function(){
 matplot(1:15,means,type="p",pch=c(15,16,17),col=c("#1C6677","#B06B2E","#465485"),xlab="Design setting",ylab="Annual coverage (%)",xaxt="n");axis(1,1:15)
 legend("bottomright",colnames(means),pch=c(15,16,17),col=c("#1C6677","#B06B2E","#465485"))})
draw("tank_curve",function(){
 xx <- cells[cells$setting %in% c(11,15,12),]
 yy <- with(xx,tapply(coverage_pct,list(tank,year),mean))
 matplot(c(5,12.5,20),yy,type="b",pch=15:17,lty=1,col=c("#1C6677","#B06B2E","#465485"),xlab="Usable tank capacity (m3)",ylab="Annual coverage (%)")
 legend("bottomright",colnames(yy),pch=15:17,col=c("#1C6677","#B06B2E","#465485"))})
draw("diagnostics",function(){par(mfrow=c(1,2));plot(surface,which=1);plot(surface,which=2)})
draw("validation",function(){lim<-range(vm$pred,vm$coverage_pct);plot(vm$coverage_pct,vm$pred,xlim=lim,ylim=lim,pch=16,col="#1C6677",xlab="Direct simulation (%)",ylab="Quadratic prediction (%)");abline(0,1,lty=2)})
print(subset(effects,panel=="mean"),digits=4)
print(vm,digits=4);print(sm,digits=4)
print(hm,digits=4)
cat("Validation MAE and max:",mean(abs(vm$err)),max(abs(vm$err)),"annual model MAE",mean(abs(vm$err_year)),"\n")
print(aggregate(cbind(coverage_pct,backup_m3,overflow_m3,backup_days)~year,cells,mean))
