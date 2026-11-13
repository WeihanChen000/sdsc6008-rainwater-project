# SDSC 6008: Hong Kong rainwater simulation. Open the R project and source this file.
if (!file.exists("data/daily_HKO_RF_ALL.csv")) stop("Open the groupwork .Rproj first.")
dir.create("results", showWarnings = FALSE)
save_csv <- function(x, file) write.csv(x, file.path("results", file), row.names = FALSE)

# Preserve the archive unchanged. Trace is below 0.05 mm per the CSV legend.
# Under the proposal's 0.1 mm wet-day rule we model it as zero effective rainfall.
raw <- readLines("data/daily_HKO_RF_ALL.csv", encoding = "UTF-8", warn = FALSE)
rows <- grep("^(2020|2021|2022|2023|2024),", raw, value = TRUE)
weather <- read.csv(text = paste(rows, collapse = "\n"), header = FALSE,
                    col.names = c("year","month","day","value","quality"))
weather$date <- as.Date(with(weather, sprintf("%04d-%02d-%02d", year, month, day)))
weather$rain <- as.numeric(ifelse(weather$value == "Trace", "0", weather$value))
stopifnot(isTRUE(all.equal(weather$date, seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day"))),
          all(weather$quality == "C"), all(is.finite(weather$rain)), all(weather$rain >= 0))
audit <- do.call(rbind, lapply(split(weather, weather$year), function(w)
  data.frame(year=w$year[1], days=nrow(w), trace_days=sum(w$value=="Trace"),
             wet_days=sum(w$rain>=0.1), effective_rain_mm=sum(w$rain))))
save_csv(audit,"weather_audit.csv")

# Rows have the same IDs and order as the submitted design table.
design <- data.frame(setting=1:15,
  roof=c(rep(c(300,700),each=4),300,700,500,500,500,500,500),
  tank=c(rep(c(5,5,20,20),2),12.5,12.5,5,20,12.5,12.5,12.5),
  flush=c(rep(c(0,1.5),4),.75,.75,.75,.75,0,1.5,.75))
save_csv(design, "design.csv")

# Daily effective rainfall after the one-off diversion per event.
retain_rain <- function(rain, flush) {
  wet <- rain >= 0.1
  start <- wet & !c(FALSE, head(wet, -1))
  retained <- numeric(length(rain))
  allowance <- flush
  for (i in which(wet)) {
    if (start[i]) allowance <- flush
    diverted <- min(allowance, rain[i])
    allowance <- allowance - diverted
    retained[i] <- rain[i] - diverted
  }
  retained
}
stopifnot(isTRUE(all.equal(retain_rain(c(.3,.8,0,.5,1.2), 1),
                           c(0,.1,0,0,.7))))
tank_run <- function(inflow, demand, capacity, score) {
  storage <- 0
  supply <- spill <- numeric(length(inflow))
  max_error <- 0
  for (i in seq_along(inflow)) {
    before <- storage
    supply[i] <- min(before, demand[i])
    available <- before - supply[i] + inflow[i]
    spill[i] <- max(0, available - capacity)
    storage <- min(available, capacity)
    max_error <- max(max_error, abs(before + inflow[i] - supply[i] - spill[i] - storage))
    if (storage < 0 || storage > capacity) stop("Storage outside capacity.")
  }
  stopifnot(max_error < 1e-9)
  backup <- demand[score] - supply[score]
  c(coverage_pct=100*sum(supply[score])/sum(demand[score]),
    backup_m3=sum(backup), overflow_m3=sum(spill[score]),
    backup_days=sum(backup > 1e-10), demand_m3=sum(demand[score]),
    supply_m3=sum(supply[score]), max_balance_error=max_error)
}
# Hand check: day one fills the tank, day two uses that water.
toy <- tank_run(c(2,0), c(1,1), 1, c(TRUE,TRUE))
stopifnot(toy["coverage_pct"]==50, toy["backup_m3"]==1, toy["overflow_m3"]==1)

make_schedule <- function(n, scheduling_seed, first_seed) {
  set.seed(scheduling_seed)
  by_year <- lapply(2021:2023,function(y) {
    g <- merge(design, data.frame(rep=seq_len(n)),by=NULL)
    g$year <- y
    g <- g[sample.int(nrow(g)),]
    g$order_in_year <- seq_len(nrow(g))
    g
  })
  g <- do.call(rbind,by_year)
  g$seed <- first_seed + seq_len(nrow(g))
  rownames(g) <- NULL
  g
}
simulate_schedule <- function(g) {
  metrics <- lapply(seq_len(nrow(g)),function(i) {
    x <- g[i,]
    w <- weather[weather$year %in% c(x$year-1,x$year),]
    inflow <- .8*x$roof*retain_rain(w$rain,x$flush)/1000
    set.seed(x$seed)
    demand <- 1.2*runif(nrow(w),.8,1.2)
    tank_run(inflow,demand,x$tank,w$year==x$year)
  })
  cbind(g,as.data.frame(do.call(rbind,metrics)))
}
# Pilot precision rule chosen for this implementation: estimated Monte Carlo
# SE of the mean annual coverage <= 0.25 percentage points in every pilot cell.
pilot <- simulate_schedule(make_schedule(10,61074,10000))
save_csv(pilot,"pilot_runs.csv")
precision <- aggregate(coverage_pct ~ setting + year, pilot, sd)
names(precision)[3] <- "pilot_sd_pp"
precision$se_at_10_pp <- precision$pilot_sd_pp/sqrt(10)
save_csv(precision,"pilot_precision.csv")
n <- max(10,ceiling(max(precision$pilot_sd_pp)^2/.25^2))
main <- simulate_schedule(make_schedule(n,60945,100000))
stopifnot(!anyDuplicated(main$seed),!any(main$seed %in% pilot$seed),
          all(table(main$setting, main$year)==n),
          all(main$coverage_pct>=0 & main$coverage_pct<=100),
          max(abs(main$supply_m3+main$backup_m3-main$demand_m3)) < 1e-7)
save_csv(main,"main_runs.csv")
cells <- aggregate(cbind(coverage_pct,backup_m3,overflow_m3,backup_days) ~
                     setting+year+roof+tank+flush,main,mean)
save_csv(cells,"setting_year_means.csv")
# Each factor coded -1, 0, +1. Balanced corner contrasts average across years.
main$A <- (main$roof-500)/200
main$B <- (main$tank-12.5)/7.5
main$C <- (main$flush-.75)/.75
corners <- subset(main,setting<=8)
factorial_fit <- lm(coverage_pct ~ factor(year)+A*B*C, corners)
fx <- coef(factorial_fit)[c("A","B","C","A:B","A:C","B:C","A:B:C")]
save_csv(data.frame(term=names(fx),factorial_effect_pp=2*unname(fx)),"factorial_effects.csv")
surface <- lm(coverage_pct ~ factor(year)+(A+B+C)^2+I(A^2)+I(B^2)+I(C^2),main)
cell_fit <- lm(coverage_pct ~ factor(interaction(setting,year)),main)
capture.output(summary(surface),anova(surface,cell_fit),file="results/response_surface.txt")
pdf("results/coverage_by_year.pdf",width=9,height=5)
means <- with(cells,tapply(coverage_pct,list(setting,year),mean))
matplot(1:15,means,type="p",pch=c(16,17,15),col=c("#24638B","#A25C28","#34704C"),
        xlab="Proposal setting",ylab="Mean annual demand coverage (%)",xaxt="n")
axis(1,at=1:15)
legend("bottomright",colnames(means),pch=c(16,17,15),col=c("#24638B","#A25C28","#34704C"))
invisible(dev.off())
capture.output(sessionInfo(), file="results/R_session.txt")
cat(sprintf("Pilot max SE %.4f pp. Repeats %d. Main runs %d.\n",max(precision$se_at_10_pp),n,nrow(main)))
print(data.frame(term=names(fx),effect_pp=round(2*unname(fx),3)))
print(subset(cells,setting==15))
print(anova(surface,cell_fit))
