# Evaluate the frozen roof, diversion, target and tank shortlist on earlier weather.
if (!exists("tank_run", mode = "function")) source("run_groupwork.R")
archive_lines <- readLines("data/daily_HKO_RF_ALL.csv", encoding = "UTF-8", warn = FALSE)
records <- grep("^(1989|199[0-9]|200[0-9]|201[0-9]),", archive_lines, value = TRUE)
hist_weather <- read.csv(text = paste(records, collapse = "\n"), header = FALSE,
                         col.names = c("year", "month", "day", "value", "flag"))
hist_weather$date <- as.Date(with(hist_weather, sprintf("%04d-%02d-%02d", year, month, day)))
hist_weather$rain <- as.numeric(ifelse(hist_weather$value == "Trace", "0", hist_weather$value))
stopifnot(isTRUE(all.equal(hist_weather$date, seq(as.Date("1989-01-01"), as.Date("2019-12-31"), by = "day"))),
          all(hist_weather$flag == "C"), all(is.finite(hist_weather$rain)), all(hist_weather$rain >= 0))
dir.create("final", showWarnings = FALSE)

# Only the timing changes. Preserve annual demand separately in warm-up and scoring.
shift_demand <- function(base, rain, year, weight) {
  weighted <- base * ifelse(rain < 0.1, weight, 1)
  weighted * ave(base, year, FUN = sum) / ave(weighted, year, FUN = sum)
}

years_checked <- 1990:2019
tank_shortlist <- c(14, 15, 20)
dry_weights <- c(baseline = 1, dry_110 = 1.10, dry_weighted = 1.25, dry_150 = 1.50)
historical_runs <- lapply(years_checked, function(y) {
  w <- subset(hist_weather, year %in% c(y - 1L, y))
  score <- w$year == y
  inflow <- 0.8 * 500 * retain_rain(w$rain, 0.75) / 1000
  paths <- lapply(seq_len(30), function(rep_id) {
    seed <- 700000L + 100L * (y - 1990L) + rep_id
    set.seed(seed)
    base <- 1.2 * runif(nrow(w), 0.8, 1.2)
    do.call(rbind, lapply(names(dry_weights), function(scenario) {
      weight <- dry_weights[[scenario]]
      demand <- if (weight == 1) base else shift_demand(base, w$rain, w$year, weight)
      stopifnot(max(abs(tapply(base - demand, w$year, sum))) < 1e-8)
      supplied <- vapply(tank_shortlist, function(capacity)
        tank_run(inflow, demand, capacity, score)[["coverage_pct"]], numeric(1))
      stopifnot(all(diff(supplied) >= -1e-9))
      data.frame(year = y, seed = seed, tank = tank_shortlist, profile = scenario,
                 coverage = supplied)
    }))
  })
  do.call(rbind, paths)
})
# Weather summaries use calendar days once per year, independently of simulated demand.
year_weather <- do.call(rbind, lapply(years_checked, function(y) {
  rain <- hist_weather$rain[hist_weather$year == y]
  spells <- rle(rain < 0.1)
  data.frame(year = y, rain_mm = sum(rain),
             longest_dry_run = max(spells$lengths[spells$values]))
}))
write.csv(year_weather, "final/history_weather.csv", row.names = FALSE)
history_runs <- do.call(rbind, historical_runs)
stopifnot(nrow(history_runs) == length(years_checked) * 30 * length(tank_shortlist) * length(dry_weights),
          all(table(history_runs$year, history_runs$tank, history_runs$profile) == 30), length(unique(history_runs$seed)) == 900L)
write.csv(history_runs, "final/history_runs.csv", row.names = FALSE)
year_means <- aggregate(coverage ~ year + tank + profile, history_runs, mean)
year_se <- aggregate(coverage ~ year + tank + profile, history_runs, function(x) sd(x)/sqrt(length(x)))
names(year_se)[4] <- "mc_se"
annual <- merge(year_means, year_se)
annual$lower <- annual$coverage - qt(.975, 29) * annual$mc_se
annual$upper <- annual$coverage + qt(.975, 29) * annual$mc_se
write.csv(annual, "final/history_years.csv", row.names = FALSE)
summary_rows <- lapply(split(annual, list(annual$tank,annual$profile), drop = TRUE), function(g)
  data.frame(tank = g$tank[1], profile = g$profile[1], years = nrow(g),
             meets55 = sum(g$coverage >= 55), below = sum(g$upper < 55),
             above = sum(g$lower >= 55), straddles = sum(g$lower < 55 & g$upper >= 55),
             median_coverage = median(g$coverage), minimum = min(g$coverage),
             minimum_year = g$year[which.min(g$coverage)]))
history_summary <- do.call(rbind, summary_rows)
write.csv(history_summary, "final/history_summary.csv", row.names = FALSE)
print(history_summary, row.names = FALSE)

baseline15 <- subset(annual, tank == 15 & profile == "baseline")
# Count weather years once, using the unrounded mean across the 30 demand paths.
targets <- c(50, 55, 60)
target_counts <- do.call(rbind, lapply(targets, function(target) {
  counts <- aggregate(I(coverage >= target) ~ tank + profile, annual, sum)
  names(counts)[3] <- "years_meeting"
  transform(counts, target_pct = target, weather_years = length(years_checked))
}))
write.csv(target_counts, "final/history_targets.csv", row.names = FALSE)

weight_check <- do.call(rbind, lapply(names(dry_weights), function(scenario) {
  means <- subset(annual, tank == 15 & profile == scenario)
  matched <- merge(baseline15[c("year","coverage")], means[c("year","coverage")],
                   by = "year", suffixes = c("_base","_weighted"))
  stopifnot(nrow(matched) == length(years_checked))
  data.frame(weight = dry_weights[[scenario]], years_meeting55 = sum(matched$coverage_weighted >= 55),
             median_paired_change_pp = median(matched$coverage_weighted - matched$coverage_base))
}))
write.csv(weight_check, "final/history_weights_15m3.csv", row.names = FALSE)

shifted15 <- subset(annual, tank == 15 & profile == "dry_weighted")
paired_years <- merge(baseline15, shifted15, by = "year", suffixes = c("_base","_shift"))
paired_years$change_pp <- paired_years$coverage_shift - paired_years$coverage_base
write.csv(paired_years[c("year","coverage_base","coverage_shift","change_pp")],
          "final/history_timing_change.csv", row.names = FALSE)
cat("Paired shift median and range (pp):", median(paired_years$change_pp), range(paired_years$change_pp), "\n")

png("final/history_chart.png", width = 1500, height = 700, res = 155)
par(mar = c(4, 4.5, 1.7, 0.9))
with(paired_years, {
  plot(year, coverage_base, type = "n", ylim = range(coverage_base,coverage_shift,55) + c(-1, 4),
       xlab = "Hong Kong Observatory rainfall year", ylab = "Mean annual coverage (%)")
  abline(h = 55, lty = 2, col = "grey40")
  segments(year, coverage_shift, year, coverage_base, col = "grey68")
  points(year, coverage_base, pch = 16, col = "#19566B")
  points(year, coverage_shift, pch = 2, col = "#9D4B22")
  legend("top", horiz = TRUE, bty = "n", cex = 0.85, legend = c("Baseline demand", "More demand on dry days", "55% target"),
         pch = c(16, 2, NA), lty = c(NA, NA, 2), col = c("#19566B", "#9D4B22", "grey40"))
})
dev.off()

