# SDSC 6008 | Day 1 recording
# Open Groupwork.Rproj. Run each block with Ctrl+Enter and pause on its output.

# 1. Weihan: show where the data come from and the archived quality flags.
getwd()
cat(readLines("data/daily_HKO_RF_ALL.csv", n = 3, encoding = "UTF-8"), sep = "\n")

# 2. Weihan: run the actual pilot and main experiment.
source("run_groupwork.R")
print(audit, row.names = FALSE)

# 3. Juwen: show the 15 settings and counts of independent annual runs.
print(design, row.names = FALSE)
print(addmargins(table(setting = main$setting, year = main$year)))
print(c(distinct_main_seeds = length(unique(main$seed))))

# 4. Juwen: explain the first annual trajectory and reproduce its response.
one <- main[1, c("setting","roof","tank","flush","rep","year","seed",
                 "supply_m3","demand_m3","coverage_pct")]
print(t(one))
print(round(100 * one$supply_m3 / one$demand_m3, 2))

# 5. Show actual output in the RStudio Plots pane.
matplot(1:15, means, type = "p", pch = c(16,17,15),
        col = c("#24638B","#A25C28","#34704C"), xaxt = "n",
        xlab = "Proposal setting", ylab = "Mean annual demand coverage (%)")
axis(1, at = 1:15)
legend("bottomright", legend = colnames(means), pch = c(16,17,15),
       col = c("#24638B","#A25C28","#34704C"))