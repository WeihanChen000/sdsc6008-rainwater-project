# Hong Kong rooftop rainwater harvesting

SDSC 6008 group project by Weihan Chen and Juwen Li. We used daily Hong Kong Observatory rainfall to compare roof area, storage capacity and first-flush diversion.

## For the project partner

- [Read the final eight-page report](submission/SDSC6008_Final_Report_61074487_60945590.pdf)
- [Download the verified R submission package](submission/SDSC6008_R_Code_Data_61074487_60945590.zip)
- [Read the proposal](proposal.pdf)
- [Download the five-minute video for voice-over](presentation/Presentation_5min_add_voices.mp4) and [open the speaker notes](presentation/Speaker_notes.md)
- [View the recorded RStudio demonstration](final/RStudio_demo.mp4)

The report and R package are ready for submission. The presentation is the silent version prepared for the two speakers. Each member also prepares their own reflection.

## Reproduce the analysis

Open Groupwork.Rproj at the repository root in RStudio and run:

```r
source("RUN_ALL.R")
```

The scripts use base R (tested with 4.6.1), read the archived rainfall data in data/, and write tables and plots to results/ and final/. The reference_outputs/ directory contains the archived results for comparison. The page 5 table comparing the common response surface with direct simulation is in final/surface_vs_direct_2021.csv after a run.

The LaTeX source is [final/Report.tex](final/Report.tex). To rebuild from the repository root with XeLaTeX and the fonts specified in the source, run `xelatex -output-directory=final final/Report.tex`.

Rainfall source: [Daily total rainfall at the Hong Kong Observatory](https://data.weather.gov.hk/weatherAPI/cis/csvfile/HKO/ALL/daily_HKO_RF_ALL.csv).
