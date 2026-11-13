# Five-minute presentation voice track

Record your two real presentations against Presentation_5min_add_voices.mp4. Suggested split below. Keep the underlying annotated RStudio footage and the two extension figures in the exported video. Review your final export for audible voices from both members and a duration of five minutes. Adjust the wording and pacing to your delivery.

## Weihan Chen

**0:00–0:26 | Research question.** How large a tank should a Hong Kong building use for roof water? If the tank is already full, a bigger roof can simply lead to more overflow. If the tank is empty during dry weather, a building will still need backup water. We modelled both outcomes. Our primary measure was the percentage of annual non-potable demand that the tank could actually supply.

**0:26–0:56 | Design.** We used daily rainfall from the Hong Kong Observatory. Roof area ranged from 300 to 700 square metres, tank capacity from 5 to 20 cubic metres, and first-flush diversion from zero to 1.5 millimetres per event. Our design combined eight factorial corners, six axial points and one centre. Each of three rainfall years contained all fifteen settings. We kept rainfall in date order and randomized simulation order within each year.

**0:56–1:47 | Actual RStudio execution.** Here is the recorded experiment. The first commands open the rainfall archive. The next command runs the full script, including the pilot. Ten demand trajectories per setting and year gave a maximum pilot standard error of about 0.16 percentage points. The counts now add up to 450 main runs. Each has a different demand seed. Look at this individual trajectory. The tank supplied 245 cubic metres out of 437 cubic metres of demand, giving 56.05 percent coverage. Finally R plots average coverage for the fifteen settings. The symbols identify the rainfall years.

**1:47–2:10 | Effect sizes.** Changing tank capacity over the tested range gave the largest average increase, about twenty percentage points. Increasing the roof area gave about nine points. Greater diversion reduced coverage by about two and a half points. For a fixed roof and diversion depth, the first equal increase in tank size added fourteen points, while the second added just over six. That is direct evidence of diminishing gains.

**2:10–2:35 | Interaction.** Did a bigger roof help more with a bigger tank? Yes in 2021 and 2022. In 2023 the benefit went the other way. We also fitted all the factorial effects separately for each year. The fit improved sharply. The three numbers on screen show why a single average gives an incomplete answer for the roof and tank.

## Juwen Li

**2:35–2:55 | Validation.** We checked the surface at four new designs. Its predictions missed coverage by 1.2 percentage points on average. The two sizes near our target were closer than that in the tightest year. To choose between them, we ran both tanks through exactly the same rain and demand.

**2:55–3:37 | Capacity decision.** We kept the roof at 500 square metres and compared seventeen tank sizes. The blue line follows the lowest mean across our three historical years. We set an example target of 55 percent. Look at 2021, the year that decides the choice. For fourteen cubic metres, the interval just crosses 55 percent. For fifteen, even the lower end is above the target. The extra cubic metre gained about 0.68 percentage points on the same demand paths. Those intervals tell us about demand variation under that year's rainfall.

**3:37–4:16 | Dry spells.** Where does extra storage actually help? On the fourth through seventh consecutive dry days, the five-cubic-metre tank supplied about ten percent of demand. The twenty-cubic-metre tank supplied about forty-nine percent. This interval accounted for only sixteen percent of days but almost one-third of all additional water delivered. From the eighth dry day onward, even the larger tank supplied only seventeen percent. Storage helps bridge a gap, but prolonged dry periods still require backup.

**4:16–4:40 | Earlier rainfall.** We took the fifteen-cubic-metre choice back to thirty earlier rainfall years. Each pair of dots is one year. On the same annual demand, the triangle shows what happened when we moved more use onto dry days. The tank met the target in twenty-seven years under baseline demand. That fell to twenty-five when demand shifted.

**4:40–5:00 | Decision.** How many missed years can the building accept? Even the twenty-cubic-metre tank fell short in 2005. That year had the second-highest rainfall total and a forty-two-day dry spell. We would take the fifteen- and twenty-cubic-metre options to a site visit and compare them against measured demand, space and cost.
