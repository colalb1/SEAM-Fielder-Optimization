# libraries
library(dplyr)
library(plotrix)
library(data.table)
library(GenSA)
library(ggplot2)
library(ggforce)
library(berryFunctions)
library(MASS)

# data
similarity_dt = data.table(readRDS("similarity-for-comparison.rds"))
babip_dt = data.table(readRDS("sampling-babip-validation-oos-changed-batters.rds"))

similarity_dt[batter == "Vladimir Guerrero Jr.", batter := "Vladimir Guerrero"]

combined_dt = similarity_dt[babip_dt, on = .(pitcher, batter), nomatch = 0]; combined_dt[, c("rhp", "rhb") := NULL]


# Some analysis. Relatively trivial; will leave for reader to play with.
combined_dt_lm = lm(babip_rel_err ~ log(similarity), 
                    data = combined_dt)
summary(combined_dt_lm)

plot(babip_rel_err ~ log(similarity), 
     data = combined_dt)