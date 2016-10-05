# Highchart interactive barplot with line superimposed 
#============================================================================================#

source("~/pkg")
dt = portfolio_example
format_data(dt)
set_attributes(DT = dt, exposure = "Exposure", claimcount = "Numtppd", claimcost = "Indtppd")
stats1 = stats(DT = dt, varnames = c("Occupation", "Type_Car"), order_by = "exposure", order = -1)
stats1[["Occupation"]]$freq <- round(stats1[["Occupation"]]$nbclaims/stats1[["Occupation"]]$exposure, digits = 2)
stats1[["Type_Car"]]$freq <- round(stats1[["Type_Car"]]$nbclaims/stats1[["Type_Car"]]$exposure, digits = 2)
plt1 = plotfreq(stats1)
plt1$Occupation
#plt1$Type_Car
