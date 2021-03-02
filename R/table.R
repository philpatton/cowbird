#
# ##
# ##
# ## Tables of Parameter estimates
# ## 02/27/17
# ##
# ##
#
# write_tables <- function(output){
#
#     # load("mod1_ests.RData")
#     # library(coda)
#     # summary(output)
#     # tab = data.frame(Mean = round(summary(output)$statistics[,1], 2),
#     #                  round(summary(output)$quantiles[,c(1,5)], 2))
#     # colnames(tab)[c(2,3)] = c('lower', 'upper')
#     # xtable(tab)
#     # write.csv("mod3_ests.csv", row.names = F)
#
#     # load("mod2_ests.RData")
#     # summary(output)
#     # tab = data.frame(Mean = round(summary(output)$statistics[,1], 2),
#     #                  round(summary(output)$quantiles[,c(1,5)], 2))
#     # colnames(tab)[c(2,3)] = c('lower', 'upper')
#     # xtable(tab)
#     # write.csv(tab, "mod2_ests.csv", row.names = F)
#
#     # I edited the rownames to be in latex/mathjax format,
#     # then used online latex table converter.
#
#     load("mod3_ests.RData")
#     summary(output)
#     tab = data.frame(Mean = round(summary(output)$statistics[,1], 2),
#                      round(summary(output)$quantiles[,c(1,5)], 2))
#     colnames(tab)[c(2,3)] = c('lower', 'upper')
#     xtable(tab)
#     # write.csv(tab, "mod3_ests.csv", row.names = F)
#
#     # I edited the rownames to be in latex/mathjax format,
#     # then used online latex table converter.
#
#     ##
#     ##
#     ## WAIC table
#     ## 02/28/17
#     ##
#     ##
#
#     tab = read.csv(paste0(getwd(), "/tables/", "WAIC.table.csv"))
#     table = tab[,2:4]
#     rownames(table) = tab[,1]
#     library(xtable)
#     xtable(table)
# }
