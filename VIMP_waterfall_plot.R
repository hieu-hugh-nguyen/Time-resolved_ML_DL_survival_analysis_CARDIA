# Plot VIMP for short term and long term prediction of all variables in waterfall plots: ############################

require(dplyr)
require(ggplot2)

work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

loading.dir = paste0(getwd(),'/csv_files')

second.grouping.table.ascvd <- read.csv(file = paste0(loading.dir,'/variable_category_second_grouping_ascvd.csv')
                                        , header = T, stringsAsFactors = F)


var.order.10 <- read.csv(file = paste0(loading.dir,'/averaged_outerloop_VIMP_short_term.csv')
                         , header = T, stringsAsFactors = F)

var.order.10.second.grouping <- var.order.10 %>% inner_join(second.grouping.table.ascvd, by = 'Variable')



# Year 10 to 26 after exam 3:
var.order.26 <- read.csv(file = paste0(loading.dir,'/averaged_outerloop_VIMP_long_term.csv')
                         , header = T, stringsAsFactors = F)

var.order.26.second.grouping <- var.order.26 %>% inner_join(second.grouping.table.ascvd, by = 'Variable')



var.order.two.years <- rbind(
  var.order.10.second.grouping %>% mutate(year = '0 to 10') %>%
    dplyr::select(c("Variable", "normalized_depth",  "second.grouping", "year")),
  var.order.26.second.grouping %>% mutate(year = '10 to 26') %>%
    dplyr::select(c("Variable", "normalized_depth",  "second.grouping", "year")))


g.3 <- ggplot(var.order.two.years, aes(second.grouping, normalized_depth, fill = second.grouping)) +
  facet_wrap(~year, ncol = 1)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "darkmagenta", #"hotpink3",
  "steelblue4", "mediumpurple3", "forestgreen", "dodgerblue3", "thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)

g.3 + 
  theme_bw() +
  
  geom_jitter(aes(color = second.grouping), size = 1.5, position=position_jitter(0.2))+
  scale_color_manual(values = color_scheme[1:13]) +
  coord_fixed(ratio = 4) +
  theme(axis.text.x = element_text(size = 10, angle=60, vjust=0.6),
        axis.text.y = element_text(size = 13),
        legend.position ='none') + 
  theme(panel.background = element_blank())+
  labs(title="", 
       y="Normalized Minimum Depth of Maximal Subtree (Variable Importance)",
       x = "")