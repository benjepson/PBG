
# why - provide a basic, functional process behavior graph
# doing for the experience, also to share my unique perspective
# why - teach basics of process behavior graphs

# packages ----
library(ggplot2)
library(patchwork)
theme_set(theme_classic())


# Function to calculate PBG info ----

# This function will be used for many things. 
# Maybe generate an object with the original dataset with these values attached?

# calculate PBC values:
# mean y_vals
# average moving range
# 3 sigma (avg mR * 2.66)
# UYL = mean yvals + 3 sigma
# LYL = mean yvals - 3 sigma
get_PBG_values <- function(y) {
    
    y_vals_avg  = mean(y)
    mR_avg      = mean(abs(diff(y)))
    three_sigma = 2.66 * mR_avg
    mid_UL      = y_vals_avg + (0.5*three_sigma)
    mid_LL      = y_vals_avg - (0.5*three_sigma)
    UL          = y_vals_avg + three_sigma
    LL          = y_vals_avg - three_sigma
    mR_UL       = mR_avg * 3.268
        
    PBG_vals <- data.frame(y_vals_avg, 
                     mR_avg, 
                     three_sigma, 
                     mid_UL,
                     mid_LL, 
                     UL, 
                     LL,
                     mR_UL)

}



# Inputs ----

# 1 - the data itself ----
# instruction box telling user what to do:
# user inputs their data (y values)
# (comma separated, type them in? cells/spreadsheet interface?)

y_vals <- c(41.2, 44.9, 51.6, 45.9, 38.0, 36.4, 49.9, 44.8)

# check if numeric

#if TRUE, continue, if FALSE display an error 
if (is.numeric(y_vals) == TRUE) {
    PBC_vals <- get_PBG_values(y = y_vals)
    } else 
        {
            print("Error - values must be numeric, separated by commas")
} #ok seems to work


dat <- data.frame(y_vals, 
                  index_col = 1:length(y_vals), 
                  mR_vals = c(0, abs(diff(y_vals))))
# ok seems to work

rm(y_vals)

PBG_vals <- get_PBG_values(dat$y_vals)

# 2 SIDEBAR - graph options inputs ----

# y axis label
# x axis label
# title
# subtitle
# theme
# point size
# point color
# line colors
# add mid box
# mid box color
# mid box alpha
# switch between static and interactive (?)
# OR MAYBE just abandon ggplot and do it directly in a better interactive package
# add moving range plot or just y_graph
# option to adjust ratio of y_graph and mR graph heights to each other
# add sideways histogram to layout next to main graph


# 3 - The main graph ----
# a section that shows the graph

# turn this into an easy function the user can give arguments for:

y_graph <- ggplot(data = dat, 
       aes(x = index_col, y = y_vals)) + 
    #mid box
    annotate('rect', 
             xmin = 1, 
             xmax = max(dat$index_col)+0.5, 
             ymin = PBG_vals$mid_LL, 
             ymax = PBG_vals$mid_UL, 
             alpha = 0.1, 
             fill = "green")+
        geom_point() + 
        geom_line() + 
    
    # upper, lower limits, and average
        geom_hline(yintercept = PBG_vals$UL, color = "purple", linewidth = 1) + #UL
        annotate(geom = "label", x = max(dat$index_col)+0.5, y = PBG_vals$UL, color = "purple", label = "UL")+
        geom_hline(yintercept = PBG_vals$LL, color = "purple", linewidth = 1) + #LL
        annotate(geom = "label", x = max(dat$index_col)+0.5, y = PBG_vals$LL, color = "purple", label = "LL")+
        geom_hline(yintercept = PBG_vals$y_vals_avg, color = "blue", linewidth = 1) + #average
        annotate(geom = "label", x = max(dat$index_col)+0.5, y = PBG_vals$y_vals_avg, color = "blue", label = "avg")+
    #integer breaks on x axis
    #needed to specify so mR and main graph line up!
        scale_x_continuous(breaks = scales::breaks_pretty(), 
                           limits = c(1, max(dat$index_col)+0.5))+
        xlab(label = "")


y_graph

#logic select if static or interactive? Or just do interactive?

#plotly kinda isn't working great. we'll try a different one when the elements are in place
# I mean it's fine but labels don't work. I don't love ggplotly
plotly::ggplotly(y_graph)      

# 3a ----
# sideways histogram next to main graph

n_bins <- if (sqrt(length(dat$y_vals))< 5) 5 else sqrt(length(dat$y_vals))

main_histogram <- ggplot(data = dat, 
                         aes(y = y_vals))+
                        geom_histogram(bins = n_bins, color = "white", fill = "light blue")+
                        theme_void()

# 4 - mR_graph ----
# another box displaying this graph

mR_graph <- ggplot(data = dat, 
       aes(x = index_col, y = mR_vals)) + 
    
    geom_point() + 
    geom_line()+
    
    geom_hline(yintercept = PBG_vals$mR_UL, color = "purple", size = 1) + #UL
    annotate(geom = "label", x = max(dat$index_col)+0.5, y = PBG_vals$mR_avg, color = "purple", label = "avg")
    geom_hline(yintercept = PBG_vals$mR_avg, color = "blue", size = 1)+ #average
    annotate(geom = "label", x = max(dat$index_col)+0.5, y = PBG_vals$mR_avg, color = "blue", label = "avg")+
    #need to always include zero in mR graph
    scale_y_continuous(limits = c(0, NA))+
    scale_x_continuous(breaks = scales::pretty_breaks(), 
                       limits = c(1, max(dat$index_col)+0.5))+
    xlab(label = "")


# The idea of how the main plus mR graphs will look
y_graph / mR_graph + plot_layout(heights = c(2,1))
    
# 5 side box explaining----
# text box with small reminder, run rules, graph interpretation