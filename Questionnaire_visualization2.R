library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#@! Adjust Y-axis label to personality pathology

# Plot parameters
height1 <- 12
width1 <- 16
res1 <- 300

# Problem parameters
# 78(6) 76(8) 74(10); cutoff == 84 (scores go higher than range)
# Overlap ranging between 0.025 to near 0.5
# Three theta estimates and three SEs
# P theta>100 ~ 0
# Different combinations of same overlap
thetas      <- c(48,52,56) # factor of 4 below the cutoff (3,2,1)
thetaSEs    <- c(4,8,12)
theta_comb  <- expand.grid(thetas,thetaSEs)

theta_range <- c(0,100)
cutoff      <- 60                                                 # Cutoff score; top 16%

overlap <- c()
limits  <- c()
for ( i in 1:nrow(theta_comb)){ 
  overlap[i] <- pnorm(theta_comb[i,1],cutoff,theta_comb[i,2])
  limits[i]   <- pnorm(100, theta_comb[i,1],theta_comb[i,2])
}


cbind(theta_comb,overlap, duplicated(overlap), limits)
cbind(theta_comb$Var1,theta_comb$Var2, round(theta_comb$Var1 - 1.96*theta_comb$Var2), 
      round(theta_comb$Var1 + 1.96*theta_comb$Var2))

# overlap   <- c(.10)                    # Probability overlap with cutoff

# theta_se  <- c(2,5,10)                                                  # Standard error

# theta_est <- list()
# for(i in 1:length(theta_se)){
# (theta_est[[i]]<- qnorm(overlap, cutoff, theta_se[i],lower.tail = T))  # Theta estimate
# }
#  pnorm(theta_est[[1]],cutoff, theta_se[1]);overlap

# Or manipulate theta_se to get overlap
# theta_est <- 78
# theta_se <- (cutoff - theta_est)/ qnorm(overlap, lower.tail = F)
# pnorm(theta_est,cutoff, theta_se);overlap; theta_se

# y <- seq(theta_range[1],theta_range[2],0.01)
# x <- dnorm(y,theta_est,theta_se)

# 
label1 <- c("Personlighetspatologi","Mild","Alvorlig") # Norwegian
label1 <- c("Personality pathology","Mild","Severe") # English
label1 <- c("Persoonlijkheidspathologie","Mild","Ernstig") # Dutch

# BASE PLOT
base_plot  <-   ggplot(data = NULL) +
  scale_y_continuous(name = label1[1], # Y-axis
                     limits = theta_range,
                     breaks = c(0,25,50,75,100), 
                     labels = c(paste0(label1[2]," 0"),"25","50","75",paste0(label1[3]," 100"))) +
  scale_x_continuous(limits = c(0,2)) +
  geom_hline(yintercept = cutoff,lty = 2) + #cutoff line
  
  #geom_point(aes(y = 30, 1), shape = 23,fill = "white", size = 4) + 
  
  theme_classic() + # Classic theme
  theme(text = element_text(size=20), # Empty x-axis
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggsave("base_plot.png",
       plot = base_plot,
  width = width1,
  height = height1,
  units = "cm",
  dpi = res1)


########################
### QUANTILE DOTPLOT ###
########################
for(i in 1:nrow(theta_comb)){
# How to choose binsize?
N_points    <- 20                                                       # Number of points
quant_y_dot <- qnorm(ppoints(N_points), mean= theta_comb[i,1], sd = theta_comb[i,2]) # Density

ggsave(paste0("quant_dotplot_",theta_comb[i,1],"(",theta_comb[i,2],").png"), 
       plot = base_plot +
           geom_dotplot(aes(y= quant_y_dot, x = 1),binpositions ="bygroup",binwidth= 4,
           binaxis = "y", stackdir = "center") + 
           geom_point(aes(y = theta_comb[i,1], 1), shape = 23,fill = "white", size = 4) + 
           geom_hline(yintercept = cutoff,lty = 2), # Cutoff line

  width = width1,
  height = height1,
  units = "cm",
  dpi = res1)

}

########################
###### ERROR BAR #######
########################
for(i in 1:nrow(theta_comb)){
  prob      <- 0.95
  conf_lvl  <- qnorm(1-(1-prob)/2)
  
  ggsave(paste0("error_bar_",theta_comb[i,1],"(",theta_comb[i,2],").png"), 
         plot = base_plot + 
           geom_errorbar(aes(ymin = theta_comb[i,1] - conf_lvl*theta_comb[i,2], ymax =  theta_comb[i,1] + conf_lvl*theta_comb[i,2], x = 1), width = .2) +
           geom_point(aes(y = theta_comb[i,1], 1), shape = 23,fill = "white", size = 4) + 
           geom_hline(yintercept = cutoff,lty = 2), # Cutoff line

  width = width1,
  height = height1,
  units = "cm",
  dpi = res1)

}

#############################
### DIAMOND GRADIENT PLOT ###
#############################
for(i in 1:nrow(theta_comb)){
  prob_dens   <- c(.001,.025,.16,.84,.975,.999)
  quant_y_gra <- qnorm(prob_dens, mean= theta_comb[i,1], sd = theta_comb[i,2])
  x_location  <- 3
  prob_dens   <- ifelse(prob_dens > .50, 1 - prob_dens, prob_dens) # c(.001,.05,.16,.16,.05,.001)
  
  ggsave(paste0("diamont_area_",theta_comb[i,1],"(",theta_comb[i,2],").png"),
         plot = base_plot + 
           geom_polygon(aes(y = c(theta_comb[i,1],quant_y_gra[6], theta_comb[i,1], quant_y_gra[1]), 
                     x = c(1.2, 1, 0.8, 1)), fill = "white", color = "black") +
           geom_polygon(aes(y = c(theta_comb[i,1],quant_y_gra[5], theta_comb[i,1], quant_y_gra[2]), 
                     x = c(1.2, 1, 0.8, 1)), fill = "white", color = "black") +
           geom_polygon(aes(y = c(theta_comb[i,1],quant_y_gra[4], theta_comb[i,1], quant_y_gra[3]), 
                     x = c(1.2, 1, 0.8, 1)), fill = "white", color = "black") +
           geom_point(aes(y = theta_comb[i,1], 1), shape = 23,fill = "white", size = 4) + 
           geom_hline(yintercept = cutoff,lty = 2), # Cutoff line
  
         width = width1,
         height = height1,
         units = "cm",
         dpi = res1)  
}

########################
##### VIOLIN PLOT ######
########################
for(i in 1:nrow(theta_comb)){
  N_points_vio <- 500
  quant_y_vio <- qnorm(ppoints(N_points_vio), mean= theta_comb[i,1], sd = theta_comb[i,2])      # Density for violin
  
  ggsave(paste0("violin_",theta_comb[i,1],"(",theta_comb[i,2],").png"),
         plot = base_plot + 
         geom_violin(aes(y= quant_y_vio, x = 1))+
         geom_point(aes(y = theta_comb[i,1], 1), shape = 23,fill = "white", size = 4) + 
         geom_hline(yintercept = cutoff,lty = 2), # Cutoff line
  
  width = width1,
  height = height1,
  units = "cm",
  dpi = res1)
}



############################
### OVERVIEW FOR RANKING ###
############################
# Plot parameters
height1 <- 20
width1 <- 35
res1 <- 300

# Problem parameters
cutoff    <- 60                                                 # Cutoff score
theta_se  <- 5                                              # Standard error, .10 and .25 for presentation
overlap   <- .11 #round(runif(1,0.025,0.50),2)                       # Probability overlap with cutoff .30 for presentation
(theta_est <- qnorm(overlap, cutoff, theta_se,lower.tail = T))  # Theta estimate

theta_range <- c(0, 100)

y <- seq(theta_range[1],theta_range[2],0.01)
x <- dnorm(y,theta_est,theta_se)

# Set confidence level for error bars
prob      <- 0.95
conf_lvl  <- qnorm(1-(1-prob)/2)


# Quantile dotplot
# How to choose binsize?
N_points    <- 20                                                       # Number of points
quant_y_dot <- qnorm(ppoints(N_points), mean= theta_est, sd = theta_se) # Density

# 50th quantile not always equal to the mean estimated score
N_points_vio <- 500
quant_y_vio <- qnorm(ppoints(N_points_vio), mean= theta_est, sd = theta_se)      # Density for violin

# How to choose binsize, endpoints, plot from outside in
N_points_gra <- 10
quant_y_gra <- qnorm(ppoints(N_points_gra), mean= theta_est, sd = theta_se)
prob_dens <- pnorm(quant_y_gra, mean = theta_est, sd = theta_se)
prob_dens <- c(prob_dens[1:(N_points_gra/2)], 1 - prob_dens[(N_points_gra/2+1):(N_points_gra)])

prob_dens   <- c(.001,.05,.16,.84,.95,.999)
quant_y_gra <- qnorm(prob_dens, mean= theta_est, sd = theta_se)
x_location  <- 3
prob_dens   <- c(.001,.05,.16,.16,.05,.001)

### BASIC PLOTS ###
base_plots <- ggplot(data = NULL) +
  
  # 95% CI error bars
  geom_errorbar(aes(ymin = theta_est - conf_lvl*theta_se, ymax =  theta_est + conf_lvl*theta_se, x = 2), width = .2) +
  geom_point(aes(y = theta_est, 2), shape = 23,fill = "white", size = 4)  + 
  
  # violin plot
  geom_violin(aes(y= quant_y_vio, x = 3)) +
  geom_point(aes(y = theta_est, 3), shape = 23,fill = "white", size = 4) +
  
  
  # Gradient plot
  geom_polygon(aes(y = c(theta_est,quant_y_gra[6], theta_est, quant_y_gra[1]), 
                   x = c(4 + .5,4,4 - .5,4)), fill = "white", color = "black") +
  geom_polygon(aes(y = c(theta_est,quant_y_gra[5], theta_est, quant_y_gra[2]), 
                   x = c(4 + .5,4,4 - .5,4)), fill = "white", color = "black") +
  geom_polygon(aes(y = c(theta_est,quant_y_gra[4], theta_est, quant_y_gra[3]), 
                   x = c(4 + .5,4,4 - .5,4)), fill = "white", color = "black") +
  geom_point(aes(y = theta_est, 4), shape = 23,fill = "white", size = 4)  +
  
  # quantile dotplot
  geom_dotplot(aes(y= quant_y_dot, x = 5),binpositions ="bygroup",binwidth= 4,
               binaxis = "y", stackdir = "center") + 
  geom_point(aes(y = theta_est, 5), shape = 23,fill = "white", size = 4) + 
  #stackratio = 0.9, dotsize = .5)  + 
  
  # Cutoff
  geom_hline(yintercept = cutoff,lty = 2) +
  
  
  # Textual Dutch
  # geom_label(aes(y = 45, x = 1), 
  #           label = stringr::str_wrap(paste0("De geobserveerde score van deze patiënt is ", round(theta_est,0), 
  #                                            ". scores tussen de ",
  #                                            round(theta_est - conf_lvl*theta_se,0)," en ",
  #                                            round(theta_est + conf_lvl*theta_se,0)," vallen binnen een foutmarge 
  #                                            (95% betrouwbaarheidsinterval) voor deze patiënt."), width = 15), size = 6) +
  # Textual Norway
  geom_label(aes(y = 45, x = 1), 
             label = stringr::str_wrap(paste0("Testskårene til denne pasienten er ", round(theta_est,0), 
                                              ". skårer mellom ",
                                              round(theta_est - conf_lvl*theta_se,0)," og ",
                                              round(theta_est + conf_lvl*theta_se,0)," faller innenfor feilmarginen (95% konfidensintervall) 
                                              for denne pasienten."), width = 15), size = 6) +

  scale_y_continuous(name = label1[1], # Y-axis
                     limits = theta_range,
                     breaks = c(0,25,50,75,100), 
                     labels = c(paste0(label1[2]," 0"),"25","50","75",paste0(label1[3]," 100"))) +
  #scale_x_continuous(breaks = 1:5, name = "plot type", limits = c(0.5,6),
  #                   labels=c("optie a","optie b","optie c","optie d","optie e")) + 
  
  scale_x_continuous(breaks = 1:5, name = "formatene", limits = c(0.5,6),
                     labels=c("alternativ a","alternativ b","alternativ c","alternativ d","alternativ e")) + 
  
  theme_classic() + 
  theme(text = element_text(size=24)) 


base_plots
overlap
theta_est

ggsave("Figure_rank.png",
       plot = base_plots,
       width = width1,
       height = height1,
       units = "cm",
       dpi = res1)
