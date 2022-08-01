#### plot function for the initial model parameters ##########

## This function is used to plot the group-level posterior distributions
## (mu_A, mu_beta, and mu_gamma) in stable and volatile blocks of **one or two schedules**
## if there are two levels in Group, one schedule, four levels, two schedules
plot_posterior <- function(groups, all_fits, color_scheme,name){
  mu_pars <- c("mu_A", "mu_beta", "mu_gamma")
  mu_A <- mu_beta <- mu_gamma <- data.frame(A = rep(NA, 12000),  # 12000 is the number of samples in the posterior distribution
                                            beta = rep(NA, 12000),
                                            gamma = rep(NA, 12000))
  all_dat <- list()
  for (i in mu_pars) {
    for (j in groups) {
      all_dat[[i]][[j]] <- all_fits[[j]][[i]]
    }
  }
  A_m <- melt(all_dat$mu_A); names(A_m) <- c("iter", "mu_A", "Group")
  beta_m <- melt(all_dat$mu_beta); names(beta_m) <- c("iter", "mu_beta", "Group")
  gamma_m <- melt(all_dat$mu_gamma); names(gamma_m) <- c("iter", "mu_gamma", "Group")
  A_m$Group <- beta_m$Group <- gamma_m$Group <-
    factor(A_m$Group, levels = groups, labels = groups)
  
  h1 <- ggplot(A_m, aes(x = mu_A, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.7) + 
    ggtitle(expression(A)) + 
    xlab("") +
    ylab("") +
    theme(legend.position = "none", 
          plot.title = element_text(size = 15))
  
  h2 <- ggplot(beta_m, aes(x = mu_beta, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.7) + 
    ggtitle(expression(beta)) +
    xlab("") +
    ylab("") +
    theme(legend.position = "none", 
          plot.title = element_text(size = 15))
  
  h3 <- ggplot(gamma_m, aes(x = mu_gamma, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.7) + 
    ggtitle(expression(gamma)) + 
    xlab("") +
    ylab("") +
    theme(legend.position = "none",
          plot.title = element_text(size = 15))
  
  h_key <- ggplot(gamma_m, aes(x = mu_gamma, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.5) + 
    xlab("") +
    ylab("") +
    theme(plot.title = element_text(size = 15))
  
  plots <- align_plots(h1, h2, h3, align = 'h', axis = 'l')
  legend <- get_legend(h_key)
  top_row <- plot_grid(plots[[1]],plots[[2]], rel_widths = c(1, 1), nrow = 1)
  bottom_row <- plot_grid(plots[[3]], legend, rel_widths = c(1,1), nrow =1)
  plot_row <- plot_grid(top_row,bottom_row, ncol = 1)
  title <- ggdraw() + draw_label(name, fontface = 'bold', x = 0, hjust = 0) +
    theme(# add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7))
  h_all <- plot_grid(title, plot_row, ncol = 1, # rel_heights values control vertical title margins
                     rel_heights = c(0.1, 1))
  ggsave(filename=paste0("plots/basic/", name, ".png"), plot = h_all, width = 5, height = 5)
  return(h_all)
}

## This function is used to plot the individual-level changes
## (A, beta, and gamma) from stable to volatile block, **two schedules** in the same plot

plot_individual_change <- function(schedule1_stable_fit, schedule1_volatile_fit, schedule2_stable_fit, schedule2_volatile_fit, name){
  A = c(colMeans(schedule1_stable_fit$A), colMeans(schedule2_stable_fit$A), 
        colMeans(schedule1_volatile_fit$A), colMeans(schedule2_volatile_fit$A))
  beta = c(colMeans(schedule1_stable_fit$beta), colMeans(schedule2_stable_fit$beta), 
           colMeans(schedule1_volatile_fit$beta), colMeans(schedule2_volatile_fit$beta))
  gamma = c(colMeans(schedule1_stable_fit$gamma), colMeans(schedule2_stable_fit$gamma), 
            colMeans(schedule1_volatile_fit$gamma), colMeans(schedule2_volatile_fit$gamma))
  df = data.frame(A = A,
                  beta = beta,
                  gamma = gamma,
                  condition = rep(c('stable', 'volatile'), 
                                  each = ncol(schedule1_stable_fit$A)+ncol(schedule2_stable_fit$A)),
                  combine = rep(c('1stable', '2stable', '1volatile', '2volatile'), 
                                time = c(ncol(schedule1_stable_fit$A),
                                         ncol(schedule2_stable_fit$A),
                                         ncol(schedule1_volatile_fit$A),
                                         ncol(schedule2_volatile_fit$A))),
                  paired = rep(1:(ncol(schedule1_stable_fit$A)+ncol(schedule2_stable_fit$A)), time = 2))
  df$combine = factor(df$combine, levels = c('1stable', '1volatile', '2volatile', '2stable'))
  p1 <- ggplot(df,aes(x=combine,y=A,fill=condition,col=condition))+
         geom_point(aes(color=condition),size=2) +
         geom_line(aes(group = paired),color="grey") +
         scale_fill_brewer(palette = "Dark2")+
         scale_colour_brewer(palette = "Dark2")+
         guides(fill = FALSE, col = FALSE)+
         ylab(expression(A))+
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())+
         scale_y_log10()
  p2 <- ggplot(df,aes(x=combine,y=beta,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    geom_line(aes(group = paired),color="grey") +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(beta))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  p3 <- ggplot(df,aes(x=combine,y=gamma,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    geom_line(aes(group = paired),color="grey") +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(gamma))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  all_plot <- plot_grid(p1, p2, p3, labels="AUTO")
  title <- ggdraw() + draw_label(name, fontface = 'bold', x = 0, hjust = 0) +
           theme(plot.margin = margin(0, 0, 0, 7))
  
  h_all = plot_grid(title, all_plot, ncol = 1, rel_heights = c(0.5, 5)) # rel_heights values control title margins
  ggsave(filename=paste0("plots/basic/", name, ".png"), plot = h_all, width = 6, height = 5)
  return(h_all)
}

## This function is used to plot the individual-level *rain* changes
## (A, beta, and gamma) from stable to volatile block, **two schedules** in the same plot

plot_individual_rain <- function(schedule1_stable_fit, schedule1_volatile_fit, schedule2_stable_fit, schedule2_volatile_fit, name){
  A = c(colMeans(schedule1_stable_fit$A), colMeans(schedule2_stable_fit$A), 
        colMeans(schedule1_volatile_fit$A), colMeans(schedule2_volatile_fit$A))
  beta = c(colMeans(schedule1_stable_fit$beta), colMeans(schedule2_stable_fit$beta), 
           colMeans(schedule1_volatile_fit$beta), colMeans(schedule2_volatile_fit$beta))
  gamma = c(colMeans(schedule1_stable_fit$gamma), colMeans(schedule2_stable_fit$gamma), 
            colMeans(schedule1_volatile_fit$gamma), colMeans(schedule2_volatile_fit$gamma))
  all = data.frame(A = A,
                  beta = beta,
                  gamma = gamma,
                  condition = rep(c('stable', 'volatile'), 
                                  each = ncol(schedule1_stable_fit$A)+ncol(schedule2_stable_fit$A)),
                  combine = rep(c('1stable','2stable','1volatile','2volatile'), 
                                time = c(ncol(schedule1_stable_fit$A),
                                         ncol(schedule2_stable_fit$A),
                                         ncol(schedule1_volatile_fit$A),
                                         ncol(schedule2_volatile_fit$A))),
                  paired = rep(1:(ncol(schedule1_stable_fit$A)+ncol(schedule2_stable_fit$A)), time = 2))
  all$combine = factor(all$combine, levels = c('1stable', '1volatile', '2volatile', '2stable'))
  p1 <- ggplot(all,aes(x=combine,y=A,fill=condition,col=condition))+
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4,adjust =4)+
    geom_point(position=position_jitter(width = .15),size = 0.5, alpha = 0.4)+ylab('A')+
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    # ggtitle(expression(A))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  p2 <- ggplot(all,aes(x=combine,y=beta,fill=condition,col=condition))+
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4,adjust =4)+
    geom_point(position=position_jitter(width = .15),size = 0.5, alpha = 0.4)+ylab(expression(beta))+
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    # ggtitle(expression(beta))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  p3 <- ggplot(all,aes(x=combine,y=gamma,fill=condition,col=condition))+
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4,adjust =4)+
    geom_point(position=position_jitter(width = .15),size = 0.5, alpha = 0.4)+ylab(expression(gamma))+
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    # ggtitle(expression(gamma))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  all_plot <- plot_grid(p1, p2, p3, labels="AUTO")
  title <- ggdraw() + draw_label(name, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  h_all = plot_grid(title, all_plot, ncol = 1, rel_heights = c(0.5, 5)) # rel_heights values control title margins
  ggsave(filename=paste0("plots/basic/", name, "_rain", ".png"), plot = h_all, width = 6, height = 5)
  return(h_all)
}


####################################################################################################
## This function is used to plot the group-level posterior distributions
## (mu_A, mu_beta, and mu_gamma) in stable and volatile blocks of of multivariate
## normal distribution model. 
## if there are two levels in Group, one schedule, four levels, two schedules
plot_posterior_multi_block <- function(group1, group2, all_fits, color_scheme,name){
  mu_pars <- c("mu_A", "mu_beta", "mu_gamma")
  mu_A <- mu_beta <- mu_gamma <- data.frame(A = rep(NA, 12000),  # 12000 is the number of samples in the posterior distribution
                                            beta = rep(NA, 12000),
                                            gamma = rep(NA, 12000))
  all_dat <- list()
  for (j in mu_pars) {
    for (k in group1) {
      if(k == "stable1"){
        m = 1
      }else{
        m = 2
      }
      all_dat[[j]][[k]] <- all_fits[[1]][[j]][,m]
    }
  }
  for (j in mu_pars) {
    for (k in group2) {
      if(k == "stable2"){
        m = 2
      }else{
        m = 1
      }
      all_dat[[j]][[k]] <- all_fits[[2]][[j]][,m]
    }
  }
  
  A_m <- melt(all_dat$mu_A); names(A_m) <- c("mu_A", "Group")
  beta_m <- melt(all_dat$mu_beta); names(beta_m) <- c("mu_beta", "Group")
  gamma_m <- melt(all_dat$mu_gamma); names(gamma_m) <- c("mu_gamma", "Group")
  A_m$Group <- beta_m$Group <- gamma_m$Group <-
    factor(A_m$Group, levels = c(group1, group2), labels = c(group1, group2))
  
  h1 <- ggplot(A_m, aes(x = mu_A, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.7) + 
    ggtitle(expression(A)) + 
    xlab("") +
    ylab("") +
    theme(legend.position = "none", 
          plot.title = element_text(size = 15))
  
  h2 <- ggplot(beta_m, aes(x = mu_beta, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.7) + 
    ggtitle(expression(beta)) +
    xlab("") +
    ylab("") +
    theme(legend.position = "none", 
          plot.title = element_text(size = 15))
  
  h3 <- ggplot(gamma_m, aes(x = mu_gamma, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.7) + 
    ggtitle(expression(gamma)) + 
    xlab("") +
    ylab("") +
    theme(legend.position = "none",
          plot.title = element_text(size = 15))
  
  h_key <- ggplot(gamma_m, aes(x = mu_gamma, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = color_scheme) + 
    scale_color_manual(values = color_scheme) + 
    geom_density(alpha = 0.5) + 
    xlab("") +
    ylab("") +
    theme(plot.title = element_text(size = 15))
  
  plots <- align_plots(h1, h2, h3, align = 'h', axis = 'l')
  legend <- get_legend(h_key)
  top_row <- plot_grid(plots[[1]],plots[[2]], rel_widths = c(1, 1), nrow = 1)
  bottom_row <- plot_grid(plots[[3]], legend, rel_widths = c(1,1), nrow =1)
  plot_row <- plot_grid(top_row,bottom_row, ncol = 1)
  title <- ggdraw() + draw_label(name, fontface = 'bold', x = 0, hjust = 0) +
    theme(# add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7))
  h_all <- plot_grid(title, plot_row, ncol = 1, # rel_heights values control vertical title margins
                     rel_heights = c(0.1, 1))
  ggsave(filename=paste0("plots/multi_block/", name, ".png"), plot = h_all, width = 5, height = 5)
  return(h_all)
}

plot_individual_change_multi_block <- function(schedule1_fit, schedule2_fit, name){
  schedule1_fit = rstan::extract(schedule1_fit)
  schedule2_fit = rstan::extract(schedule2_fit)
  A = c(colMeans(schedule1_fit$A[,,1]), colMeans(schedule2_fit$A[,,2]), 
        colMeans(schedule1_fit$A[,,2]), colMeans(schedule2_fit$A[,,1]))
  beta = c(colMeans(schedule1_fit$beta[,,1]), colMeans(schedule2_fit$beta[,,2]), 
           colMeans(schedule1_fit$beta[,,2]), colMeans(schedule2_fit$beta[,,1]))
  gamma = c(colMeans(schedule1_fit$gamma[,,1]), colMeans(schedule2_fit$gamma[,,2]), 
            colMeans(schedule1_fit$gamma[,,2]), colMeans(schedule2_fit$gamma[,,1]))
  df = data.frame(A = A,
                  beta = beta,
                  gamma = gamma,
                  condition = rep(c('stable', 'volatile'), 
                                  each = ncol(schedule1_fit$A[,,1])+ncol(schedule2_fit$A[,,1])),
                  combine = rep(c('1stable', '2stable', '1volatile', '2volatile'), 
                                time = c(ncol(schedule1_fit$A[,,1]),
                                         ncol(schedule2_fit$A[,,1]),
                                         ncol(schedule1_fit$A[,,1]),
                                         ncol(schedule2_fit$A[,,1]))),
                  paired = rep(1:(ncol(schedule1_fit$A[,,1])+ncol(schedule2_fit$A[,,1])), time = 2))
  df$combine = factor(df$combine, levels = c('1stable', '1volatile', '2volatile', '2stable'))
  p1 <- ggplot(df,aes(x=combine,y=A,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.2, size=1, color = 'black') +
    geom_line(aes(group = paired),color="grey", linetype = "dashed", size = 0.5) +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(A))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())+
    scale_y_log10()
  p2 <- ggplot(df,aes(x=combine,y=beta,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.2, size=1, color = 'black') +
    geom_line(aes(group = paired),color="grey", linetype = "dashed", size = 0.5) +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(beta))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  p3 <- ggplot(df,aes(x=combine,y=gamma,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.2, size=1, color = 'black') +
    geom_line(aes(group = paired),color="grey", linetype = "dashed", size = 0.5) +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(gamma))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  all_plot <- plot_grid(p1, p2, p3, labels="AUTO")
  title <- ggdraw() + draw_label(name, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  h_all = plot_grid(title, all_plot, ncol = 1, rel_heights = c(0.5, 5)) # rel_heights values control title margins
  ggsave(filename=paste0("plots/multi_block/", name, ".png"), plot = h_all, width = 6, height = 5)
  return(h_all)
}
