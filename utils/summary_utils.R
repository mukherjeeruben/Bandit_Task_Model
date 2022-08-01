correct_choice <- function(list_data, schedule, block){
  choice = list_data$choice
  if(schedule == 1 & block == "stable"){
    mean_trial = colMeans(choice == 2)
    mean_sub = rowMeans(choice == 2)
  }else if(schedule == 1 & block == "volatile"){
    mean_trial1 = colMeans(choice[, 1:20] == 1) 
    mean_trial2 = colMeans(choice[, 21:40] == 2) 
    mean_trial3 = colMeans(choice[, 41:60] == 1)
    mean_trial4 = colMeans(choice[, 61:70] == 2) 
    mean_trial = c(mean_trial1, mean_trial2, mean_trial3, mean_trial4)
    
    sub1 = choice[, 1:20] == 1
    sub2 = choice[, 21:40] == 2
    sub3 = choice[, 41:60] == 1
    sub4 = choice[, 61:70] == 2
    sub = cbind(sub1, sub2, sub3, sub4)
    mean_sub = rowMeans(sub) 
  }else if(schedule == 2 & block == "stable"){
    mean_trial = colMeans(choice == 1)
    mean_sub = rowMeans(choice == 1)
  }else{
    mean_trial1 = colMeans(choice[, 1:20] == 2) 
    mean_trial2 = colMeans(choice[, 21:40] == 1) 
    mean_trial3 = colMeans(choice[, 41:60] == 2)
    mean_trial4 = colMeans(choice[, 61:70] == 1) 
    mean_trial = c(mean_trial1, mean_trial2, mean_trial3, mean_trial4)
    
    sub1 = choice[, 1:20] == 2
    sub2 = choice[, 21:40] == 1
    sub3 = choice[, 41:60] == 2
    sub4 = choice[, 61:70] == 1
    sub = cbind(sub1, sub2, sub3, sub4)
    mean_sub = rowMeans(sub) 
  }
  return_list = list(mean_trial = mean_trial, mean_sub = mean_sub)
  return(return_list)
}
trial_plot <- function(list1, list2, legend1, legend2, ylabel, title){
  trial_df = data.frame(trial_mean = c(list1$mean_trial, 
                                       list2$mean_trial),
                        condition = rep(c(legend1, legend2), each=70),
                        trial = rep(1:70, time = 2))
  p = ggplot(trial_df, aes(x=trial, y=trial_mean, group=condition, color=condition))+
    geom_line(aes(linetype=condition))+
    scale_color_brewer(palette="Dark2")+
    ggtitle(title)+
    ylab(ylabel) 
  return(p)
}
trial_plot_loss <- function(list1, list2, legend1, legend2, ylabel, title){
  trial_df = data.frame(trial_mean = c(list1$mean_trial, 
                                       list2$mean_trial),
                        condition = rep(c(legend1, legend2), each=69),
                        trial = rep(1:69, time = 2))
  p = ggplot(trial_df, aes(x=trial, y=trial_mean, group=condition, color=condition))+
    geom_line(aes(linetype=condition))+
    scale_color_brewer(palette="Dark2")+
    ggtitle(title)+
    ylab(ylabel) 
  return(p)
}


subject_plot <- function(list1, list2, list3, list4, legend1, legend2,legend3, legend4, ylabel, title){
  subject_df = data.frame(sub_mean = c(list1$mean_sub, 
                                       list2$mean_sub, 
                                       list3$mean_sub,
                                       list4$mean_sub), 
                          condition = c(rep(legend1, each=length(list1$mean_sub)),
                                        rep(legend2, each=length(list2$mean_sub)),
                                        rep(legend3, each=length(list3$mean_sub)),
                                        rep(legend4, each=length(list4$mean_sub))))
  p = ggplot(subject_df, aes(x=condition, y=sub_mean, group=condition, color=condition))+
      geom_boxplot()+
      geom_jitter(shape=1, position=position_jitter(0.25))+
      scale_color_brewer(palette="Dark2")+
      ggtitle(title) +
      ylab(ylabel)
  return(p)
}

smaller_choice <- function(list_data){
  choice = list_data$choice
  amount_orange = list_data$amount_orange
  amount_blue = list_data$amount_blue
  smaller = ifelse((amount_blue<amount_orange) == TRUE, 1, 2)
  mean_trial = colMeans(choice == smaller)
  mean_sub = rowMeans(choice == smaller)
  return_list = list(mean_trial = mean_trial, mean_sub = mean_sub)
  return(return_list)
}

loss_shift_fun <- function(list_data){
  choice = list_data$choice
  outcome = list_data$outcome
  n_subj = nrow(choice)
  last_choice = cbind(rep(NA, n_subj), choice[, 1:ncol(choice)-1])
  shift = last_choice != choice
  last_loss = cbind(rep(NA, n_subj), outcome[, 1:ncol(outcome)-1])
  loss_shift = (shift==1 & last_loss==1)[,2:ncol(choice)]
  loss_no_shift = (shift==0 & last_loss==1)[, 2:ncol(choice)]
  
  trial_loss_shift = colSums(loss_shift)
  trial_loss_no_shift = colSums(loss_no_shift)
  trial_loss_shift_percen = trial_loss_shift/(trial_loss_shift+trial_loss_no_shift)
  subj_loss_shift = rowSums(loss_shift)
  subj_loss_no_shift = rowSums(loss_no_shift)
  subj_loss_shift_percen = subj_loss_shift/(subj_loss_shift+subj_loss_no_shift)
  return_list = list(mean_trial = trial_loss_shift_percen, mean_sub = subj_loss_shift_percen)
}

