######################################
# collection of functions 
######################################
# written by Taro Mieno on 04/09/2016




# CF_run <- function(data,rates,var_ls,cl_id=NA){
#   #=== treatment assignment ===#
#   data_temp_dt <- data[treat_var %in% rates,] %>%
#     .[,trt:=ifelse(treat_var==rates[1],0,1)] %>% 
#     .[,id:=1:nrow(.)]

#   # data_temp_dt <- balance_data(data_temp_dt,var_ls_all)

#   #=== causal forest analysis ===#
#   X <- data_temp_dt[,var_ls,with=FALSE]
#   Y <- data_temp_dt[,yield]
#   W <- data_temp_dt[,trt]

#   if (!is.na(cl_id)){
#     cl <- data_temp_dt[,cl_id]
#   }
  

#   #=== preliminary runs ===#
#   Y_forest <- regression_forest(X,Y)
#   Y_hat <- predict(Y_forest)$predictions

#   W_forest <- regression_forest(X,W)
#   W_hat <- predict(W_forest)$predictions

#   # #=== raw forest ===#
#   # tau_forest_raw <- causal_forest(X, Y, W, Y.hat=Y_hat,W.hat=W_hat)
#   # var_imp <- variable_importance(tau_forest_raw)
#   # var_imp > mean(var_imp)

#   #=== causal forest analysis ===#
#   if (!is.na(cl_id)) {
#     #--- if cl_id is present ---#
#     tau_forest_temp <- causal_forest(X, Y, W,
#       Y.hat=Y_hat,
#       W.hat=W_hat,
#       num.trees=4000,
#       # min.node.size=10,
#       cluster=cl,
#       # tune.parameters=''
#       # tune.parameters='all'
#       tune.parameters=c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
#     )

#   } else {
#     #--- if cl_id is NOT present ---#
#     tau_forest_temp <- causal_forest(X, Y, W,
#       Y.hat=Y_hat,
#       W.hat=W_hat,
#       num.trees=4000,
#       min.node.size=10,
#       # tune.parameters=''
#       # tune.parameters='all'
#       tune.parameters=c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
#     )
#   }

#   return(tau_forest_temp)
# }



#===================================
#  Visualize CF results
#===================================
#' Description
#' + get_impact(): Calculate the HTE (prediction) for a specific variables
#' + gen_impact_viz(): run get_impact() for all the variables and visualize 

get_impact <- function(cf_res,data_base,var_ls,var_name){
	# cf_res= cf1; data_base=data_reg_case3; treat_var='treat2'; var_ls= cov_ls
	# var_name = var_ls[[1]]

	#- make summary table -#
  X_eval_base <- copy(data_base)[,var_ls,with=FALSE] %>%
    as_tibble(.) %>% 
    summarize_all(mean) %>%
    data.table()

  #- change the name of a target variable to `temp_var` -#
  data_temp <- copy(data_base) %>%
    setnames(var_name,'temp_var')

  #- define the range of `temp_var` values used for prediction -#
  min_temp_var <- data_temp[,temp_var] %>% quantile(prob=0.025)
  max_temp_var <- data_temp[,temp_var] %>% quantile(prob=0.90)

  #- create testing dataset -#
  	#- for all the other variables, the values are set as their mean values -#
  temp_impact <- copy(X_eval_base) %>%
    setnames(var_name,'temp_var') %>%
    .[rep(1,1000),] %>%
    .[,temp_var:=seq(min_temp_var,max_temp_var,length=1000)] %>%
    setnames('temp_var',var_name) 

  #- prediction -#
  tau_hat_set <- predict(cf_res, temp_impact, estimate.variance = TRUE)

 	#- data including tau_hat and se -#
  return_data <- copy(temp_impact) %>% 
    .[,tau_hat:=tau_hat_set$predictions] %>%
    .[,tau_hat_se:=sqrt(tau_hat_set$variance.estimates)] %>%
    .[,c('tau_hat','tau_hat_se',var_name),with=FALSE] %>%
    setnames(var_name,'value') %>%
    .[,type:='Treatment Effect (inches)'] %>%
    .[,variable:=var_name] 

  return(return_data)
}

# treat_var <- 'tgts'

gen_impact_viz <- function(cf_res,data_base,treat_var,var_ls){
	# cf_res= cf1; data_base=data_reg_case3; treat_var='treat2'; var_ls= cov_ls

  impact_data <- lapply(var_ls,function(x) get_impact(cf_res=cf_res,data_base=data_base,var_ls=var_ls,x)) %>%
    rbindlist() 
  # x <- 'aa_s'

  min_max_v <- impact_data[,.(min_v=min(value),max_v=max(value)),by=variable]

  temp_data <- copy(data_base) %>% 
    setnames(treat_var,'treat_var') 

  treatment_ls <- temp_data[,treat_var] %>%  unique %>%  sort

  data_dist_viz <- temp_data %>% 
    .[,var_ls,with=FALSE] %>% 
    .[,id:=1:nrow(.)] %>% 
    melt(id.var='id') %>%
    .[,type:='Distribution'] %>% 
    min_max_v[.,on='variable'] %>% 
    .[value>=min_v & value<=max_v,] 

  g_impact_viz <- ggplot() +
    geom_line(data=impact_data,aes(y=tau_hat,x=value)) +
    geom_ribbon(data=impact_data,aes(
      ymin=tau_hat-1.96*tau_hat_se,
      ymax=tau_hat+1.96*tau_hat_se,
      x=value
      ),
      alpha=0.4
    ) +
    geom_histogram(data=data_dist_viz,aes(x=value),color='black',fill='white') +
    facet_grid(type~variable,scale='free') +
    ylab('') +
    xlab('')

  return(g_impact_viz)

}

#===================================
# Find normalized difference
#===================================
#--------------------------
# Define the function
#--------------------------

find_norm_dif <- function(data,var_ls,treat_var){

var_exsp_mean <- paste(var_ls,'=mean(',var_ls,')',sep='',collapse=',')

eval(parse(text=paste('data[,treat:=',treat_var,']',sep="")))

means_ls <- eval(parse(text=paste('data[,.(',var_exsp_mean,'),by=treat]',sep=""))) %>% 
	gather(-treat,key='variable',value='value') %>% 
	spread(key='treat',value='value') %>% 
	setnames(c('0','1'),c('treated_mean','control_mean')) %>% 
	mutate(dif=control_mean-treated_mean)

sd_exsp_var <- paste(var_ls,'=sd(',var_ls,')',sep='',collapse=',')

sds_ls <- eval(parse(text=paste('data[,.(',sd_exsp_var,'),by=treat]',sep=""))) %>% 
	gather(-treat,key='variable',value='value') %>% 
	spread(key='treat',value='value') %>% 
	setnames(c('0','1'),c('treated_sd','control_sd')) %>% 
	mutate(sd_tot=sqrt(treated_sd^2+control_sd^2))

norm_dif <- left_join(means_ls,sds_ls,by='variable') %>% 
	mutate(nor_dif=dif/sd_tot)

return(norm_dif)

}

#--------------------------
# Confidence interval of treatment by precip
#--------------------------

cf_calc <- function(data,year_ls,trt_vars,reg,vcov){
	precip_temp <- data_w[year %in% year_ls,.(min(precip_in),max(precip_in))] %>% unlist()
	precip_seq <- seq(precip_temp[1],precip_temp[2],length=1000)
	eval_mat <- c(rep(1,1000),precip_seq) %>% matrix(nrow=1000)

	impact_seq <- eval_mat %*% reg$coef[trt_vars]
	se_seq <- sqrt(diag(eval_mat%*%vcov[trt_vars,trt_vars]%*%t(eval_mat)))

	data_temp <- data.frame(
		precip=precip_seq,
		beta=impact_seq,
		beta_low=impact_seq-1.96*se_seq,
		beta_high=impact_seq+1.96*se_seq,
		t=impact_seq/se_seq,
		sig=abs(impact_seq/se_seq)>1.96
		)
	return(data_temp)
}

# reg <-reg_store_w[['LT']]
# vcov <- vcov_store_w[['LT']] 
# trt_vars <- c('treat2','treat2_pin')

cf_calc_at <- function(precip_ls,trt_vars,reg,vcov){
	
	precip_len <- length(precip_ls)
	eval_mat <- c(rep(1,precip_len),precip_ls) %>% 
		matrix(nrow=precip_len)

	impact_seq <- eval_mat %*% reg$coef[trt_vars]
	se_seq <- sqrt(diag(eval_mat%*%vcov[trt_vars,trt_vars]%*%t(eval_mat)))

	data_temp <- data.frame(
		precip=precip_ls,
		beta=impact_seq,
		se=se_seq,
		t=impact_seq/se_seq,
		sig=abs(impact_seq/se_seq)>1.96
		)
	return(data_temp)
}


#--------------------------
# Summary statistics
#--------------------------

gen_sum_table <- function(data,var_ls,treat_var){

var_exsp_mean <- paste(var_ls,'=mean(',var_ls,')',sep='',collapse=',')
var_exsp_sd <- paste(var_ls,'=sd(',var_ls,')',sep='',collapse=',')
var_exsp_min <- paste(var_ls,'=min(',var_ls,')',sep='',collapse=',')
var_exsp_max <- paste(var_ls,'=max(',var_ls,')',sep='',collapse=',')

eval(parse(text=paste('data[,treat:=',treat_var,']',sep="")))

means_ls <- eval(parse(text=paste('data[,.(',var_exsp_mean,'),by=treat]',sep=""))) %>% 
	gather(-treat,key='variable',value='value') %>% 
	spread(key='treat',value='value') %>% 
	setnames(c('0','1'),c('treated_mean','control_mean')) %>% 
	mutate(dif=control_mean-treated_mean)

sd_exsp_var <- paste(var_ls,'=sd(',var_ls,')',sep='',collapse=',')

sds_ls <- eval(parse(text=paste('data[,.(',sd_exsp_var,'),by=treat]',sep=""))) %>% 
	gather(-treat,key='variable',value='value') %>% 
	spread(key='treat',value='value') %>% 
	setnames(c('0','1'),c('treated_sd','control_sd')) %>% 
	mutate(sd_tot=sqrt(treated_sd^2+control_sd^2))

norm_dif <- left_join(means_ls,sds_ls,by='variable') %>% 
	mutate(nor_dif=dif/sd_tot)

min_ls <- eval(parse(text=paste('data[,.(',var_exsp_min,'),by=treat]',sep=""))) %>% 
	gather(-treat,key='variable',value='value') %>% 
	spread(key='treat',value='value') %>% 
	setnames(c('0','1'),c('treated_min','control_min')) 

max_ls <- eval(parse(text=paste('data[,.(',var_exsp_max,'),by=treat]',sep=""))) %>% 
	gather(-treat,key='variable',value='value') %>% 
	spread(key='treat',value='value') %>% 
	setnames(c('0','1'),c('treated_max','control_max')) 

table_sum <- norm_dif %>% 
	left_join(.,min_ls,by='variable') %>% 
	left_join(.,max_ls,by='variable') %>% 
	data.table()

table_sum[,mean_sd_tr:=paste(round(treated_mean,digits=2),' (',round(treated_sd,digits=2),')',sep='')]
table_sum[,minmax_sd_tr:=paste('(',round(treated_min,digits=2),',',round(treated_max,digits=2),')',sep='')]
table_sum[,mean_sd_co:=paste(round(control_mean,digits=2),' (',round(control_sd,digits=2),')',sep='')]
table_sum[,minmax_sd_co:=paste('(',round(control_min,digits=2),',',round(control_max,digits=2),')',sep='')]

return(table_sum[,.(variable,mean_sd_tr,minmax_sd_tr,mean_sd_co,minmax_sd_co,round(dif,digits=2),round(nor_dif,digits=2))])

}

