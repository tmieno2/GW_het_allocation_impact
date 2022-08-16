######################################
# collection of functions 
######################################
# written by Taro Mieno on 04/09/2016






#/*=================================================*/
#' # Download and process gridMET data
#/*=================================================*/
#' Used in 0_5_get_weather.R
#' Objective: download gridMET data

get_grid_MET <- function(var_name, year) {
  # Ex)
  # var_name = "srad"; year = 2012

  target_url <- 
    paste0(
      "http://www.northwestknowledge.net/metdata/data/",
      var_name, "_", year,
      ".nc"
    )

  # = modified code = #
  file_name <-
     paste0(here("Shared/Data/gridMET-historical/"),
      var_name, "_", year,
      ".nc"
  )

  if (!file.exists(file_name)) {
    downloader::download(
      url = target_url,
      destfile = file_name,
      mode = 'wb'
    )
  }

}


# /*=================================================*/
#' # Extract values from Raster  
# /*=================================================*/

#' This function extract values from gridMET data for each point data of well locations,
#' and summarise those values by each well-id and year


get_in_values_gridMET <- function(var_name, year){
  # Ex) 
  # var_name = "pet"; year = 2007

  # /*===== read raster data ======*/
  file_name <- paste0(var_name, "_", year, ".nc")
  print(paste0("working on ", file_name))
  temp_gmet <- terra::rast(here(paste0("Shared/Data/gridMET-historical/",file_name)))

  # /*===== get in-season days ======*/
  # + days since "1900-01-01"
  start_day <- (ymd(paste0(year, "-04-01")) - ymd("1900-01-01")) %>% as.numeric
  end_day <- (ymd(paste0(year, "-09-30")) - ymd("1900-01-01")) %>% as.numeric 

  ls_in_day <- seq(start_day, end_day) %>% as.character

  # /*===== Extract in-season daily weather values for site-level data (well)  ======*/
  temp_res <- terra::extract(temp_gmet, vect(unique_well_sf)) %>%
    data.table() %>%
    # --- select only data for in-season days --- #
    setnames(names(.)[-1], gsub(".*=", "", names(.)[-1])) %>%
    .[,c("ID", ls_in_day), with=FALSE]
 
  res_return <- temp_res %>%
    melt(id.vars = "ID", variable.name = "day") %>%
    .[,`:=`(
      varibale = var_name,
      year = year
      )]

  return(res_return)
}


# /*=================================================*/
#' # GDD calculation
# /*=================================================*/
# tmmx = 80
# tmmn = 56
# base_temp = 50

get_gdd <- function(tmmx, tmmn, base_temp) {
  
  # /*===== check =====*/
  tmmx <- pmin(tmmx, 86)
  tmmn <- pmax(tmmn, 50)
  # /*===== GDD calculation  =====*/
  gdd <- pmax((tmmx+tmmn)/2-50, 0)

  return(gdd)
}


# /*=================================================*/
#' # Get ssurgo data
# /*=================================================*/
# This function lets you download SSURGO dataset 


get_ssurgo_props <- function(field, vars, summarize = FALSE) {
  # field=unique_well_sp; vars = c("sandtotal_r", "claytotal_r")
  # Get SSURGO mukeys for polygon intersection
  ssurgo_geom <-
    SDA_spatialQuery(
      geom = field,
      what = "geom",
      db = "SSURGO",
      geomIntersection = TRUE
    ) %>%
    st_as_sf()
    # mutate(
    #   area = as.numeric(st_area(.)),
    #   area_weight = area / sum(area)
    # )

  # Get soil properties for each mukey
  mukeydata <- 
    get_SDA_property(
      property = vars,
      method = "Weighted Average",
      mukeys = ssurgo_geom$mukey,
      top_depth = 0,
      bottom_depth = 150
    ) %>%
    data.table()

  ssurgo_data <- left_join(ssurgo_geom, mukeydata, by = "mukey")
  
  if (summarize == TRUE) {
    ssurgo_data_sum <-
      ssurgo_data %>%
      data.table() %>%
      .[,
        lapply(.SD, weighted.mean, w = area_weight),
        .SDcols = vars
      ]
    return(ssurgo_data_sum)
  } else {
    return(ssurgo_data)
  }
}



#/*=================================================*/
#' # Prepare a data set for prediction (ir share)
#/*=================================================*/
#' used in 1_regression_analysis.R
# data <-temp$share_data[[1]]
# vars <- c("balance_avg", "days_ab_35_avg")

gen_pred_data_is <- function(data, vars) {

  var_1 <- vars[1]
  var_2 <- vars[2]

  return_data <- 
    expand.grid(
      var_1 = quantile(data[, ..var_1] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      var_2 = quantile(data[, ..var_2] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      sat = 
        seq(
          min(data[, sat], na.rm = TRUE),
          max(data[, sat], na.rm = TRUE),
          length = 50
        )
    ) %>% 
    data.table() %>% 
    setnames(
      c("var_1", "var_2"), 
      vars
    )

  return(return_data)

}

#/*=================================================*/
#' # Prepare a data set for prediction (total impact)
#/*=================================================*/
#' used in 1_regression_analysis.R
# data <-temp$share_data[[1]]
# vars <- c("balance_avg", "days_ab_35_avg")

# data <- all_results$data[[1]]
# data_avg <- all_results$share_data[[1]]
# vars <- c("balance", "days_ab_35")
# vars_avg <- c("balance_avg", "days_ab_35_avg")

gen_pred_data_total <- function(data, vars, data_avg, vars_avg, sat_ls) {

  var_1 <- vars[1]
  var_2 <- vars[2]
  var_avg_1 <- vars_avg[1]
  var_avg_2 <- vars_avg[2]

  return_data <- 
    expand.grid(
      var_1 = 
        seq(
          min(data[, ..var_1], na.rm = TRUE),
          max(data[, ..var_1], na.rm = TRUE),
          length = 50
        ),
      var_2 = 
        seq(
          min(data[, ..var_2], na.rm = TRUE),
          max(data[, ..var_2], na.rm = TRUE),
          length = 50
        ),
      var_avg_1 = quantile(data_avg[, ..var_1] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      var_avg_2 = quantile(data_avg[, ..var_2] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      sat = sat_ls
    ) %>% 
    data.table() %>% 
    setnames(
      c("var_1", "var_2", "var_avg_1", "var_avg_2"), 
      c(vars, vars_avg)
    ) %>% 
    .[, 
      sat_cat := cut(
        sat, 
        breaks = c(0, 0.01, sat_breaks),
        include.lowest = TRUE
      )  
    ]

  return(return_data)

}





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
    setnames(var_name,'value') %>% #value is a sequence of target variable for prediction
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

