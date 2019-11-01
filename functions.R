#Fb ads summary with breakdown option


fbins_breakdown <- function(date_preset, breakdown="", report_level, time_increment, fb_access_token, account){
  
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", account, "/insights")
  
  #call insights
  content_result <- content(GET(URL,
                                query = list(
                                  access_token = fb_access_token,
                                  date_preset = date_preset,
                                  level = report_level,
                                  fields = "campaign_name, campaign_id, objective, adset_id, adset_name, ad_id, ad_name, impressions, cpm, clicks,ctr, cpc",
                                  time_increment=time_increment,
                                  limit = "1000",
                                  breakdowns = breakdown
                                ),
                                encode = "json",
                                verbose()))
  #extract data and name
  result_df <- data.frame(content_result$data %>% reduce(bind_rows))
  
  # Condition to detect the next page
  if(exists("next", content_result$paging) == TRUE){
    # Checking from the originally returned list
    result <- fromJSON(content_result$paging$`next`)
    result_df <- bind_rows(result_df, as.tibble(result$data))
    
    # Looping through subsequent returned pages
    while(exists("next", result$paging) == TRUE){
      result <- fromJSON(result$paging$`next`)
      result_df <- bind_rows(result_df, as.tibble(result$data))
    }
  }
  result_df <- result_df
}


#Fb ads with active campaign filter option

fbins_summ <- function(date_preset, report_level, time_increment, active, fb_access_token, account){
  
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", account, "/insights")
  
  #call insights
  if(active==TRUE){
    content_result <- content(GET(URL,
                                  query = list(
                                    access_token = fb_access_token,
                                    date_preset = date_preset,
                                    level = report_level,
                                    filtering = "[{'field':'adset.effective_status','operator':'IN','value':['ACTIVE']}]",
                                    fields = "campaign_name, campaign_id, objective, purchase_roas, action_values, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                                    time_increment=time_increment,
                                    limit = "1000"
                                  ),
                                  encode = "json",
                                  verbose()))
    } else{
    content_result <- content(GET(URL,
                                  query = list(
                                    access_token = fb_access_token,
                                    date_preset = date_preset,
                                    level = report_level,
                                    fields = "campaign_name, campaign_id, objective, purchase_roas, action_values, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                                    time_increment=time_increment,
                                    limit = "1000"
                                  ),
                                  encode = "json",
                                  verbose()))
    }
  
  #extract data and name
  result_df <- data.frame(content_result$data %>% reduce(bind_rows))
  
  # Condition to detect the next page
  if(exists("next", content_result$paging) == TRUE){
    # Checking from the originally returned list
    result <- fromJSON(content_result$paging$`next`)
    result_df <- bind_rows(result_df, as.tibble(result$data))
    
    # Looping through subsequent returned pages
    while(exists("next", result$paging) == TRUE){
      result <- fromJSON(result$paging$`next`)
      result_df <- bind_rows(result_df, as.tibble(result$data))
    }
  }
  result_df <- result_df
}


#time range with active campaign filter

fbins_trange <- function(report_level, trange, time_increment, active, fb_access_token, account){
  
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", account, "/insights")
  
  #call insights
  if(active==TRUE){
    content_result <- content(GET(URL,
                                  query = list(
                                    access_token = fb_access_token,
                                    time_range = trange,
                                    level = report_level,
                                    filtering = "[{'field':'adset.effective_status','operator':'IN','value':['ACTIVE']}]",
                                    fields = "campaign_name, campaign_id, objective, purchase_roas, action_values, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                                    time_increment=time_increment,
                                    limit = "1000"
                                  ),
                                  encode = "json",
                                  verbose()))
    } else{
    content_result <- content(GET(URL,
                                  query = list(
                                    access_token = fb_access_token,
                                    time_range = trange,
                                    level = report_level,
                                    fields = "campaign_name, campaign_id, objective, purchase_roas, action_values, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                                    time_increment=time_increment,
                                    limit = "1000"
                                  ),
                                  encode = "json",
                                  verbose()))
    }
  
  #extract data and name
  result_df <- data.frame(content_result$data %>% reduce(bind_rows))
  
  # Condition to detect the next page
  if(exists("next", content_result$paging) == TRUE){
    # Checking from the originally returned list
    result <- fromJSON(content_result$paging$`next`)
    result_df <- bind_rows(result_df, as.tibble(result$data))
    
    # Looping through subsequent returned pages
    while(exists("next", result$paging) == TRUE){
      result <- fromJSON(result$paging$`next`)
      result_df <- bind_rows(result_df, as.tibble(result$data))
    }
  }
  result_df <- result_df
}
