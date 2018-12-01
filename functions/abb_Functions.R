#*****************************************************************************************
#                                                                                        *    
#  [To Airbnb?] Custom functions for working with APM and AirDNA data in the             *
#                                                                                        *
#*****************************************************************************************

### Calculate the booking status ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

abbCalcBookStr <- function(book_data){
  
  # If property has more than one status (one daily booking) find summaries
  if (nrow(book_data) > 1){
    
    # Find min and max date
    id_min <- min(book_data$date)
    id_max <- max(book_data$date)
    
    # Divide by status and collapse
    st_v <- book_data$status[1]
    
    for(ss in 2:length(book_data$status)){
      
      if(book_data$status[ss] == book_data$status[[ss - 1]]){
        st_v <- paste0(st_v, book_data$status[[ss]])
      } else {
        st_v <- paste0(st_v, '.' ,book_data$status[[ss]])
      }
    }
    
    # Collapse into list objects
    ss_. <- strsplit(st_v, '[.]')[[1]]
    
    # Grab the first status of each
    sg_. <- substr(ss_.[[1]], 1, 1)
    for(sg in 1:length(ss_.)){
      sg_.[sg] <- substr(ss_.[[sg]], 1, 1)  
    }
    
    # Find location of three types
    id_B <- which(unlist(sg_.) == 'B')
    id_R <- which(unlist(sg_.) == 'R')
    id_A <- which(unlist(sg_.) == 'A')
    
    # Extract
    if (length(id_R) > 0){
      r_. <- ss_.[id_R]
      bookings <- sum(nchar(unlist(r_.)))
    } else {
      bookings <- 0
    }
    
    if (length(id_A) > 0){
      a_. <- ss_.[id_A]
      avails <- unlist(lapply(a_., nchar))
      avail_rate <- sum(avails) / length(book_data$status)
      
    } else {
      
      avail_rate <- 0
      
    }
    
    if (length(id_B) > 0){
      b_. <- ss_.[id_B] 
      
      # Count longest and blocked times
      blocks <- unlist(lapply(b_., nchar))
      
      block_rate <- sum(blocks) / length(book_data$status)
      longest_block <- max(blocks)
      med_block <- median(blocks)
      nbr_block <- length(id_B)
      
    } else {
      
      block_rate <- 0
      longest_block <- 0
      med_block <- 0
      nbr_block <- 0
      
    }
    
    total_days <- length(book_data$status)  
    
  } else {
    
    block_rate <- NA
    longest_block <- NA
    med_block <- NA
    nbr_block <- NA
    days <- NA
    id_min <- NA
    id_max <- NA
    avail_rate <- NA
    bookings <- NA
    
  }  
  
  ## Return Values
  
  return(data.frame(min_date=id_min,
                    max_date=id_max,
                    total_days=total_days,
                    block_rate=round(block_rate, 3),
                    avail_rate=round(avail_rate, 3),
                    longest_block=longest_block,
                    nbr_block=nbr_block,
                    med_block=med_block,
                    bookings=bookings))
}

### Correct the APM data dates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fixAPMDates <- function(x_date){
  
  # x.date:  date vector
  
  ## Convert to character (from factor or numberic)
  
  if (class(x_date) != 'character') x_date <- as.character(x_date)
  
  ## Remove Time suffixes  
  
  x_date <- str_replace_all(x_date, ' 0:00', '')
  
  ## Standardize all years to 2000s  
  
  x_date <- str_replace_all(x_date, '/20', '/')
  
  ## Return values as standardized, British/Australian date format  
  
  as.Date(x_date, "%d/%m/%y")   
  
}

### Set the cleaning counter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setCleanCount <- function(){
  
  # Make counts of initial sizes
  str_orig <- nrow(str_tdf)
  ltr_orig <- nrow(ltr_tdf)
  
  # Create initial data.frame
  clean_df <- data.frame(operation='initial',
                         str=str_orig,
                         ltr=ltr_orig)
  
  # Return
  structure(list(count_df = clean_df,
                 str_running = str_orig,
                 ltr_running = ltr_orig),
            class='clean')

}

### Cleaning counting updater ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

countCleaning <- function(clean_obj, operation){
  
  if (!'clean' %in% class(clean_obj)){
    message('"clean_obj" not an object of class "clean"')
    return(NULL)
  } 
  
  # Count recent cuts  
  str_cut <- clean_obj$str_running - nrow(str_tdf)
  ltr_cut <- clean_obj$ltr_running - nrow(ltr_tdf)
  
  # Build new dataframe
  new_df <- data.frame(operation=operation,
                       str=str_cut,
                       ltr=ltr_cut)
  
  # Add to existing DF
  comb_df <- rbind(clean_obj$count_df, new_df)

  # Return
  structure(list(count_df = comb_df,
                 str_running = nrow(str_tdf),
                 ltr_running = nrow(ltr_tdf)),
            class='clean')

}

### Impute likely bookings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impLikelyBookings <- function(prop_df, 
                              rate_summ){
  
  # Unnest the str prop data
  prop_df <- prop_df %>% 
    tidyr::unnest()
  
  # Get relative booking rate (prop versus other props on those days)
  rsp <- rate_summ %>%
    dplyr::filter(date %in% prop_df$date)
  w_obs <- sum(rsp$obs * rsp$rate) / sum(rsp$obs)
  rel_rate <- (1 - prop_df$block_rate - prop_df$avail_rate) / w_obs
  
  # Estimate likely bookings with dates not currently in property
  rso <- rate_summ %>%
    dplyr::filter(!date %in% prop_df$date)
  lik_book <- round(sum(rso$obs * rso$rate) / sum(rso$obs) * nrow(rso) * rel_rate, 0)
  
  # Add to existing to create likely and limit columns
  p_df <- prop_df %>% 
    dplyr::mutate(lik_bookings = bookings + lik_book) %>%
    dplyr::select(property_id, lik_bookings) %>%
    dplyr::slice(1)
  
  # Return
  p_df
  
}

### Impute likely bookings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impPotentialBookings <- function(prop_df, 
                                 rate_summ){
  
  prop_df <- prop_df %>% tidyr::unnest()
  
  # Unnest the str prop data
  notblock_df <- prop_df %>% 
    tidyr::unnest() %>%
    dplyr::filter(status != 'B')

  block_df <- prop_df %>% 
    tidyr::unnest() %>%
    dplyr::filter(status == 'B')
  
  # Get relative booking rate (prop versus other props on those days)
  rsp <- rate_summ %>%
    dplyr::filter(date %in% notblock_df$date)
  w_obs <- sum(rsp$obs * rsp$rate) / sum(rsp$obs)
  rel_rate <- unique((1 - prop_df$block_rate - prop_df$avail_rate) / w_obs)
  
  # Estimate likely bookings with dates not currently in property
  rso <- rate_summ %>%
    dplyr::filter(date %in% block_df$date)
  pot_book <- round(sum(rso$obs * rso$rate) / sum(rso$obs) * nrow(rso) * rel_rate, 0)
  
  # Add to existing to create likely and limit columns
  p_df <- prop_df %>% 
    dplyr::mutate(pot_bookings = lik_bookings + pot_book) %>%
    dplyr::select(property_id, pot_bookings) %>%
    dplyr::slice(1)
  
  # Return
  p_df
  
}

### Wrapper function to handle all of the imputation and comparison ~~~~~~~~~~~~~~~~~~~~~~

abbImputeCompare <- function(str.df,
                             ltr.df,
                             mod.spec, 
                             match.factor=NULL,
                             split.field=NULL,
                             verbose=FALSE){
  
  init_names <- names(str.df)
  
  ## Split data by field  
  
  # If field is specified
  if (!is.null(split.field)){
    
    str.list <- split(str.df, str.df[ ,split.field])
    ltr.list <- split(ltr.df, ltr.df[ ,split.field])

    # If no field specified  
  } else {
    
    str.list <- list(str.df)
    ltr.list <- list(ltr.df)
    
    if(verbose){
      cat('Data analyzed at global level')
    }
    
  }
  
  ## Loop through split dfs
  
  # Set up capture list
  imp.list <- list()
  
  # Run Loop
  for (il in 1:length(str.list)){
    
    #if(verbose) cat('Imputing and Comparing: ', split.levels[il], '\n')
    
    # Add quartile information to the data
    
    str.list[[il]]$rate_qtl <- makeWtdQtl(str.list[[il]]$med_rate, 
                                          return.type='rank') 
    str.list[[il]]$occ_qtl <- makeWtdQtl(str.list[[il]]$occ_rate, 
                                         return.type='rank') 
    str.list[[il]]$pot_occ_qtl <- makeWtdQtl(str.list[[il]]$pot_occ_rate, 
                                             return.type='rank') 
    
    # Impute long term rents
    imp.temp <- imputeLtrRents(ltr.df=ltr.df, 
                               str.df=str.df, 
                               mod.spec=mod.spec,
                               match.factor=match.factor)
    
    # Add imputed LTRs to the STR data
    str.list[[il]] <- merge(str.list[[il]], imp.temp$imp_rent, by='property_id')
    imp.list[[il]] <- imp.temp
    
    # Impute days on market
    str.list[[il]]$imp_dom <- imputeDOM(str.list[[il]], 
                                        ltr.list[[il]], 
                                        calc.type='median')
    
    # Create imputed LTR Revenue
    str.list[[il]]$ltr_imp_revenue <- (str.list[[il]]$imp_rent * 
                                         (52 - str.list[[il]]$imp_dom / 7))
    
    # Compare revenues 
    #comp.revs <- compareRevenues(str.list[[il]])
    
    # Add revenue comparison fields to str data
    #str.list[[il]] <- merge(str.list[[il]], 
    #                        comp.revs, 
    #                        by='property_id')
    
    
  }
  
  ## Convert list into a df  
  
  str.df <- plyr::rbind.fill(str.list)
  
  ## Add indicator of which field was the split based on  
  
  str.df$split_field <- split.field
  
  ## Return Values  
  
  return(str.df[, c('property_id', names(str.df)[!names(str.df) %in% init_names])])
  
}

### Assign quartile values based on a give vector, weighted if necessary ~~~~~~~~~~~~~~~~~

makeWtdQtl <- function(data.vec, 
                       wgts=rep(1,length(data.vec)),
                       return.type='rank')
{
  
  ## Load required library  
  
  require(Hmisc)
  
  ## Set the adjustment jitter to prevent identical breaks  
  
  adj.jit <- abs(mean(data.vec) / 100000)
  
  ## Calculate the weighted quantiles 0  to 1000  
  
  wtd.qtl <- Hmisc::wtd.quantile(data.vec + runif(length(data.vec), 0, adj.jit), 
                                 weights=wgts, 
                                 probs=seq(0, 1, .01))
  
  ## Fix the ends
  
  # Minimum
  if(wtd.qtl[1] > min(data.vec)){
    wtd.qtl[1] <- min(data.vec) - adj.jit
  }
  
  # Maximum
  if(wtd.qtl[length(wtd.qtl)] < max(data.vec)){
    wtd.qtl[length(wtd.qtl)] <- max(data.vec) + adj.jit
  }
  
  ##  Convert to a vector of quantile indicators 
  
  qtl.vec <- as.numeric(as.factor(cut(data.vec, 
                                      breaks=(wtd.qtl + seq(0, 1, .01) * adj.jit))))
  
  ## Return value
  
  if(return.type == 'rank'){
    return(qtl.vec)
  } else {
    return(wtd.qtl)
  }
  
}

### Cross impute rates and rents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

imputeLtrRents <- function(ltr.df,
                           str.df,
                           mod.spec,
                           match.factor=NULL)
{
  
  ## Arguments
  
  # ltr.df:  data.frame of long term rental observations
  # str.df:  data.frame of airbnb properties
  # ltr.mod.spec:  specification for rent price model
  # str.mod.spec:  specification for airbnb properties
  # clip.field: field to ensure factors match between rent and str
  
  ## Remove those within the clip field that isn't present in both  
  
  if(!is.null(match.factor)){
    
    for(i.cf in 1:length(match.factor)){
      
      # Find the fields that are used to clip
      l.cf <- which(names(ltr.df) == match.factor[i.cf])
      s.cf <- which(names(str.df) == match.factor[i.cf])
      
      # Get IDs for those to be removed
      ltr.df <- ltr.df[ltr.df[[l.cf]] %in% str.df[[s.cf]], ]
      str.df <- str.df[str.df[[s.cf]] %in% ltr.df[[l.cf]], ]
      
      # id.l <- ltr.df[ ,l.cf] %in% names(table(as.character(str.df[ ,s.cf])))
      # id.s <- str.df[ ,s.cf] %in% names(table(as.character(ltr.df[ ,l.cf])))
      # 
      # # Filter out obs missing matched factors  
      # ltr.df <- ltr.df[id.l, ]
      # str.df <- str.df[id.s, ]
      
    }
    
  }
  
  ## Add the monthly factors
  
  str.df$month <- '2015_8'
  
  ## Build regression models for rental values
  
  ltr.mod <- lm(mod.spec, data=ltr.df)
  
  ## Add the predicted values to the short term data
  
  imp.rent <- exp(predict(ltr.mod, str.df))
  
  ## Return Values  
  
  return(list(imp_rent=data.frame(property_id=str.df$property_id,
                                  imp_rent=round(imp.rent, 0)),
              model=ltr.mod))
}



knnImputeWrap <- function(ltr_df, 
                          str_df,
                          knns = c(3, 5, 7)){
  
  # Create Split Lists
  ltr_ <- plyr::dlply(ltr_df, .variables = c('bedrooms', 'bathrooms', 
                                             'type'))
  str_ <- plyr::dlply(str_tdf, .variables = c('bedrooms', 'bathrooms', 
                                              'type'))
  ltr_ <- ltr_[names(ltr_) %in% names(str_)]
  
  k_ <- list()
  
  for (j in knns){
    
    k_[[j]] <- purrr::map2(.x = str_,
                           .y = ltr_,
                           .f = knnImpute,
                           k = j) %>%
      dplyr::bind_rows()
  }
  
  x <- tidyr::spread(k_ %>% bind_rows(), k, weekly_rent)
  if (length(unique(knns)) > 1){
   x$mean <- rowMeans(x[,grepl('k_', names(x))])
  } else {
    x$mean <- x[,grepl('k_', names(x))]
  }
  x$year <- x$mean * 52
  x
}

knnImpute <- function(s_df, l_df, k){
  
  nn <- RANN::nn2(data=l_df[, c('longitude', 'latitude')],
                  query = s_df[, c('longitude', 'latitude')],
                  k = k)
  
  getMed <- function(x,y){median(y$price[x])}
  
  kp_df <- plyr::adply(nn$nn.idx, 1, getMed, y=l_df)
  
  return(data.frame(property_id = s_df$property_id,
                    weekly_rent = kp_df$V1,
                    k = paste0('k_', k)))
  
}


### Impute days on market for the airbnb properties ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

imputeDOM <- function(str.df,
                      ltr.df,
                      calc.type='median'){
  
  ## If median type  
  
  if(calc.type == 'median'){
    dom.qtl <- makeWtdQtl(ltr.df$dom, return.type='raw')
    str.df$imp.dom <- dom.qtl[51]
  }
  
  ## if model type  
  
  if(calc.type == 'model'){
    
    # Save for later
    
  }
  
  ## Return Values
  
  return(round(str.df$imp.dom, 0))
  
}


# Place multiple ggplots into a configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggMultiPlots <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tapply2DF <- function(xData,          # Vector being tapply'd 
                      byField,        # Field to split vector by
                      xFunc,          # Function to apply
                      newName='Var',  # Name of new variable 
                      idName='ID',
                      na.rm=FALSE)    # Name of identification field 
{
  
  ## Execute tapply()
  
  xTable <- as.data.frame(tapply(xData, byField, xFunc, na.rm=na.rm))
  
  ## Add names and new fields
  
  # Give calculated field a name
  names(xTable) <- newName
  
  # Add id field and give it a name
  xTable[ ,2] <- rownames(xTable)
  names(xTable)[2] <- idName
  
  # Reorder columns
  xTable <- xTable[ ,c(2, 1)]
  
  # Remove existing field names
  rownames(xTable) <- 1:nrow(xTable)
  
  ## Return values
  
  return(xTable)
}

### Create comparison table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

abbCreateCompTable <- function(ic.df,
                               split.field=NULL){
  
  if(split.field == 'none'){
    
    str.act <- mean(ic.df$act_pref)
    str.lik <- mean(ic.df$lik_pref)
    str.pot <- mean(ltr.df$pot_pref)
    
    rate.table <- data.frame(ID='all',
                             var=c(str.act, str.lik, str.pot),
                             rev.type=c('Actual', 
                                        'Likely', 
                                        'Potential'))
    
  } else {
    
    # Calculate cross-tab values
    str.act <- tapply2DF(ic.df$act_pref, ic.df[ ,split.field], mean)
    str.lik <- tapply2DF(ic.df$lik_pref, ic.df[ ,split.field], mean)
    str.pot <- tapply2DF(ic.df$pot_pref, ic.df[, split.field], mean)
    
    # Add names
    str.act$rev.type <- 'Actual'
    str.lik$rev.type <- 'Likely'
    str.pot$rev.type <- 'Potential'
    
    # Combine into table
    rate.table <- rbind(str.act, str.lik, str.pot)
    
    # Reorder factors for common split fields
    if(split.field == 'geo_mrkt'){
      
      rate.table$ID <- factor(rate.table$ID, 
                              levels=c('city-core', 'city', 'beach',
                                       'suburban', 'rural'))
    }
    
    if(split.field == 'host_type'){
      rate.table$ID <- factor(rate.table$ID,
                              levels=c('Profit Seeker', 'Opportunistic Sharer', 
                                       'Multi-Platform User', 'Unknown'))
    }
    
  }
  
  ## Return Values
  
  return(rate.table)
  
}

### Create a vector of significance stars for regression results ~~~~~~~~~~~~~~~~~~~~~~~~~

makeStatSig <- function(x){
  x <- as.numeric(x)
  y <- rep('***', length(x))
  y[x > .01] <- '** '
  y[x > .05] <- '*  '
  y[x > .1] <- '   '
  y
}

### Diagnostics for logistic regression models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

logDx <- function(log.model, data, resp.var){
  
  pred <- prediction(predict(log.model, data, type='response'), resp.var)
  auc <- performance(pred, measure='auc')
  ll <- logLik(log.model)
  AIC <- AIC(log.model)
  
  return(list(AIC=AIC,
              logLik=ll,
              auc=auc))
}
