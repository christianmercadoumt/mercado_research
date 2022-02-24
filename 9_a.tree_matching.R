# 1 make a data frame version
pgp_data_all_df <- data.frame(pgp_data_all)

# 2 add some variables for tracking
pgp_data_all_df$unique_entry <- 1:nrow(pgp_data_all_df)
pgp_data_all_df$match_type <- "none"
pgp_data_all_df$next_measure <- NA

# 3. next, run through the settings
cases <- unique(pgp_data_all_df$SETTING_ID)

for (test_case in cases){
  #test_case <- "01160714020002"
  # subset to setting_id
  test_set <- pgp_data_all_df[pgp_data_all_df$SETTING_ID==test_case &
                                is.na(pgp_data_all_df$SUBPLOT) & # don't care about subplot trees
                                !is.na(pgp_data_all_df$LIVE_DEAD),] #drop empty plot records
  # look at each entry
  for (row in 1:nrow(test_set)){
    # pull candidate matching records (same plot, next measurement)
    candidates <- test_set[test_set$MEASUREMENT_NO==(test_set$MEASUREMENT_NO[row]+1) & 
                             test_set$cluster==test_set$cluster[row] &
                             test_set$PLOT==test_set$PLOT[row],]
    if (nrow(candidates)>0){ # only bother if later measures exist
      # change next_measure to non-missing (for tracking purposes)
      pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- 0
      # look for a TAG_ID+SPECIES+DIAMETER match
      match_tsd <- candidates[(candidates$TAG_ID==test_set$TAG_ID[row]) &
                                (candidates$SPECIES_SYMBOL==test_set$SPECIES_SYMBOL[row]) &
                                (candidates$DIAMETER>=round(test_set$DIAMETER[row]*.95,1)),
                              "unique_entry"]
      # if the focal record has a distance and azimuth then try to match by 
      #  (1) D&A or (2) Tag, species, DBH
      # if only an azimuth or only a distance, then try to match by
      #  (1) D&Tag or A&Tag or (2) Tag, species, DBH
      # if neither azimuth nor distance, then try to match by
      #  (1) Tag, species, DBH
      if (!is.na(test_set$AZIMUTH[row]) & !is.na(test_set$DISTANCE[row])){ 
        # If dist and az are non-missing, then look for a matching dist and az
        match_az <- candidates[(!is.na(candidates$AZIMUTH) & candidates$AZIMUTH==test_set$AZIMUTH[row]) &
                                 (!is.na(candidates$DISTANCE) & candidates$DISTANCE==test_set$DISTANCE[row]),
                               "unique_entry"]
        if (length(match_az)==1){
          # if there is ONE dist/az match, take it
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_az
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "AD"
        } else if (length(match_tsd)==1) {
          # if there isn't a dist/az match, then take the TSD match, if there is ONE
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_tsd
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "TSD"
        }
      } else if (!is.na(test_set$DISTANCE[row])){ 
        # if dist is non-missing but az is missing, look for distance, spp, tag
        match_dst <- candidates[(!is.na(candidates$DISTANCE) & candidates$DISTANCE==test_set$DISTANCE[row]) &
                                  (candidates$SPECIES_SYMBOL==test_set$SPECIES_SYMBOL[row]) &
                                  (candidates$TAG_ID==test_set$TAG_ID[row]),
                                "unique_entry"]
        if (length(match_dst)==1){
          # if there is ONE dst match, take it
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_dst
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "DST"
        } else if (length(match_tsd)==1) {
          # if there isn't ONE dst match, take a TSD match, if there is ONE
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_tsd
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "TSD"
        }
      } else if (!is.na(test_set$AZIMUTH[row])){
        # if dist is missing but az is not missing, look for az, spp, tag match
        match_ast <- candidates[(!is.na(candidates$AZIMUTH) & candidates$AZIMUTH==test_set$AZIMUTH[row]) &
                                  (candidates$SPECIES_SYMBOL==test_set$SPECIES_SYMBOL[row]) &
                                  (candidates$TAG_ID==test_set$TAG_ID[row]),
                                "unique_entry"]
        if (length(match_ast)==1){
          # if ONE ast match, take it
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_ast
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "AST"
        } else if (length(match_tsd)==1) {
          # if not, then take TSD if it there's ONE
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_tsd
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "TSD"
        }
      } else { 
        # if both dist and az missing, look for a TSD match
        if (length(match_tsd)==1) {
          pgp_data_all_df$next_measure[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- match_tsd
          pgp_data_all_df$match_type[pgp_data_all_df$unique_entry==test_set$unique_entry[row]] <- "TSD"
        }
      }
    } # end check for later measurements
  } # end row loop
} # end cases loop

# 4. look at result: match type by whether or not there's a next measurement
table(pgp_data_all_df$match_type,is.na(pgp_data_all_df$next_measure))

pgp_data_all_df[is.na(pgp_data_all_df$next_measure) & pgp_data_all_df$match_type=="TSD",]
# this is a tree that is 2' tall and without a DBH: should be coded as a subplot tree
#  with an appropriate TPA_EQUIV


# 5. add a unique tree identifier
uid <- 1
pgp_data_all_df$unique_tree_id <- NA
for (row in 1:nrow(pgp_data_all_df)){
  if (is.na(pgp_data_all_df$unique_tree_id[row])){
    pgp_data_all_df$unique_tree_id[row] <- uid
    uid <- uid + 1
  }
  if (!is.na(pgp_data_all_df$next_measure[row])){
    pgp_data_all_df$unique_tree_id[pgp_data_all_df$unique_entry==pgp_data_all_df$next_measure[row]] <-
      pgp_data_all_df$unique_tree_id[row]
  }
}

# 6. look at unique tree numbers
reps <- table(pgp_data_all_df$unique_tree_id)
reps[which.max(reps)]
pgp_data_all_df[pgp_data_all_df$unique_tree_id==5501,]


# 7. calculate bai
pgp_data_all_df$bai.calc <- NA
for (row in 1:nrow(pgp_data_all_df)){
  if (!is.na(pgp_data_all_df$next_measure[row]) &
      pgp_data_all_df$next_measure[row]!=0 &
      pgp_data_all_df$LIVE_DEAD[row]=="L"){
    next_obs <- pgp_data_all_df[pgp_data_all_df$unique_entry==pgp_data_all_df$next_measure[row],]
    if (next_obs$LIVE_DEAD=="L"){
      pgp_data_all_df$bai.calc[row] <- (next_obs$DIAMETER^2 - pgp_data_all_df$DIAMETER[row]^2)/
                                         (next_obs$myear - pgp_data_all_df$myear[row])*
                                              pi/4/144
    }
  }
}

pgp_data_all_df[pgp_data_all_df$unique_tree_id==5501,]

# 8. counts

nrow(pgp_data_all_df[!is.na(pgp_data_all_df$bai.calc),]) 
# 16594 increments
nrow(pgp_data_all_df[!is.na(pgp_data_all_df$bai.calc) & 
                       pgp_data_all_df$SPECIES_SYMBOL=="LAOC",]) 
# 7239 larch increments
nrow(pgp_data_all_df[!is.na(pgp_data_all_df$bai.calc) &
                       pgp_data_all_df$bai.calc>=0 &
                       pgp_data_all_df$SPECIES_SYMBOL=="LAOC",]) 
# 7217 larch increments of 0+

length(unique(pgp_data_all_df$unique_tree_id[!is.na(pgp_data_all_df$bai.calc) & 
                                               pgp_data_all_df$SPECIES_SYMBOL=="LAOC"]))
# larch increments taken on 2388 unique trees



# 8. return to a tibble format
bai.calc <- tibble(pgp_data_all_df)