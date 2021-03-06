library(lubridate)
library(plyr)
library(reshape2)

raw_data <- read.csv("241_Project__Pilot_mTurk.csv", na.strings='')

perform_data_cleaning <- function (raw_data) {
  
  # Not used in at runtime, but useful for debugging this function
  questions <- raw_data[1,]
  
  # Remove the question text row
  working_data <- raw_data[ 2:nrow(raw_data), ]
  
  start_time <- ymd_hms(working_data$V8)
  completion_time <- ymd_hms(working_data$V9)
  working_data$time_taken <- as.numeric( completion_time - start_time )
  
  working_data <- working_data[ working_data$time_taken > 100, ]
  
  working_data <- rename(working_data, c(
    "Q18_4" = "agreement_fox_correctly_labeled",
    "Q20_7" = "agreement_fox_counter_labeled",
    "Q18_5" = "credibility_fox_correctly_labeled",
    "Q20_9" = "credibility_fox_counter_labeled",
    "Q19_4" = "agreement_huff_correctly_labeled",
    "Q18_6" = "agreement_huff_counter_labeled",
    "Q19_5" = "credibility_huff_correctly_labeled",
    "Q18_7" = "credibility_huff_counter_labeled",
    "Q26" = "age_block"
  ))
  
  likert <- levels(working_data$agreement_fox_correctly_labeled)
  likert_order <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
  to_likert_factor <- function(ratings) {
    return(
      factor(likert[ratings], levels=likert_order)
    )
  }
  
  merge_responses <- function (correctly_labeled, counter_labeled) {
    return(ifelse(
      is.na(counter_labeled), correctly_labeled, counter_labeled
    ))
  }
  
  agreement_fox_raw <- merge_responses( 
    working_data$agreement_fox_correctly_labeled,
    working_data$agreement_fox_counter_labeled
  )
  
  credibility_fox_raw <- merge_responses(
    working_data$credibility_fox_correctly_labeled,
    working_data$credibility_fox_counter_labeled
  )
  
  agreement_huff_raw <- merge_responses(
    working_data$agreement_huff_correctly_labeled,
    working_data$agreement_huff_counter_labeled
  )
  
  credibility_huff_raw <- merge_responses(
    working_data$credibility_huff_correctly_labeled,
    working_data$credibility_huff_counter_labeled
  )
  
  working_data$agreement_fox <- to_likert_factor(agreement_fox_raw)
  working_data$credibility_fox <- to_likert_factor(credibility_fox_raw)
  working_data$agreement_huff <- to_likert_factor(agreement_huff_raw)
  working_data$credibility_huff <- to_likert_factor(credibility_huff_raw)
  
  working_data <- rename(working_data, c(
    "Q1" = "party_dem_ind_rep",
    "Q2" = "raw_party_loyalty",
    "Q2.1" = "party_dem_rep"
  ))
  
  party <- working_data$party_dem_ind_rep
  
  ##Assign Independents which party they feel more affiliated to
  party[party=="Independent"] <- working_data$party_dem_rep[
    !is.na(working_data$party_dem_rep)
  ] 
  
  working_data$party <- factor(party)
  
  party_loyalty <- working_data$raw_party_loyalty
  
  party_loyalty[is.na(party_loyalty)] <- "Weak"
  working_data$party_loyalty <- factor(party_loyalty, levels=c("Weak", "Moderate", "Strong"))
  
  working_data$treatment <- ifelse(is.na(working_data$agreement_fox_counter_labeled), 0, 1)
  
  working_data$age_block <- ifelse(!(working_data$age_block %in% c("18-25", "26-34")),"35_and_older", levels(working_data$age_block)[working_data$age_block])
  working_data$age_block <- factor(working_data$age_block, levels = c("18-25", "26-34","35_and_older"))
  
  return(
    working_data[,c(
      "mTurkCode", 
      "treatment", 
      "agreement_huff", 
      "credibility_huff", 
      "agreement_fox", 
      "credibility_fox",
      "party",
      "party_loyalty",
      "age_block"
    )]
  )
}

clean_data <- perform_data_cleaning(raw_data)


summary(lm(as.numeric(agreement_fox) ~ party + party_loyalty + treatment, data=clean_data))
summary(lm(as.numeric(credibility_fox) ~ party + party_loyalty + treatment, data=clean_data))
summary(lm(as.numeric(agreement_huff) ~ party + party_loyalty + treatment, data=clean_data))
summary(lm(as.numeric(credibility_huff) ~ party + party_loyalty + treatment, data=clean_data))

impute_agreement <- function(clean_data) {
  mTurkCode <- clean_data$mTurkCode
  treatment <- clean_data$treatment
  agreement_fox <- as.numeric(clean_data$agreement_fox)
  agreement_huff <- as.numeric(clean_data$agreement_huff)
  imputed_agreement_ap <- ( agreement_fox + agreement_huff ) / 2.0
  
  party <- clean_data$party
  party_loyalty <- clean_data$party_loyalty
  
  imputed_agreement <- data.frame( 
    mTurkCode = mTurkCode, 
    treatment = treatment,
    party = party,
    party_loyalty = party_loyalty,
    imputed_ap = imputed_agreement_ap,
    fox = agreement_fox - imputed_agreement_ap,
    huff = agreement_huff - imputed_agreement_ap
  )
  
  return(melt( 
    imputed_agreement,
    id.vars = c("mTurkCode", "treatment", "party", "party_loyalty", "imputed_ap"),
    measure.vars = c("fox", "huff"),
    variable.name = "source",
    value.name = "lift"
  ))
}

imputed_agreement <- impute_agreement(clean_data)

impute_credibility <- function(clean_data) {
  mTurkCode <- clean_data$mTurkCode
  treatment <- clean_data$treatment
  credibility_fox <- as.numeric(clean_data$credibility_fox)
  credibility_huff <- as.numeric(clean_data$credibility_huff)
  imputed_credibility_ap <- ( credibility_fox + credibility_huff ) / 2.0
  
  party <- clean_data$party
  party_loyalty <- clean_data$party_loyalty
  
  imputed_credibility <- data.frame( 
    mTurkCode = mTurkCode, 
    treatment = treatment,
    party = party,
    party_loyalty = party_loyalty,
    imputed_ap = imputed_agreement_ap,
    fox = credibility_fox - imputed_credibility_ap,
    huff = credibility_huff - imputed_credibility_ap
  )
  
  return(melt( 
    imputed_credibility,
    id.vars = c("mTurkCode", "treatment", "party", "party_loyalty", "imputed_ap"),
    measure.vars = c("fox", "huff"),
    variable.name = "source",
    value.name = "lift"
  ))
}

imputed_credibility <- impute_credibility(clean_data)

# The model specs we care about:

summary(lm( lift ~ source + party + source:party, data = imputed_agreement ))

summary(lm( lift ~ source + party + source:party + treatment + treatment:party, data = imputed_agreement ))

summary(lm( lift ~ source + party + source:party, data = imputed_credibility ))

summary(lm( lift ~ source + party + source:party + treatment + treatment:party, data = imputed_credibility ))

