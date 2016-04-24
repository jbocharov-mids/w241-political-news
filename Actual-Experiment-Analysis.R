library(lubridate)
library(plyr)

raw_data <- read.csv("241_Project__Actual_Experiment.csv", na.strings='')

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
    "Q45_7" = "agreement_AP_treatment",
    "Q45_9" = "credibility_AP_treatment",
    "Q26_7" = "agreement_AP_control",
    "Q26_9" = "credibility_AP_control",
    "DO.BL.Treatment" = "q_order_treatment",
    "DO.BL.Control" = "q_order_control",
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
  
  q_order <- unique(c(levels(working_data$DO.BL.Control),levels(working_data$DO.BL.Treatment)))
  
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
  
  agreement_ap_raw <- merge_responses(
    working_data$agreement_AP_control,
    working_data$agreement_AP_treatment
  )
  
  credibility_ap_raw <- merge_responses(
    working_data$credibility_AP_control,
    working_data$credibility_AP_treatment
  )
  
  q_order_raw <- merge_responses(
    as.character(working_data$q_order_control),
    as.character(working_data$q_order_treatment)
  )
  
  working_data$agreement_fox <- to_likert_factor(agreement_fox_raw)
  working_data$credibility_fox <- to_likert_factor(credibility_fox_raw)
  working_data$agreement_huff <- to_likert_factor(agreement_huff_raw)
  working_data$credibility_huff <- to_likert_factor(credibility_huff_raw)
  working_data$agreement_ap <- to_likert_factor(agreement_ap_raw)
  working_data$credibility_ap <- to_likert_factor(credibility_ap_raw)
  
  working_data$q_order <- as.factor(ifelse(substring(q_order_raw,1,7) %in% c("Q45|Q20", "Q26|Q18"), "Fox", "Huff"))
  
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
      "agreement_ap",
      "credibility_ap",
      "agreement_huff", 
      "credibility_huff", 
      "agreement_fox", 
      "credibility_fox",
      "q_order",
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
