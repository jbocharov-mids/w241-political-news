library(lfe)  # For fixed effects
library(lubridate)
library(plyr)
library(stargazer)

raw_data <- read.csv("241_Project__Actual_Experiment.csv", na.strings='', stringsAsFactors=FALSE)

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
    "Q26" = "age_block",
    "Q8" = "gender",
    "Q12" = "state",
    "Q9" = "education",
    "Q19_1" = "fox_familiar_raw",
    "Q19_2" = "huff_familiar_raw"
  ))

  agree_levels <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
  convert_agree_likert_to_numeric <- function (correctly_labeled, counter_labeled) {
    # One of the label will be empty, simply concatenate and assign new factor levels.
    as_factor = factor(ifelse(is.na(counter_labeled), correctly_labeled, counter_labeled), levels=agree_levels)
    # We just need values, get rid of labels.
    return (as.numeric(as_factor))
  }

  working_data$agreement_fox <- convert_agree_likert_to_numeric( 
    working_data$agreement_fox_correctly_labeled,
    working_data$agreement_fox_counter_labeled
  )

  working_data$credibility_fox <- convert_agree_likert_to_numeric(
    working_data$credibility_fox_correctly_labeled,
    working_data$credibility_fox_counter_labeled
  )

  working_data$agreement_huff <- convert_agree_likert_to_numeric(
    working_data$agreement_huff_correctly_labeled,
    working_data$agreement_huff_counter_labeled
  )

  working_data$credibility_huff <- convert_agree_likert_to_numeric(
    working_data$credibility_huff_correctly_labeled,
    working_data$credibility_huff_counter_labeled
  )

  working_data$agreement_ap <- convert_agree_likert_to_numeric(
    working_data$agreement_AP_control,
    working_data$agreement_AP_treatment
  )

  working_data$credibility_ap <- convert_agree_likert_to_numeric(
    working_data$credibility_AP_control,
    working_data$credibility_AP_treatment
  )

  # Order in which articles are displayed
  q_order_raw <- paste(working_data$q_order_control, working_data$q_order_treatment, sep="")
  working_data$q_order <- as.factor(ifelse(substring(q_order_raw,1,7) %in% c("Q45|Q20", "Q26|Q18"), "fox_first", "huff_first"))

  # Party affiliation
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

  # Treatment or Control
  working_data$treatment <- ifelse(is.na(working_data$agreement_fox_counter_labeled), 0, 1)

  # Recode age blocks to 3 groups - 18-25, 26-34, 35+
  working_data$age_block <- ifelse(!(working_data$age_block %in% c("18-25", "26-34")),"35_and_older", working_data$age_block)
  working_data$age_block <- factor(working_data$age_block, levels = c("18-25", "26-34","35_and_older"))

  # Recode familiarity with articles
  familiar_levels <- c("Not familiar at all", "Slightly familiar", "Moderately familiar", "Very familiar", "Extremely familiar")
  convert_familiar_likert_to_numeric <- function (familiarity) {
    as_factor = factor(familiarity, levels=familiar_levels)
    # We just need values, get rid of labels.
    return (as.numeric(as_factor))
  }
  working_data$fox_familiar <- convert_familiar_likert_to_numeric(working_data$fox_familiar_raw)
  working_data$huff_familiar <- convert_familiar_likert_to_numeric(working_data$huff_familiar_raw)

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
      "age_block",
      "gender",
      "state",
      "education",
      "fox_familiar",
      "huff_familiar"
    )]
  )
}

clean_data <- perform_data_cleaning(raw_data)
clean_data$democrat_treatment = (clean_data$party == "Democrat") * clean_data$treatment
clean_data$republican_treatment = (clean_data$party == "Republican") * clean_data$treatment

clean_data$fox_agreement_lift <- clean_data$agreement_fox - clean_data$agreement_ap
clean_data$fox_credibility_lift <- clean_data$credibility_fox - clean_data$credibility_ap
clean_data$huff_agreement_lift <- clean_data$agreement_huff - clean_data$agreement_ap
clean_data$huff_credibility_lift <- clean_data$credibility_huff - clean_data$credibility_ap

##############################
# Fox news article analysis
##############################

# Agreeability
m1 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party, data = clean_data)
m2 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar, data = clean_data)
m3 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order, data = clean_data)
m4 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender, data = clean_data)
m5 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education), data = clean_data)
m6 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state), data = clean_data)
m7 = felm(fox_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state) + G(age_block), data = clean_data)
stargazer(m1, m2, m3, m4, m5, m6, m7, type = "html",
          dep.var.labels = "Agreement with Fox News article")

# Credibility
m1 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party, data = clean_data)
m2 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar, data = clean_data)
m3 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order, data = clean_data)
m4 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender, data = clean_data)
m5 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education), data = clean_data)
m6 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state), data = clean_data)
m7 = felm(fox_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state) + G(age_block), data = clean_data)
stargazer(m1, m2, m3, m4, m5, m6, m7, type = "html",
          dep.var.labels = "Credibility of Fox News article")

##############################
# Huff Postnews article analysis
##############################

# Agreeability
m1 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party, data = clean_data)
m2 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar, data = clean_data)
m3 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order, data = clean_data)
m4 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender, data = clean_data)
m5 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education), data = clean_data)
m6 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state), data = clean_data)
m7 = felm(huff_agreement_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state) + G(age_block), data = clean_data)
stargazer(m1, m2, m3, m4, m5, m6, m7, type = "html",
          dep.var.labels = "Agreement with Huff Post article")

# Credibility
m1 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party, data = clean_data)
m2 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar, data = clean_data)
m3 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order, data = clean_data)
m4 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender, data = clean_data)
m5 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education), data = clean_data)
m6 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state), data = clean_data)
m7 = felm(huff_credibility_lift ~ democrat_treatment + republican_treatment + party + fox_familiar + huff_familiar + q_order + gender + G(education) + G(state) + G(age_block), data = clean_data)
stargazer(m1, m2, m3, m4, m5, m6, m7, type = "html",
          dep.var.labels = "Credibility of Huff Post article")
