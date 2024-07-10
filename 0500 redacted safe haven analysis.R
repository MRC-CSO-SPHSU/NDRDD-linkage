rm(list = ls())

wd <- "[redacted file path]"
setwd(wd)
dir()
library("dplyr")
library("GGMnonreg")
library("igraph")

###Link the drd, smr and pis data
conditions_wide_ndrdd       <- base::readRDS(paste0(wd,"/Research/data_objects/conditions_wide_ndrdd.rds"))
conditions_wide_smr01_smr04 <- base::readRDS(paste0(wd,"/Research/data_objects/conditions_wide_smr01_smr04.rds"))
conditions_wide_pis         <- base::readRDS(paste0(wd,"/Research/data_objects/conditions_wide_pis.rds"))

conditions_only <- conditions_wide_ndrdd[-c(14,240)]

full_link <- left_join(conditions_only,
                       conditions_wide_smr01_smr04)
dim(full_link)
full_link <- left_join(full_link,
                       conditions_wide_pis)

names(full_link)[1:15]
###Remove personal IDs and non-condition variables
full_link$patient_id                     <- NULL
full_link$usual_council_area_of_res_p    <- NULL
full_link$sex                            <- NULL
full_link$ethnicity_p                    <- NULL
full_link$marital_status_p               <- NULL
full_link$no_children_under_16_p         <- NULL
full_link$no_child_lived_with_under_16_p <- NULL
full_link$usual_council_area_of_res_p    <- NULL
full_link$sex                            <- NULL
full_link$p_ethnicity                    <- NULL
full_link$p_marital_status               <- NULL
full_link$p_no_children_under_16         <- NULL
full_link$p_no_child_lived_with_under_16 <- NULL
full_link$yod                            <- NULL
full_link$yor                            <- NULL
full_link$scsimd2012quintile             <- NULL
full_link$scsimd2009v2quintile           <- NULL
full_link$scsimd2016quintile             <- NULL
full_link$hbacd                          <- NULL

###Set NAs to zero. 
#   This works on the basis that those not appearing in e.g. the SMR04 did not have a psych condition.
#   And they contribute zeroes to the co-occurrence of psych conditions with other conditions.
full_link[is.na(full_link)] <- 0
col_sums.full <- colSums(full_link)
###Include only conditions with at least 20 occurrences
# This stabilises the estimation time
trim_link <- full_link[,colSums(full_link) > 20]

Y <- as.matrix(trim_link)
####Run co-occurence model
ggminf   <- ggm_inference(Y, cores = 4)

#Save to file
colnames(ggminf$wadj) <- colnames(Y)
rownames(ggminf$wadj) <- colnames(Y)
ggminf$wadj <- round(ggminf$wadj, 2)
write.csv(ggminf$wadj, file = paste0(wd,"/Results/output/full_link weighted adjacency with obs above 20.csv"))

#Confidence intervals
ggminfci <- ggm_inference(Y, boot = T,
                          method = "spearman",
                          B = 100)
confint(ggminfci)

write.csv(confint(ggminfci), file = paste0(wd,"/Results/output/full_link weighted adjacency CIs with obs above 20.csv"))

###Proportion of bootstrap samples where edges were included
ggmeip <- eip(Y,method = "spearman",
                          B = 100)
ggmeip

write.csv(ggmeip$eip, file = paste0(wd,"/Results/output/full_link weighted adjacency bootstrap proportions with obs above 20.csv"))
