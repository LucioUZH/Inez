# Date: Wednesday Feb 1st
# Title: survival analyses prep for alcohol as covariate

# Load data files
raute_alc_23 <- read_excel("Raute_alcohol_2023.xlsx")

# Data to be used
    # raute_demo_2015
    # raute

# raute alcohol dataset: alter
raute_alc_23$alcohol_freq <- ifelse(raute_alc_23$drinks_day_cat == "1" | raute_alc_23$drinks_day_cat == "Sometimes", "One or less",
                                    ifelse(raute_alc_23$drinks_day_cat == "1+" | raute_alc_23$drinks_day_cat == "2", "Two or less",
                                           ifelse(raute_alc_23$drinks_day_cat == "2+" | raute_alc_23$drinks_day_cat == "3", "Three or less",
                                                  ifelse(raute_alc_23$drinks_day_cat == "3+" | raute_alc_23$drinks_day_cat == "4", "Four or less",
                                                         ifelse(raute_alc_23$drinks_day_cat == "No", "No", "More than four")))))
raute_alc_23 <- raute_alc_23[,2:26]
raute_alc_23 <- raute_alc_23[!is.na(raute_alc_23$alcohol_freq),] #have to delete the NAs in the covariate
write.csv(raute_alc_23, "raute_alc_23.csv")

# Change the file for raute_ individuals
raute_demo_alc <- merge(raute_demo_2015, raute_alc_23[,1], by ="id_2014", all.y=T)

# create covariate matrix with variable "alcohol_freq" for categorical alcohol frequency
cov_mat_alc <- raute_alc_23[,c(1,25)]
cov_mat_alc  <- merge(cov_mat_alc , raute_demo_alc[,1:2], by = 'id_2014', all.x=T)
cov_mat_alc <- MakeCovMat("~ alcohol_freq - 1", data = cov_mat_alc)
cov_mat_alc <- cov_mat_alc[order(cov_mat_alc$ID),]
write.csv(cov_mat_alc, "cov_mat_alc.csv")

# Create file with ids, year, and whether to include yes (1) or no(0)
census_alc <- data.frame(id_2014 = rep(raute_demo_alc$id_2014, each = 9), year = rep(c(2014:2022), times = 97))
census_alc <- merge(census_alc, raute_demo_alc, by = 'id_2014', all.x=T)
census_alc$include <- ifelse((census_alc$birthYear < census_alc$year & (census_alc$deathYear == 0 | census_alc$deathYear >= census_alc$year)), 1, 0)
census_alc <- census_alc[,c(1:2,5)] #latest birth year for individuals is 2015 > so excludes quite a few due to lack of data for this variable
census_alc_wide <- reshape(census_alc, idvar = "id_2014", timevar = "year", direction = "wide")
census_alc_wide  <- census_alc_wide %>% 
  purrr::set_names(c('id_2014', 2014:2022)) # 97 individuals, between 2014 and 2022
rownames(census_alc_wide) <- 1:nrow(census_alc_wide)
write.csv(census_alc_wide, "census_alc_wide.csv")

# Create input matrix
raute_input_alc <- as.data.frame(cbind(raute_demo_alc, census_alc_wide[,-1], cov_mat_alc[, -1]))
check <- DataCheck(raute_input_alc, studyStart = 2014, studyEnd = 2022,
                   autofix = rep(1,7), silent = FALSE) #no problems detected with the data

# Create the BaSTA file & basic plot
basta_alc <- BaSTA::basta(object = raute_input_alc, studyStart = 2014, studyEnd = 2022, niter=50000, model = c("GO"), shape = c("simple"), nsim = 2, parallel = TRUE, ncpus = 2)
summary(basta_2015)
plot(basta_2015, fancy =T, plot.trace=F, main = 'basta_2015', noCI=T) 


