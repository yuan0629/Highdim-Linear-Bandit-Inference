d <- read.csv(here::here("Real data", "IWPC1.csv"),
              check.names = FALSE, na.strings = c("", "NA"))
d <- d[!is.na(d[["PharmGKB Subject ID"]]) &
         !is.na(d[["Therapeutic Dose of Warfarin"]]), ]

x <- function(n) d[[n]]
yes <- function(n) as.integer(!is.na(x(n)) & x(n) == 1)
mis <- function(n) as.integer(is.na(x(n)))
bin <- function(n) cbind(yes(n), mis(n))
dum <- function(n, v) sapply(v, function(a)
  as.integer(!is.na(x(n)) & trimws(as.character(x(n))) == a))

# Median-impute height and weight and retain their missing indicators.
h <- x("Height (cm)"); hm <- mis("Height (cm)")
w <- x("Weight (kg)"); wm <- mis("Weight (kg)")
h[is.na(h)] <- median(h, na.rm = TRUE)
w[is.na(w)] <- median(w, na.rm = TRUE)

# Eight treatment-indication indicators plus one missing indicator.
ind <- "Indication for Warfarin Treatment"
I <- sapply(1:8, function(k)
  as.integer(grepl(paste0("(^|[^0-9])", k, "([^0-9]|$)"),
                   ifelse(is.na(x(ind)), "", x(ind)))))

clinical <- c(
  "Diabetes",
  "Congestive Heart Failure and/or Cardiomyopathy",
  "Valve Replacement"
)

med <- c(
  "Aspirin",
  "Acetaminophen or Paracetamol (Tylenol)",
  "Was Dose of Acetaminophen or Paracetamol (Tylenol) >1300mg/day",
  "Simvastatin (Zocor)", "Atorvastatin (Lipitor)",
  "Fluvastatin (Lescol)", "Lovastatin (Mevacor)",
  "Pravastatin (Pravachol)", "Rosuvastatin (Crestor)",
  "Cerivastatin (Baycol)", "Amiodarone (Cordarone)",
  "Carbamazepine (Tegretol)", "Phenytoin (Dilantin)",
  "Rifampin or Rifampicin", "Sulfonamide Antibiotics",
  "Macrolide Antibiotics", "Anti-fungal Azoles",
  "Herbal Medications, Vitamins, Supplements"
)

B <- do.call(cbind, lapply(c(clinical, med), function(n) {
  if (n == "Cerivastatin (Baycol)") mis(n)
  else if (n %in% c("Rifampin or Rifampicin", "Macrolide Antibiotics")) yes(n)
  else bin(n)
}))

vkor <- function(site, levels) {
  n <- grep(paste0("^VKORC1 genotype: ", site), names(d), value = TRUE)[1]
  dum(n, levels)
}

dataall <- as.data.frame(cbind(
  dum("Gender", c("male", "female")),
  dum("Race (OMB)", c("White", "Black or African American", "Asian")),
  dum("Ethnicity (OMB)", c("not Hispanic or Latino", "Hispanic or Latino")),
  dum("Age", c("60 - 69", "50 - 59", "40 - 49", "70 - 79",
               "30 - 39", "80 - 89", "90+", "20 - 29", "10 - 19")),
  h, hm, w, wm,
  I, mis(ind),
  B, bin("Current Smoker"),
  dum("Cyp2C9 genotypes",
      c("*1/*1", "*1/*3", "*1/*2", "*2/*2", "*2/*3", "*3/*3",
        "*1/*5", "*1/*13", "*1/*14", "*1/*11", "*1/*6")),
  vkor("-1639", c("A/G", "A/A", "G/G")),
  vkor("497",   c("G/T", "T/T", "G/G")),
  vkor("1542",  c("C/G", "C/C", "G/G")),
  vkor("3730",  c("A/G", "G/G", "A/A"))
))

names(dataall) <- paste0("V", 1:93)
dataall[-c(17, 19)] <- lapply(dataall[-c(17, 19)], as.integer)

dataallnew <- dataall
dataallnew[, 17] <- dataallnew[, 17] / max(dataallnew[, 17])
dataallnew[, 19] <- dataallnew[, 19] / max(dataallnew[, 19])

dose <- x("Therapeutic Dose of Warfarin")
reward <- as.integer(ifelse(dose <= 21, 0, ifelse(dose < 49, 1, 2)))

stopifnot(nrow(dataall) == 5528, ncol(dataall) == 93)
save(dataallnew, dataall, reward,
     file = here::here("Real data", "dataall.RData"))
