#---------- PRE-PROCESSING DATA -------------

# Install once, then keep these commented out
#install.packages(c("readxl", "psych", "dplyr", "readr"))

library(psych)
library(readxl)
library(dplyr)
library(readr)

# ---- 1. Read raw data and keep user 11 ----
path  <- "C:\\Users\\SUMANA MITHIYA\\Downloads\\facesurvey-data.xlsx"
data1 <- read_xlsx(path)
data2 <- subset(data1, user == 11)

dim(data1)
dim(data2)
unique(data1$user)
str(data2)

# ---- 2. Missing -> 0, present -> 1 for the flag columns ----
flag_cols <- c("asymmetry", "cap", "dark", "medical",
               "facial", "scar", "tape", "blurry")
for (v in flag_cols) {
  data2[[v]] <- ifelse(is.na(data2[[v]]), 0, 1)
}

# ---- 3. Recode warm_photo ----
data2 <- data2 %>%
  mutate(warm_photo = recode(warm_photo,
                             "Very cold"   = "1",
                             "Slight cold" = "2",
                             "Neutral"     = "3",
                             "Slight warm" = "4",
                             "Very warm"   = "5"))

# ---- 4. Clean the image code FIRST ----
data2$image <- sub("F.*", "", data2$image)

# ---- 5. Find duplicate image IDs and look at them ----
counts <- as.data.frame(table(data2$image))
counts[counts$Freq > 1, ]

dup_ids <- data2$image[duplicated(data2$image)]
data2[data2$image %in% dup_ids, ]

# ---- 6. Remove duplicates (keep the LAST rating of each image) ----
data <- data2[!duplicated(data2$image, fromLast = TRUE), ]
# To keep the FIRST rating instead, use:
# data <- data2[!duplicated(data2$image), ]

# ---- 7. Check ----
dim(data2)                    # before
dim(data)                     # after
sum(duplicated(data$image))   # must be 0

# ---- 8. Recode gender ----
data$gender <- recode(data$gender,
                      "Female" = 2,
                      "Male"   = 1,
                      "Unsure" = 1.5)
table(data$gender, useNA = "ifany")

# ---- 9. Save ----
write.csv(data, "face_survey_data11.csv", row.names = FALSE)

# ---- 10. Read the cleaned file back and view ----
data <- read_csv("face_survey_data11.csv")
View(data)
table(substr(data$image, 1, 3))

write.csv(data,
          "C:/Users/SUMANA MITHIYA/Downloads/face_survey_data11.csv",
          row.names = FALSE)
