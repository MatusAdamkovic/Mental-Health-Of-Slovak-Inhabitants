# Paper -------------------------------------------------------------------

# https://docs.google.com/document/d/1BC64ACbpjYcs7ptkqteqsXA3rXZ0oHbeAZ4HTTojX_4/edit?usp=sharing


# TO DO -------------------------------------------------------------------

# Weighting
# e.g.: https://github.com/iropovik/sexualneObtazovanieVS/blob/master/analysis.R

# Imputation
# use mice or missRanger (probably missRanger) for descriptives; fiml for models

# Split to two/three papers?


# Data --------------------------------------------------------------------

# https://osf.io/hqvbe/


# Load libraries ----------------------------------------------------------

list.of.packages <- c("corrplot", "ggplot2", "lme4", "lavaan", "missRanger", "psych", "readxl", "stringr", "tidyverse", "writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE))


# Data wrangling ----------------------------------------------------------

# Read data
data_files <- paste0("dataWave", 1:9, "_cleaned.xlsx")
data_list <- lapply(data_files, read_excel)

# Select variables
vars_specs <- list(
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("pcl", 1:20), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder",
    paste0("qids", 1:16), paste0("pcl", 1:20), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("covidStress", 1:20)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("pcl", 1:20), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("covidStress", 1:20), paste0("pss", 1:10)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("pcl", 1:20), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("igd", 1:4), paste0("covidStress", 1:18), paste0("pss", 1:5)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("igd", 1:4), paste0("covidStress", 1:18), paste0("pss", 1:10), paste0("warStress", c(1,2,4,7,12,13,14,15,18))),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("pcl", 1:20), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("igd", 1:4), paste0("covidStress", 1:18), paste0("pss", 1:10), paste0("warStress", 1:10)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("igd", 1:4), paste0("covidStress", 1:18), paste0("pss", 1:10), paste0("warStress", 1:10)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder", 
    paste0("qids", 1:16), paste0("pcl", 1:20), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("igd", 1:4), paste0("covidStress", 1:18), paste0("pss", 1:10), paste0("warStress", 1:10)),
  c("id", "gender", "age", "edu", "ecoStatus", "partnerStatus", "sesLadder",
    paste0("qids", 1:16), paste0("gad", 1:7), paste0("isi", 1:7), paste0("audit", 1:10), paste0("igd", 1:4), paste0("pss", 1:10), paste0("warStress", 1:10))
)

data_list <- mapply(function(data, cols) select(data, all_of(cols)), data_list, vars_specs, SIMPLIFY = FALSE)


# Recode variables
for (i in 1:length(data_list)) {
  data <- data_list[[i]]
  
  if("id" %in% colnames(data) && (i >= 1 && i <= 3)) {
    data$id <- as.character(data$id)
  }
  
  if("sesLadder" %in% colnames(data) && i == 4) {
    data$sesLadder <- 11 - data$sesLadder
  }
  
  if("isi4" %in% colnames(data)) {
    data$isi4 <- 6 - data$isi4
  }
  
  if("audit1" %in% colnames(data)) {
    data$audit1[data$audit1 == 1] <- NA
  }
  
  if("audit9" %in% colnames(data)) {
    data$audit9[data$audit9 == 2] <- 3
    data$audit9[data$audit9 == 3] <- 5
  }
  
  if("audit10" %in% colnames(data)) {
    data$audit10[data$audit10 == 2] <- 3
    data$audit10[data$audit10 == 3] <- 5
  }
  
  if("edu" %in% colnames(data)) {
    original_edu <- data$edu
    if (i >= 1 && i <= 3) {
      data$edu[original_edu == 2] <- 1
      data$edu[original_edu == 3] <- 2
      data$edu[original_edu >= 4] <- 3
    } else if (i >= 4 && i <= 9) {
      data$edu[original_edu == 3] <- 2
      data$edu[original_edu >= 4] <- 3
    }
  }
  
  if("ecoStatus" %in% colnames(data)) {
    original_ecoStatus <- data$ecoStatus
    data$ecoStatus <- as.character(data$ecoStatus)
    if (i >= 1 && i <= 3) {
      data$ecoStatus[original_ecoStatus == "2" | original_ecoStatus == "3"] <- "1"
      data$ecoStatus[original_ecoStatus >= "4"] <- "2"
    } else if (i == 4) {
      data$ecoStatus[original_ecoStatus >= "2" & original_ecoStatus <= "4"] <- "1"
      data$ecoStatus[original_ecoStatus == "1" | original_ecoStatus >= "5"] <- "2"
    } else if (i >= 5 && i <= 9) {
      detected <- str_detect(original_ecoStatus, "2|3|4") & !is.na(original_ecoStatus)
      data$ecoStatus[detected] <- "1"
      data$ecoStatus[!detected & !is.na(original_ecoStatus)] <- "2"
    }
  }
  
  if("partnerStatus" %in% colnames(data)) {
    original_status <- data$partnerStatus
    if (i >= 1 && i <= 3) {
      data$partnerStatus[original_status == 1] <- 2
      data$partnerStatus[original_status == 2] <- 1
      data$partnerStatus[original_status >= 3] <- 2
    } else if (i >= 4 && i <= 9) {
      data$partnerStatus[original_status == 1] <- 2
      data$partnerStatus[original_status == 2] <- 1
      data$partnerStatus[original_status == 3] <- 1
      data$partnerStatus[original_status == 4] <- 2
    }
  }
  
  data_list[[i]] <- data
}

# gender
# 1 = male
# 2 = female
# 3 = other
# 4 = don't want no answer

# edu
# 1 = no edu or primary school
# 2 = high school
# 3 = university

# ecoStatus
# 1 = working
# 2 = not working

# partnerStatus
# 1 = in relationship
# 2 = single

lapply(data_list, function(data) table(data$gender))
lapply(data_list, function(data) describe(data$age)[, c(3,4,8,9)])
lapply(data_list, function(data) table(data$edu))
lapply(data_list, function(data) table(data$ecoStatus))
lapply(data_list, function(data) table(data$partnerStatus))


# Extract ids
ids <- list(data1 = data_list[[1]]$id, data2 = data_list[[2]]$id, data3 = data_list[[3]]$id, 
            data4 = data_list[[4]]$id, data5 = data_list[[5]]$id, data6 = data_list[[6]]$id, 
            data7 = data_list[[7]]$id, data8 = data_list[[8]]$id, data9 = data_list[[9]]$id)


# Loop through each pair of datasets to check the id overlap
for (i in 1:(length(ids) - 1)) {
  for (j in (i + 1):length(ids)) {
    overlap <- length(intersect(ids[[i]], ids[[j]]))
    proportion <- overlap / nrow(data_list[[j]])
    cat("Between", names(ids)[i], "and", names(ids)[j], ":\n")
    cat("  - Overlapping 'id' count:", overlap, "\n")
    cat("  - Proportion relative to", names(ids)[j], ":", proportion, "\n\n")
  }
}

# Count missings (percentage)
lapply(data_list, function(x) sum(is.na(x))/(nrow(x)*(ncol(x)-7)))

# Check correlations
lapply(data_list, function(x) round(lowerCor(x[, 8:(ncol(x)-1)]), 2))


# Add suffixes to each dataframe in data_list
add_suffix <- function(data, suffix) {
  colnames(data)[colnames(data) != "id"] <- paste0(colnames(data)[colnames(data) != "id"], suffix)
  return(data)
}
data_list <- map2(data_list, paste0("_t", 1:length(data_list)), add_suffix)


# Join datasets into a wide format
dataWide <- reduce(data_list, ~full_join(.x, .y, by = "id"))


#Randomly select one from duplicated cases (Whitehead, 2013)
duplicated.random = function(x, incomparables = FALSE, ...)
{
  if ( is.vector(x) )
  {
    permutation = sample(length(x))
    x.perm      = x[permutation]
    result.perm = duplicated(x.perm, incomparables, ...)
    result      = result.perm[order(permutation)]
    return(result)
  }
  else if ( is.matrix(x) )
  {
    permutation = sample(nrow(x))
    x.perm      = x[permutation,]
    result.perm = duplicated(x.perm, incomparables, ...)
    result      = result.perm[order(permutation)]
    return(result)
  }
}
dataWide <- dataWide[!duplicated.random(dataWide$id), ]


# Save data
write_xlsx(dataWide, "dataWide_woScores.xlsx")


# Calculate scores --------------------------------------------------------

calculate_scores <- function(data, time_point) {
  sleep_cols <- paste0("qids", 1:4, time_point)
  weight_cols <- paste0("qids", 6:9, time_point)
  psychomotor_cols <- paste0("qids", 15:16, time_point)
  dep_cols <- c(paste0("sleep", time_point), paste0("qids5", time_point), paste0("weight", time_point), 
                paste0("qids10", time_point), paste0("qids11", time_point), paste0("qids12", time_point), 
                paste0("qids13", time_point), paste0("qids14", time_point), paste0("psychomotor", time_point))
  alcohol_cols <- paste0("audit", 1:10, time_point)
  anxiety_cols <- paste0("gad", 1:7, time_point)
  insomnia_cols <- paste0("isi", 1:7, time_point)
  
  # First mutate call to create sleep, weight, and psychomotor columns
  data <- data %>%
    mutate(
      !!paste0("sleep", time_point) := pmax(!!!select(data, all_of(sleep_cols)), na.rm = TRUE),
      !!paste0("weight", time_point) := pmax(!!!select(data, all_of(weight_cols)), na.rm = TRUE),
      !!paste0("psychomotor", time_point) := pmax(!!!select(data, all_of(psychomotor_cols)), na.rm = TRUE)
    )
  
  # Second mutate call to calculate dep_t1 using the newly created columns
  data <- data %>%
    mutate(
      !!paste0("dep", time_point) := rowMeans(select(., all_of(dep_cols)), na.rm = TRUE),
      !!paste0("alcohol", time_point) := rowMeans(select(., all_of(alcohol_cols)), na.rm = TRUE),
      !!paste0("anxiety", time_point) := rowMeans(select(., all_of(anxiety_cols)), na.rm = TRUE),
      !!paste0("insomnia", time_point) := rowMeans(select(., all_of(insomnia_cols)), na.rm = TRUE)
    )
  
  if (time_point %in% c("_t4", "_t5", "_t6", "_t7", "_t8", "_t9")) {
    gd_cols <- paste0("igd", 1:4, time_point)
    data <- data %>%
      mutate(
        !!paste0("gd", time_point) := rowMeans(select(., all_of(gd_cols)), na.rm = TRUE)
      )
  }
  
  if (time_point %in% c("_t1", "_t2", "_t3", "_t4", "_t6", "_t8")) {
    ptsd_cols <- paste0("pcl", 1:20, time_point)
    data <- data %>%
      mutate(
        !!paste0("ptsd", time_point) := rowMeans(select(., all_of(ptsd_cols)), na.rm = TRUE)
      )
  }
  
  if (time_point %in% c("_t2", "_t3")) {
    covidStress_cols <- paste0("covidStress", 1:20, time_point)
    data <- data %>%
      mutate(
        !!paste0("covidStress", time_point) := rowMeans(select(., all_of(covidStress_cols)), na.rm = TRUE)
      )
  } else if (time_point %in% c("_t4", "_t5", "_t6", "_t7", "_t8")) {
    covidStress_cols <- paste0("covidStress", 1:18, time_point)
    data <- data %>%
      mutate(
        !!paste0("covidStress", time_point) := rowMeans(select(., all_of(covidStress_cols)), na.rm = TRUE)
      )
  }
  
  if (time_point == "_t4") {
    stress_cols <- paste0("pss", 1:5, time_point)
    data <- data %>%
      mutate(
        !!paste0("stress", time_point) := rowMeans(select(., all_of(stress_cols)), na.rm = TRUE)
      )
  } else if (time_point %in% c("_t3", "_t5", "_t6", "_t7", "_t8", "_t9")) {
    stress_cols <- paste0("pss", 1:10, time_point)
    data <- data %>%
      mutate(
        !!paste0("stress", time_point) := rowMeans(select(., all_of(stress_cols)), na.rm = TRUE)
      )
  }
  
  if (time_point == "_t5") {
    warStress_cols <- paste0(c("warStress1", "warStress2", "warStress4", "warStress7", "warStress12", "warStress13", "warStress14", "warStress15", "warStress18"), time_point)
    if (all(warStress_cols %in% colnames(data))) {
      data <- data %>%
        mutate(
          !!paste0("warStress", time_point) := rowMeans(select(., all_of(warStress_cols)), na.rm = TRUE)
        )
    }
  } else if (time_point %in% c("_t6", "_t7", "_t8", "_t9")) {
    warStress_cols <- paste0("warStress", 1:10, time_point)
    if (all(warStress_cols %in% colnames(data))) {
      data <- data %>%
        mutate(
          !!paste0("warStress", time_point) := rowMeans(select(., all_of(warStress_cols)), na.rm = TRUE)
        )
    }
  }
  
  return(data)
}


# Apply the calculate_scores function for each time point
time_points <- paste0("_t", 1:9)
dataWide <- reduce(time_points, calculate_scores, .init = dataWide)


# Reorganize variables ----------------------------------------------------

# Define the categories and time points
categories <- c("sleep", "weight", "psychomotor", "dep", "alcohol", "anxiety", "insomnia", "gd", "ptsd", "covidStress", "stress", "warStress")
time_points <- paste0("_t", 1:9)
# Initialize a vector to store the new order of variables
new_order <- character(0)
# Loop through each category and time point to create the new order of variables
for (category in categories) {
  for (time_point in time_points) {
    var_name <- paste0(category, time_point)
    if (var_name %in% colnames(dataWide)) {
      new_order <- c(new_order, var_name)
    }
  }
}
# Keep the first 803 variables in their original order
original_order <- colnames(dataWide)[1:803]
# Combine the original order with the new order
final_order <- c(original_order, new_order)
# Reorder the columns in dataWide based on the final order
dataWide <- dataWide[, final_order]

# Save data
write_xlsx(dataWide, "dataWide_wScores.xlsx")



# Check descriptives ------------------------------------------------------


# Reliabilities -----------------------------------------------------------

calculate_omega <- function(data, variables, time_points, nfactors = 1) {
  suppressWarnings(suppressMessages({
    reliabilities <- numeric(length(time_points))
    for (i in seq_along(time_points)) {
      tp <- time_points[i]
      current_vars <- paste0(variables, "_t", tp)
      current_data <- dplyr::select(data, dplyr::one_of(current_vars))
      reliabilities[i] <- psych::omega(current_data, nfactors = nfactors, poly = FALSE)$omega.tot
    }
    return(reliabilities)
  }))
}

# List of constructs and their respective variables
constructs <- list(
  Depression = c("sleep", "qids5", "weight", paste0("qids", 10:14), "psychomotor"),
  Anxiety = paste0("gad", 1:7),
  AlcoholAbuse = paste0("audit", 1:10),
  Insomnia = paste0("isi", 1:7),
  PTSD = paste0("pcl", 1:20),
  GamingDisorder = paste0("igd", 1:4),
  Stress = paste0("pss", 1:10),
  COVIDStress = paste0("covidStress", 1:20),
  WarStress = paste0("warStress", 1:18)
)

# Time points for each construct
time_points <- list(Depression = 1:9, Anxiety = 1:9, AlcoholAbuse = 1:9,
                    Insomnia = 1:9, PTSD = c(1:4, 6, 8), GamingDisorder = 4:9,
                    Stress = 3:9, COVIDStress = 2:8, WarStress = 5:9)

# Initialize reliabilities data frame
reliabilities <- data.frame(matrix(NA, ncol = 10, nrow = 0))
colnames(reliabilities) <- c("Construct", paste0("t", 1:9))

# Calculate omega coefficients and store reliabilities in a data frame
for (construct in names(constructs)) {
  omega_reliabilities <- rep(NA, 9)  # Placeholder for omega reliabilities initialized with NAs
  for (time_point in time_points[[construct]]) {
    omega_reliabilities[time_point] <- calculate_omega(dataWide, constructs[[construct]], time_point)
  }
  reliabilities <- rbind(reliabilities, c(construct, round(omega_reliabilities, 2)))
}

# Replace zeros with NAs
reliabilities[reliabilities == 0] <- NA

# Explicitly set the column names again to ensure they are displayed correctly
colnames(reliabilities) <- c("Construct", paste0("t", 1:9))

# Print reliabilities
print(reliabilities)


# Descriptives ------------------------------------------------------------

describe(select(dataWide, dep_t1:warStress_t9))


# Histograms --------------------------------------------------------------

# Define the categories, their x-axis limits, and time points
categories <- c("dep", "alcohol", "anxiety", "insomnia", "gd", "ptsd", "covidStress", "stress", "warStress")
x_lims <- list(dep = c(1, 4), alcohol = c(1, 5), anxiety = c(1, 4), insomnia = c(1, 5), 
               gd = c(1, 5), ptsd = c(1, 5), covidStress = c(1, 7), stress = c(1, 5), warStress = c(1, 7))
time_points <- paste0("_t", 1:9)

# Loop through each category
for (cat in categories) {
  # Find variables belonging to the current category across all time points
  vars <- names(dataWide)[sapply(names(dataWide), function(x) grepl(paste0(cat, "_t[1-9]$"), x))]
  # If there are variables for the current category, plot them
  if (length(vars) > 0) {
    # Set up the layout for the current category
    num_vars <- length(vars)
    num_cols <- min(3, num_vars)
    num_rows <- ceiling(num_vars / num_cols)
    # Set the layout parameters
    par(mfrow = c(num_rows, num_cols), mar = c(4, 4, 2, 1))
    # Loop through each variable and plot histogram
    for (var in vars) {
      hist(dataWide[[var]], main = var, xlab = var, col = "lightblue", border = "white", xlim = x_lims[[cat]])
    }
    # Reset layout parameters to default
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  }
}


# Correlogram -------------------------------------------------------------

# Select the relevant portion of the data
data_for_heatmap <- select(dataWide, dep_t1:warStress_t9)

# Calculate the correlation matrix
cor_matrix <- cor(data_for_heatmap, use = "complete.obs")

# Plot and save the correlogram
png("correlogram.png", width = 3000, height = 3000)
my_colors <- colorRampPalette(c("#920000", "white", "#009292"))(100)
corrplot(cor_matrix, method = "color", type = "upper", order = "original", col = my_colors,
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
dev.off()


# Trajectories ------------------------------------------------------------

# Extract mean scores
selected_data <- dataWide %>%
  select(dep_t1:warStress_t9) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Reshape to long format
data_long <- selected_data %>%
  gather(key = "variable_time", value = "mean_score") %>%
  separate(variable_time, into = c("variable", "time_point"), sep = "_") %>%
  mutate(time_point = factor(time_point, levels = paste0("t", 1:9)))

# Plot the trajectories
ggplot(data_long, aes(x = time_point, y = mean_score, group = variable, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Mean Scores of Mental Health Indicators from August 2020 to August 2023", x = "", y = "Mean Score") +
  theme_minimal() +
  ylim(1, 5) +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("Aug2020", "Oct2020", "Dec2020", 
                              "Dec2021", "Mar2022", "Jul2022",
                              "Dec2022", "Apr2023", "Aug2023")) +
  scale_color_manual(values = c("#920000", "#F0E442", "#006ddb", "#000000", "#ff6db6", "#db6d00", "#b66dff", "#b6dbff", "#009292"),
                     labels = c("Alcohol Abuse\n(scale 1 to 5)", "Anxiety\n(scale 1 to 4)", "COVID Stress\n(scale 1 to 7)", 
                                "Depression\n(scale 1 to 4)", "Gaming Disorder\n(scale 1 to 5)", "Insomnia\n(scale 1 to 5)", 
                                "PTSD\n(scale 1 to 5)", "Stress\n(scale 1 to 5)", "War Stress\n(scale 1 to 7)"))+  # Rename legend labels
  theme(
    plot.title = element_text(size = 12, face = "bold"),  # Change title size and make it bold
    axis.text.x = element_text(angle = 15, hjust = 1), # Tilt x-axis labels and adjust horizontal justification
    legend.title = element_blank()
  )


# Cut-off values ----------------------------------------------------------

# Define the transformation function
transform_scale <- function(x, x_min, x_max, y_min, y_max) {
  a <- (y_max - y_min) / (x_max - x_min)
  b <- y_min - a * x_min
  y <- a * x + b
  return(y)
}

# Function to calculate percentage
calculate_percentages <- function(dataWide, var_prefix, time_points, thresholds_original, scale_min, scale_max, scale_y_min, scale_y_max, labels) {
  thresholds_used <- transform_scale(thresholds_original, scale_min, scale_max, scale_y_min, scale_y_max)
  thresholds <- c(-Inf, thresholds_used, Inf)
  threshold_labels <- labels
  results <- data.frame(matrix(ncol = length(threshold_labels), nrow = length(time_points)))
  colnames(results) <- threshold_labels
  rownames(results) <- paste(var_prefix, time_points, sep = "")
  for (i in seq_along(time_points)) {
    var_name <- paste(var_prefix, time_points[i], sep = "")
    binned_data <- cut(dataWide[[var_name]], breaks = thresholds, labels = threshold_labels, include.lowest = TRUE)
    freq_table <- table(binned_data)
    total <- sum(freq_table)
    percentages <- round(as.numeric(freq_table) / total * 100, 2)
    results[i, names(freq_table)] <- percentages
  }
  results[is.na(results)] <- 0
  print(results)
}

# Depression
depression_labels <- c("No Depression", "Mild Depression", "Moderate Depression", "Severe Depression", "Very Severe Depression")
calculate_percentages(dataWide, "dep_t", 1:9, c(5, 10, 15, 20), 0, 27, 1, 4, depression_labels)

# Anxiety
anxiety_labels <- c("Minimal Anxiety", "Mild Anxiety", "Moderate Anxiety", "Severe Anxiety")
calculate_percentages(dataWide, "anxiety_t", 1:9, c(4, 9, 14), 0, 21, 1, 4, anxiety_labels)

# PTSD
ptsd_labels <- c("No PTSD", "Probable PTSD")
calculate_percentages(dataWide, "ptsd_t", c(1:4, 6, 8), c(32), 0, 80, 1, 5, ptsd_labels)

# Insomnia
insomnia_labels <- c("No Insomnia", "Sub-threshold Insomnia", "Moderate Insomnia", "Severe Insomnia")
calculate_percentages(dataWide, "insomnia_t", 1:9, c(7, 14, 21), 0, 28, 1, 5, insomnia_labels)

# Gaming disorder
gd_labels <- c("No Gaming Disorder", "Potential Gaming Disorder")
calculate_percentages(dataWide, "gd_t", 4:9, c(15), 4, 20, 1, 5, gd_labels)





