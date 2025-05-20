### Analysis 9-18-24: Activity 

rm(list = ls())

library(readxl)
library(tidyverse)
library(kableExtra)

read_multiple_sheets <- function(file_path) {
  sheet_names <- excel_sheets(file_path)
  sheet_data <- list()
  for (sheet_name in sheet_names) {
    sheet_data[[sheet_name]] <- read_excel(file_path, sheet = sheet_name)
  }
  return(sheet_data)
}

file_path <- ...
all_dat <- read_multiple_sheets(file_path)
all_dat <- all_dat[1:length(all_dat)]
all_dat_p <- all_dat
all_dat_p_title_lab <- all_dat

p_col_names <- c("group", names(all_dat_p[[1]])[2:3])

all_dat_p <- lapply(all_dat_p, setNames, p_col_names)

calculate_prop_p <- function(num_acl_inj, num_no_acl_inj) {
  prop <- num_acl_inj / (num_acl_inj + num_no_acl_inj)
  return(prop)
}

calculate_ci_p <- function(count, total_count){
  lb <- binom.test(count, total_count)$conf.int[1]
  ub <- binom.test(count, total_count)$conf.int[2]
  return(c(lb, ub))
}

for(i in 1:length(all_dat_p)) {
  all_dat_p[[i]]$prop <-  mapply(calculate_prop_p, all_dat[[i]]$`ACL Inj. Freq.`, all_dat[[i]]$`ACL Inj. Freq.` + all_dat[[i]]$`No ACL Inj. Freq.`)
  all_dat_p[[i]]$lb <- mapply(calculate_ci_p,   all_dat[[i]]$`ACL Inj. Freq.`, all_dat[[i]]$`ACL Inj. Freq.` + all_dat[[i]]$`No ACL Inj. Freq.`)[1, ]
  all_dat_p[[i]]$ub <- mapply(calculate_ci_p,   all_dat[[i]]$`ACL Inj. Freq.`, all_dat[[i]]$`ACL Inj. Freq.` + all_dat[[i]]$`No ACL Inj. Freq.`)[2, ]
}

calculate_ci <- function(count, total_count) {
  ci <- binom.test(count, total_count)$conf.int*100
  formatted_ci <- paste("[", round(ci[1], 3), ", ", round(ci[2], 3), "]", sep = "")
  return(formatted_ci)
}

add_overlap_column <- function(data) {
  ci_bounds <- strsplit(gsub("\\[|\\]", "", data$ci), ", ")
  lower_bounds <- sapply(ci_bounds, function(x) as.numeric(x[1]))
  upper_bounds <- sapply(ci_bounds, function(x) as.numeric(x[2]))
  data$overlap <- character(nrow(data))
  for (i in 1:nrow(data)) {
    overlapping_rows <- character(0)
    for (j in 1:nrow(data)) {
      if (i == j) {
        next
      }
      if (upper_bounds[i] >= lower_bounds[j] && lower_bounds[i] <= upper_bounds[j]) {
        overlapping_rows <- c(overlapping_rows, as.character(j))
      }
    }
    if (length(overlapping_rows) > 0) {
      overlap_string <- paste(paste(overlapping_rows, collapse = ", "), sep = " ")
      data$overlap[i] <- overlap_string
    } else {
      data$overlap[i] <- "No Overlap"
    }
  }
  
  return(data)
}

for(i in 1:length(all_dat)) {
  all_dat[[i]]$ci <- mapply(calculate_ci, all_dat[[i]]$`ACL Inj. Freq.`, all_dat[[i]]$`ACL Inj. Freq.` + all_dat[[i]]$`No ACL Inj. Freq.`)
  all_dat[[i]]$overlap <- add_overlap_column(all_dat[[i]])$overlap
  caption_name <- names(all_dat[[i]])[1]
  
  all_dat[[i]] <- data.frame(
    `Cohort` = all_dat[[i]][, 1],
    `ACL Inj. Freq.` = all_dat[[i]]$`ACL Inj. Freq.`,
    `No ACL Inj. Freq.` = all_dat[[i]]$`No ACL Inj. Freq.`,
    `Proportion ACL Inj.` = paste0(round((all_dat[[i]]$`ACL Inj. Freq.` / all_dat[[i]]$`No ACL Inj. Freq.`) * 100, digits = 3), "%"),
    `95% CI` = all_dat[[i]]$ci,
    `Row Overlap` = all_dat[[i]]$overlap,
    check.names = FALSE
  ) 
  
  names(all_dat[[i]]) <- c("Cohort", names(all_dat[[i]])[2:length(all_dat[[i]])])
  
  all_dat[[i]] <- all_dat[[i]] %>% 
    kable(caption = paste0("Group: ", caption_name)) %>% 
    kable_classic(html_font = 'cambria', full_width = F)
  
}

plot_list <- list(
  
)


## Plots 
for(i in 1:length(all_dat_p)){
  if(i == 1){
    plot_list[[i]] <- ggplot(
      data = as.data.frame(all_dat_p[[i]]), aes(x = group, y = prop)) +
      geom_bar(
        stat = "identity",
        position = "dodge", 
        width = 0.6,
        color = "black",
        fill = "lightblue") +
      geom_errorbar(
        aes(
          ymin = lb, 
          ymax = ub
        ),
        width = 0.4, 
        position = position_dodge(width = 0.6)) +
      labs(
        y = "Proportion ACL Inj.",
        x = ""
      ) +
      theme_bw() + 
      ggtitle(paste0("ACL Injury Proportions | ", names(all_dat_p_title_lab[[i]][1])))+
      scale_y_continuous(labels = scales::percent_format()) + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
  }
  else { 
    plot_list[[i]] <- ggplot(
      data = as.data.frame(all_dat_p[[i]]), aes(x = group, y = prop)) +
      geom_bar(
        stat = "identity",
        position = "dodge", 
        width = 0.6,
        color = "black",
        fill = "lightblue") +
      geom_errorbar(
        aes(
          ymin = lb, 
          ymax = ub
        ),
        width = 0.4, 
        position = position_dodge(width = 0.6)) +
      labs(
        y = "Proportion ACL Inj.",
        x = ""
      ) +
      theme_bw() + 
      ggtitle(paste0("ACL Injury Proportions | ", names(all_dat_p_title_lab[[i]][1])))+
      scale_y_continuous(labels = scales::percent_format())
  }
}

all_dat
plot_list
