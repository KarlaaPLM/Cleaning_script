
################################################################################
################################################################################
######Para todos los archivos de excel y todos las sheets####################
################################################################################
################################################################################
################################################################################

library(readxl)
library(dplyr)
library(purrr)
# List all Excel files in the folder (adjust path if needed)
excel_files <- list.files(path = "~/Desktop/Dados IPMA/RegJuv2024_clean", 
                          pattern = ".xlsx", full.names = TRUE)
# Updated function: process a single sheet from a given file
process_sheet <- function(file, sheet_name) {
  # Read metadata
  metadata <- read_excel(file, sheet = sheet_name, range = "A1:T35", 
                         col_names = FALSE)
  tank <- metadata[[1, 1]]
  species <- metadata[[2, 1]]
  lote <- metadata[[2, 6]]
  month <- metadata[[1, 9]]
  # Read main data
  df <- read_excel(file, sheet = sheet_name, range = "A4:T35")
  col_names <- names(df)
  # Handle Idade columns
  has_idade1 <- "Idade 1" %in% col_names
  has_idade2 <- "Idade 2" %in% col_names
  has_idade3 <- "Idade 3" %in% col_names
  
  if (has_idade1 & has_idade2 & has_idade3) {
    df$Idade <- rowMeans(select(df, `Idade 1`, `Idade 2`, `Idade 3`), 
                           na.rm = TRUE)
  } else if (has_idade1) {
    df$Idade <- df$`Idade 1`
  } else {
    df$Idade <- NA_real_
  }
  # Add metadata and calculate new columns
  data_cleaned <- df %>%
    mutate(
      Tank = tank,
      Species = species,
      Lote = lote,
      Month = month,
      `Nº de indíviduos` = as.numeric(`Nº de indíviduos`),
      Mortalidade = rowMeans(select(., Fora, Dentro), na.rm = TRUE),
      Food = rowSums(select(., `Food 1`, `Food 2`, `Food 3`, `Food 4`), na.rm = TRUE),
      File = basename(file),
      Sheet = sheet_name
    ) %>%
    select(Lote, Species, Tank, Month, Dia, Idade, `Nº de indíviduos`, Mortalidade,
           O2, Temp, Food, File, Sheet)
  
  return(data_cleaned)
}
# Process all sheets in all files
final_data <- map_dfr(excel_files, function(file) {
  sheets <- excel_sheets(file)
  map_dfr(sheets, ~process_sheet(file, .x))
})

################################################################################
################################################################################
########To divide the food by the days it has passed since the last meal########
################################################################################
################################################################################

#Filtering the days of the month that don't have any value for Age, Individuals, etc
#Mostly after the splitting of densities in different tanks. This ensures that the calculations for the
#food are accurate across the days of the month. 

final_data <- final_data %>%
  filter(!is.na(`Nº de indíviduos`)) %>%
  mutate_all(~replace(., is.nan(.), 0))

# Initialize result vector
result <- rep(NA, nrow(final_data))

# Loop through each Tank group
for (tank in unique(final_data$Tank)) {
  
  # Subset data for this tank
  tank_indices <- which(final_data$Tank == tank)
  tank_data <- final_data[tank_indices, ]
  
  # Get indices of non-zero Food values within this tank group
  non_zero_indices <- which(tank_data$Food != 0)
  
  # Loop through each block of values
  for (i in seq_along(non_zero_indices)) {
    start <- non_zero_indices[i]
    end <- if (i < length(non_zero_indices)) non_zero_indices[i + 1] - 1 else nrow(tank_data)
    
    total_cells <- end - start + 1  # Including the current non-zero row
    divided_value <- tank_data$Food[start] / total_cells
    
    # Fill values in the result vector (use original row indices)
    result[tank_indices[start:end]] <- divided_value
  }
}

# Replace original column
final_data$Food <- result

final_data <- final_data %>%
  select(!(File)) %>%
  select(!(Sheet))


write.csv(final_data, "cleaned_s.aurata_d.labrax_2024.csv", row.names = FALSE)
writexl::write_xlsx(final_data, "cleaned_s.aurata_d.labrax_2024.xlsx")

