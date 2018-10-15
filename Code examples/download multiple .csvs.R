# nesting dataframes

data_path <- "/Users/brett.johnson/Documents/Projects/juvenile-salmon/Fine-scale migration dynamics/data"
files <- dir(data_path, pattern = "*.csv")

data <- data_frame(filename = files) %>% 
  mutate(file_contents = map(filename,
                             ~ read_csv(file.path(data_path, .), skip = 10))
  )

data <- unnest(data)         

split(data, factor(filename))
