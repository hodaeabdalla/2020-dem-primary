library(fs)

#Used file_copy to copy the rds file into the 2020_Primary shinyapp file

file_copy(path = "raw-data/data.rds", new_path = "2020_Primary/data.rds")

file_copy(path = "2020.pdf", new_path = "2020_Primary/www/primary.pdf")
