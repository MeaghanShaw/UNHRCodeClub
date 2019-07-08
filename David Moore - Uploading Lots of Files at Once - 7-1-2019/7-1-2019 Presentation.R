
# Uploading Lots of Files at Once
# By David Moore
# 7-1-2019

# Use the list.files function to search a directory for files and folders containing a certain character string (i.e., pattern)
list.files("C:/Users/David Moore/Documents", pattern = "Coffee and Doughnuts")
list.files("C:/Users/David Moore/Documents/Sap Flow Research/Thompson Farm Project", pattern = "Sap_Flow_Data.csv")
# Files not followed by an extension (such as '.csv') are probably folders within the folder you're looking in

# To search for these files in many subfolders within a folder, use the 'recursive = T' argument (this can take a while depending on how many things are in your folders, so be patient)
list.files("C:/Users/David Moore/Documents", pattern = "Coffee and Doughnuts", recursive = T)

# To return complete names (including the filepath), use the 'full.names = T' argument
list.files("C:/Users/David Moore/Documents", pattern = "Coffee and Doughnuts", full.names = T)

# To upload only the most recent file containing a certain character string from a particular folder:
all_coffee_and_doughnut_files <- file.info(list.files("C:/Users/David Moore/Documents", pattern = "Coffee and Doughnuts", full.names = T))
# You now have a data frame containing lots of information about the files containing the character string of interest:
str(all_coffee_and_doughnut_files)
# The 'mtime' column contains dates and times of a files' most recent modification
str(all_coffee_and_doughnut_files$mtime)
# To get the most recent file, select the one with the maximum 'mtime'
which.max(all_coffee_and_doughnut_files$mtime)
all_coffee_and_doughnut_files[which.max(all_coffee_and_doughnut_files$mtime), ]
most_recent_coffee_and_doughnuts_file <- all_coffee_and_doughnut_files[which.max(all_coffee_and_doughnut_files$mtime), ]
most_recent_coffee_and_doughnuts_file

# What if I have several different groups of files (each group contains a certain character string), and I want to upload the most recent file from each group?
Working_Directory <- "C:/Users/David Moore/Documents/Sap Flow Research/Thompson Farm Project"
setwd(Working_Directory)
Group_Names <- c("Oak", "Pine", "Maple")
All_Files_in_Folder_1 <- list.files(Working_Directory, pattern = ".dat", full.names = T, recursive = T)
All_Files_in_Folder_2 <- file.info(All_Files_in_Folder)
Files_Containing_Group_Names <- lapply(Group_Names, function (x) {grep(x, rownames(All_Files_in_Folder_2))})
List_of_Files_to_Upload <- lapply(Files_Containing_Group_Names, function (x) {All_Files_in_Folder_2[x, ]})
List_of_Most_Recent_Files <- lapply(List_of_Files_to_Upload, function (x) {x[which.max(x$mtime), ]})
All_Files <- lapply(List_of_Most_Recent_Files, function (x) {read.table(rownames(x), skip = 1, header = T, stringsAsFactors = F, sep = ",", na.strings = "NAN")})
names(All_Files) <- Group_Names
