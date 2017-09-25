##################################################################################
# NAME:         ECG_data_prep.R
# AUTHOUR:      Alan Davies
# DATE:         03/08/2017 
# INSTITUTION:  Interaction Analysis and Modelling Lab (IAM), University of Manchester
# DESCRIPTION:  Data munging for markov chain analysis
#               
##################################################################################

#---------------------------------------------------------------------------------
# FUNCTION:     openFiles()
# INPUT:        String
# OUTPUT:       list
# DESCRIPTION:  Returns a list of files accessable by label.
#                
#---------------------------------------------------------------------------------
openFiles <- function(file_path)
{
    files <- list()
    files_list <- NULL
    file_names <- NULL 
    
    # set the dir path and access all files
    dir_path <- paste0(path.expand("~/"), file_path)
    files_list <- list.files(path = dir_path, pattern = "csv", all.files = TRUE,
                             full.names = TRUE, recursive = TRUE, ignore.case = TRUE, include.dirs = TRUE)
    
    # get the names and use as the list element identifier. Store the files in the files list 
    for(i in 1:length(files_list))
    {
        file_names <- c(file_names, tools::file_path_sans_ext(basename(files_list[i])))
        files[[file_names[i]]] <- read.csv(files_list[i], header = TRUE, na.strings = c(" ", "NA", "-"))   
        cat("Adding file: ", file_names[i], "\n")
    }
    
    return(files)
}

#---------------------------------------------------------------------------------
# FUNCTION:     getAccuracySubgroup()
# INPUT:        String, String, String, boolean
# OUTPUT:       data.frame
# DESCRIPTION:  Load the file and return stimuli and selected subgroup based 
#               on accuracy
#---------------------------------------------------------------------------------
getAccuracySubgroup <- function(data_file, stimuli, accuracy, condition = FALSE)
{
    # subset on accuracy and stimuli
    dir_path <- paste0(path.expand("~/"), data_file)
    data <- read.csv(dir_path, header = TRUE, na.strings = c(" ", "NA", "-"))  
    data <- data[which(data$stimuli == stimuli & data$accuracy == accuracy), ]
    
    # if condition present also subset on condition
    if(condition) data <- data[data$condition == condition, ]
    
    return(data)
}

#---------------------------------------------------------------------------------
# FUNCTION:     scanForAOIHits()
# INPUT:        data.frame, int
# OUTPUT:       data.frame
# DESCRIPTION:  Produces a data frame of AOI hits in order containing 
#               AOI identifier participant name. Ensure leads have correct number
#               of factors for those where they be missing due to small numbers of
#               participants
#---------------------------------------------------------------------------------
getAOIHits <- function(data, num_factors)
{
    data_frames <- list()
    AOI_vector <- NULL
    participants_vector <- NULL
    data <- data[data$GazeEventType == "Fixation", ]
    data <- distinct(data) # remove duplicate fixation index events
    participants <- as.vector(data[!duplicated(data$ParticipantName), "ParticipantName"])
    
    for(i in 1:length(participants))
    {
        participant_data <- data[data$ParticipantName == participants[i], ]
        start_AOI_index <- grep("^AOI.A.Hit$", colnames(participant_data))
        participant_data <- participant_data[ ,start_AOI_index:ncol(participant_data)]
        
        for(j in 1:nrow(participant_data))
        {
            current_row <- as.vector(participant_data[j, ])
            if(1 %in% current_row)
            {
                hit_index <- which(current_row %in% 1)
                col_name <- colnames(current_row)[hit_index]
                AOI_string <- substr(col_name, 5, 5)
                AOI_vector <- c(AOI_vector, AOI_string)
            }
        }
        AOIs <- LETTERS[1:num_factors]
        participants_vector <- rep(participants[i], length(AOI_vector))
        df <- data.frame(participant = participants_vector, AOI = AOI_vector)
        df$AOI <- factor(df$AOI, levels = AOIs)
        data_frames[[i]] <- df
        
        participant_data <- NULL
    }
    hit_data <- ldply(data_frames, data.frame)
    return(hit_data)
}

# open the files
ECG_data_files <- openFiles("ECG_study_1/data/AOI hit data (pilot)/")

