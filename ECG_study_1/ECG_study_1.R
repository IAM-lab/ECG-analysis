##################################################################################
# NAME:         ECG_study_1.R
# AUTHOUR:      Alan Davies
# DATE:         21/08/2017 
# INSTITUTION:  Interaction Analysis and Modelling Lab (IAM), University of Manchester
# DESCRIPTION:  Markov chain analysis analysis of ECGs for inital study using
#               Jensen-Shannon Distance
##################################################################################

#---------------------------------------------------------------------------------
# FUNCTION:     loadPackages(package.args)
# INPUT:        vector
# OUTPUT:       void
# DESCRIPTION:  Loads required packages.
#                
#---------------------------------------------------------------------------------
loadPackages <- function(package.args)
{ 
    for(i in package.args)
    {
        if(!is.element(i, .packages(all.available = TRUE)))
        {
            cat("\nPackage <", i, "> not found, attempting to add it...")
            install.packages(i)
        }
        library(i, character.only = TRUE)
    }
}

#---------------------------------------------------------------------------------
# FUNCTION:     initialize()
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Set up function for adding packages and other source data
#               
#---------------------------------------------------------------------------------
initialize <- function()
{
    # load packages
    package.args <- c("msm", "checkpoint", "dplyr", "tidyr", "binom", "plyr", "reshape2")
    loadPackages(package.args)
    checkpoint("2017-08-16")
}

#---------------------------------------------------------------------------------
# FUNCTION:     convertToMatrix()
# INPUT:        data.frame
# OUTPUT:       matrix
# DESCRIPTION:  Returns matrix representing probabilty of AOI transitions
#               
#---------------------------------------------------------------------------------
convertToMatrix <- function(data)
{
    tmp0 <- data
    tmp <- tmp0 %>% group_by(participant) %>% mutate(to = lead(AOI))
    tmp2 <- tmp[complete.cases(tmp), ]
    with(tmp2, table(AOI, to))
    out_mat <- as.matrix(with(tmp2, table(AOI, to)))
    prob_mat <- out_mat / rowSums(out_mat)
    
    return(prob_mat)
}

#---------------------------------------------------------------------------------
# FUNCTION:     convertToPrior()
# INPUT:        matrix
# OUTPUT:       matrix
# DESCRIPTION:  Bayesian prior (Dirichlet) method
#               
#---------------------------------------------------------------------------------
convertToPrior <- function(data)
{
    m <- 0
    matrix_len <- nrow(data) 
    for(i in 1:matrix_len)
    {
        m <- sum(data[i, ] * 10)
        for(j in 1:matrix_len)
            data[i, j] <- ((data[i, j] * 10) + 1) / (m + matrix_len)
    }
    return(data)
}

#---------------------------------------------------------------------------------
# FUNCTION:     calculateJSDistance()
# INPUT:        matrix, matrix
# OUTPUT:       double
# DESCRIPTION:  Computes correct Jensen Shannon distance between two matrices
#               
#---------------------------------------------------------------------------------
calculateJSDistance <- function(m1, m2)
{
    results <- list()
    JS_distance <- 0
    summed_JS_distance <- 0
    matrix_length <- nrow(m1)
    coeff1 <- 0
    coeff2 <- 0
    
    for(i in 1:matrix_length)
    {
        for(j in 1:matrix_length)
        {
            # calculate KLD per row
            coeff1 <- coeff1 + (m1[i, j] * log(m1[i, j] / (0.5 * (m1[i, j] + m2[i, j]))))
            coeff2 <- coeff2 + (m2[i, j] * log(m2[i, j] / (0.5 * (m1[i, j] + m2[i, j])))) 
        }
        JS_distance <- sqrt(0.5 * (coeff1 + coeff2))
        summed_JS_distance <- summed_JS_distance + JS_distance
        JS_distance <- 0
        coeff1 <- 0
        coeff2 <- 0
    }
    average_distance <- summed_JS_distance / matrix_length
    return(average_distance)
}

#---------------------------------------------------------------------------------
# FUNCTION:     randomPermutations()
# INPUT:        data.frame, int, int, int
# OUTPUT:       vector
# DESCRIPTION:  Generate 2 random sub groups the same size as the original groups
#               and compare distances for the selected amout of permutations.
#---------------------------------------------------------------------------------
randomPermutations <- function(data, group1_size, stimuli, perms = 10)
{
    progress <- 0
    distance <- NULL
    
    # create progress bar
    progress_bar <- winProgressBar(title = "Computing Distances", min = 0, max = perms, width = 300)
    getWinProgressBar(progress_bar)
    
    # extract participants
    participants <- as.data.frame(data[!duplicated(data$ParticipantName), "ParticipantName"])
    colnames(participants) <- "participant"
    
    # loop over the permutations  
    for(i in 1:perms)
    {
        # get a random subset of participants and store in group 1 whatever is left put in second group
        group1_participants <- participants[sample(unique(nrow(participants)), group1_size), ]
        group2_participants <- as.vector(participants[!(participants$participant %in% group1_participants), ])
        
        # get the selected groups data
        group1 <- data[which(data$ParticipantName %in% group1_participants), ]
        group2 <- data[which(data$ParticipantName %in% group2_participants), ]
        
        # build Markov chains
        chain_1 <- convertToMatrix(getAOIHits(group1, stimuli$leads))
        chain_2 <- convertToMatrix(getAOIHits(group2, stimuli$leads))
        
        # remove any NaN
        chain_1[is.na(chain_1)] <- 0
        chain_2[is.na(chain_2)] <- 0
        
        # convert to prior
        chain_1 <- convertToPrior(chain_1)
        chain_2 <- convertToPrior(chain_2)                       
        
        # get distance 
        distance_results <- calculateJSDistance(chain_1, chain_2)
        
        # store computed distance in vector and return
        distance <- c(distance, distance_results)
        
        # display progress bar
        setWinProgressBar(progress_bar, i, title = paste(round(i / perms * 100, 0), "% processed [Computing Distance]"))
    }
    
    # close the progress bar & return result
    close(progress_bar)
    return(distance)
}

#---------------------------------------------------------------------------------
# FUNCTION:     calculatePvalue()
# INPUT:        vector, vector
# OUTPUT:       void
# DESCRIPTION:  Calculate p-value (% of values > correct/incorrect value)
#               
#---------------------------------------------------------------------------------
calculatePvalue <- function(correct_and_incorrect, shuffled_distances)
{
    gtr <- length(shuffled_distances[shuffled_distances > correct_and_incorrect])
    pvalue <- gtr / length(shuffled_distances) 
    return(pvalue)
}

#---------------------------------------------------------------------------------
# FUNCTION:     generateDensityPlot()
# INPUT:        double, vector, list
# OUTPUT:       void
# DESCRIPTION:  Output density plot 
#               
#---------------------------------------------------------------------------------
generateDensityPlot <- function(distance_result, shuffled_distances, stimuli_data)
{
    density_plot <- density(shuffled_distances)
    plot(density_plot, type = "n", main = stimuli_data$label, xlab = "Jensen-Shannon Distance")
    polygon(density_plot, col = "lightgray", border = "grey")
    rug(shuffled_distances, col = ifelse(shuffled_distances == distance_result, 'blue', 'red'))
    print(abline(v = distance_result, col = "purple"))
    print(density_plot)
}

#---------------------------------------------------------------------------------
# FUNCTION:     outputReport()
# INPUT:        list
# OUTPUT:       void
# DESCRIPTION:  Output results of distance and p-value 
#               
#---------------------------------------------------------------------------------
outputReport <- function(report_args)
{
    cat("\n\nData: ", report_args[["dataname"]], "\n")
    df <- data.frame(JSD = report_args[["JSD"]], p.value = report_args[["p-value"]], Permutations = report_args[["permutations"]])
    print(df) 
    cat("\nGroup 1 (n): ", report_args[["groupsize1"]])
    cat("\nGroup 2 (n): ", report_args[["groupsize2"]])  
}

#---------------------------------------------------------------------------------
# FUNCTION:     main()
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Main function. 
#               Makes all subsequent function calls
#---------------------------------------------------------------------------------
main <- function()
{
    permutations <- 10000
    
    initialize()
    source(paste0(path.expand("~/"), "ECG_study_1/experiment_1.R"))
    source(paste0(path.expand("~/"), "ECG_study_1/ECG_data_prep.R"))
    
    for(i in 1:length(getStimuliList()))
    {
        # set the current stimuli
        stimuli_data <- getExperimentSetupData(getStimuliList()[i])
        current_stimuli <- stimuli_data$ref_name
        
        # get participant accuracy results
        combined_data <- ECG_data_files[[current_stimuli]]
        
        correct_participants_list <- getAccuracySubgroup("patient_history_phd/data/accuracy_pilot.csv", current_stimuli, "1")
        correct_participants_list <- as.vector(correct_participants_list$participant)
        incorrect_participant_list <- getAccuracySubgroup("patient_history_phd/data/accuracy_pilot.csv", current_stimuli, "0")
        incorrect_participant_list <- as.vector(incorrect_participant_list$participant)
        
        # get correct and incorrect groups
        correct_data <- combined_data[which(combined_data$ParticipantName %in% correct_participants_list), ]
        incorrect_data <- combined_data[which(combined_data$ParticipantName %in% incorrect_participant_list), ]
        
        # recombine with acutal participants and build matrices
        combined_data <- rbind(correct_data, incorrect_data)
        correct_chain <- convertToMatrix(getAOIHits(correct_data, stimuli_data$leads))
        incorrect_chain <- convertToMatrix(getAOIHits(incorrect_data, stimuli_data$leads))
        
        # remove any NaN
        correct_chain[is.na(correct_chain)] <- 0
        incorrect_chain[is.na(incorrect_chain)] <- 0
        
        correct_chain <- convertToPrior(correct_chain)
        incorrect_chain <- convertToPrior(incorrect_chain)
        
        # work out distance between correct and incorrect groups
        distance_result <- calculateJSDistance(correct_chain, incorrect_chain)
        
        # carry out permutation test
        shuffled_distances <- randomPermutations(combined_data, length(correct_participants_list), stimuli_data, perms = permutations)
        
        # output density plot
        generateDensityPlot(distance_result, shuffled_distances, stimuli_data)
        
        # generate report args
        report_args <- list()
        report_args[["dataname"]] <- stimuli_data$label
        report_args[["JSD"]] <- distance_result
        report_args[["p-value"]] <- calculatePvalue(distance_result, shuffled_distances) 
        report_args[["permutations"]] <- permutations
        report_args[["groupsize1"]] <- length(correct_participants_list)
        report_args[["groupsize2"]] <- length(incorrect_participant_list)
        outputReport(report_args)
    }
}

# run main
main()
