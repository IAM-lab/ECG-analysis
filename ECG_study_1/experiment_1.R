##################################################################################
# NAME:         experiment_1.R
# AUTHOUR:      Alan Davies
# DATE:         21/08/2017 
# INSTITUTION:  Interaction Analysis and Modelling Lab (IAM), University of Manchester
# DESCRIPTION:  Metadata for experiement.
#               
##################################################################################

#---------------------------------------------------------------------------------
# FUNCTION:     getStimuliList()
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Returns list of all the stimuli in the experiment
#               
#---------------------------------------------------------------------------------
getStimuliList <- function()
{
    study_stimuli <- c("antlat STEMI", "Atrial Flutter", "HK", 
                       "LBBB", "NSR", "Sinus Tachycardia",
                       "SVT", "Ventricular paced rhythm", 
                       "VF", "VT", "WPW") 
    
    return(study_stimuli)
}

#---------------------------------------------------------------------------------
# FUNCTION:     getExperimentSetupData()
# INPUT:        String
# OUTPUT:       list
# DESCRIPTION:  Returns metradata about the experimental stimuli
#               
#---------------------------------------------------------------------------------
getExperimentSetupData <- function(stimuli_name)
{
    stimuli <- list()
    
    stimuli[["antlat STEMI"]] <- list(ref_name = "antlat STEMI", 
                                      file_name = "Anterior STEMI.jpg", 
                                      label = "Anterolateral STEMI",
                                      layout_type = "A",
                                      leads = 13)
    
    stimuli[["Atrial Flutter"]] <- list(ref_name = "Atrial Flutter", 
                                        file_name = "Atrial Fluter.jpg", 
                                        label = "Atrial Flutter",
                                        layout_type = "A",
                                        leads = 13)
    
    stimuli[["HK"]] <- list(ref_name = "HK", 
                            file_name = "Hyperkalaemia.jpg", 
                            label = "Hyperkalaemia",
                            layout_type = "A",
                            leads = 13)
    
    stimuli[["VF"]] <- list(ref_name = "VF", 
                            file_name = "Ventricular Fibrillation.jpg", 
                            label = "Torsades de pointes",
                            layout_type = "A",
                            leads = 13)
    
    stimuli[["WPW"]] <- list(ref_name = "WPW", 
                             file_name = "WPW.jpg", 
                             label = "Wolff Parkinson White syndrome (WPW)",
                             layout_type = "A",
                             leads = 13)
    
    stimuli[["VT"]] <- list(ref_name = "VT", 
                            file_name = "VT.JPG", 
                            label = "Ventricular Tachycardia (VT)",
                            layout_type = "B",
                            leads = 15)
    
    stimuli[["LBBB"]] <- list(ref_name = "LBBB", 
                              file_name = "AF, LBBB.JPG", 
                              label = "Left Bundle Branch Block (LBBB)",
                              layout_type = "B",
                              leads = 15)
    
    stimuli[["NSR"]] <- list(ref_name = "NSR", 
                             file_name = "Normal Sinus Rhythm.JPG", 
                             label = "Normal Sinus Rhythm (NSR)",
                             layout_type = "B",
                             leads = 15)
    
    stimuli[["Sinus Tachycardia"]] <- list(ref_name = "Sinus Tachycardia", 
                                           file_name = "Myocarditis - sinus tachy non specific ST changes.JPG", 
                                           label = "Sinus Tachycardia",
                                           layout_type = "B",
                                           leads = 15)
    
    stimuli[["SVT"]] <- list(ref_name = "SVT", 
                             file_name = "SVT.JPG", 
                             label = "Supraventricular Tachycardia (SVT)",
                             layout_type = "B",
                             leads = 15)
    
    stimuli[["Ventricular paced rhythm"]] <- list(ref_name = "Ventricular paced rhythm", 
                                                  file_name = "Bivent Pacer.JPG", 
                                                  label = "Ventricular paced rhythm",
                                                  layout_type = "B",
                                                  leads = 15)
    
    return(stimuli[[stimuli_name]])   
}