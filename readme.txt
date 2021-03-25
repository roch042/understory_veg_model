Fine Scale Vegetation - Understory prediction

Loosely have to run code in this order. To go from fitting models to predicting output. 

1. templates\Run_models_template.R - Reads in vegetation field data collected to fit predictive models. Will run through model fitting function for each species/subspecies/genus and save results for several types of machine learning models as .RData.
2. templates\Prepare_Covariates_Prediction_template.R - This code, along with a series of 'by hand' steps i detail in the code, have to be done to extract and prepare the quadpolygon specific covariates that are necessary to feed into the best model in order to predict either presence/absence or percent cover.
3. templates\Predict_models_template.R - Reads in a .RData file with covariates for the selected quadpolygon_IDs you are interested in predicting either presence/absence or percent cover for. Then loops through each of the species/subspecies/genus models that you have created and picks the best model and predicts from it.
	- Open the terminal in R and run the bash code if you are looping through multiple models (have to restart R each time to clear the virtual memory). Code is set up run through all the available models completed in the 'model' folder. Will have to alter code if you want to run a different length of loop. There are the following bash files in the functions folder to run:
		- loop_R_code_genus_percent_cover.sh
		- loop_R_code_genus_presence.sh
		- loop_R_code_species_percent_cover.sh
		- loop_R_code_species_presence.sh
		- loop_R_code_subspecies_percent_cover.sh
		- loop_R_code_subspecies_presence.sh
	- Code to run in the terminal:
		- sh functions/loop_R_code_species_percent_cover.sh

Other Code Templates:

templates\Summary_trained_models_template.R - Goes through all the saved machine learning results and summarizes the fit statistics for the 'best' of these models. Output can be used to create a summary report.

templates\Centroid_Maps_template.R - Reads in a text file of QuadPolygon_IDs and their centroids and then joins this with a text file of either presence/absence or percent cover data for each quadpolygon_ID.

