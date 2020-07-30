# fmoisture
Moisture function analysis
This folder contains R scripts for moisture function analysis manuscript
File summary
1. datasets_mp.csv--this is the data file with soil information, moisture and respiration measurements, fitted van Genuchten (or dual van Genuchten) parameters (datasets.csv---without van Genuchten parameters)
2. mvol_mp_model_fitting.R--script to fit van Genuchten
3. fmoisture-texture.R--script to create figure S1 (moisture functions across 6 different soil textures)
4. fws_residual_plot.R--script to create residual plot (scaling_ghe.R--scale Ghezzzehei fw prediction) 
5. MMkinetics.R --script applys moisture function to MM kinetics, using gas and solute diffusivity to control delivery of substrates
