# Create dataframe comprised of all RNA densitometry measurements
  type <- 'RNA' # oligonucleotide type
  lane <- c(1:10) # gel lane
  tex_conc <- c(0, 2.5, 5, 10, 15, 20, 25, 30, 40, 0) # Tex protein concentration (there are two 0 lanes to ensure negative control on each side)
  # Oligonucleotide (RNA)-only band florescence (integrated density measured in ImageJ) by experiment repeat
  experiment_1_oligo <- c(28.622, 31.595, 32.241, 34.116, 27.859, 28.712, 10.586, 15.927, 9.403, 26.213)
  experiment_2_oligo <- c(38.488, 24.513, 33.265, 32.876, 32.281, 20.213, 21.907, 19.329, 26.757, 38.675)
  experiment_3_oligo <-c(34.180, 35.269, 24.955, 28.194, 30.175, 27.767, 11.048, 27.881, 31.308, 39.028)
  # Total band florescence (integrated density measured in ImageJ) by experiment repeat
  experiment_1_total <- c(28.510, 32.852, 39.093, 48.360, 40.359, 60.567, 28.604, 51.542, 53.459, 27.244)
  experiment_2_total <- c(39.922, 26.079, 40.759, 40.326, 48.365, 49.236, 37.053, 52.557, 60.389, 40.582)
  experiment_3_total <- c(34.493, 36.795, 37.786, 57.478, 74.756, 68.865, 36.504, 74.179, 81.600, 41.202)
  # Calculation of fluorensecence originating from Tex-bound oligonucleotide
  expeirment_1_bound <- experiment_1_total - experiment_1_oligo
  experiment_2_bound <- experiment_2_total - experiment_2_oligo
  experiment_3_bound <- experiment_3_total - experiment_3_oligo
  # Calculation of Tex-bound fluorescene as a fraction of the total (adapted from Johnson et al. 2008)
  experiment_1_bf <- 1-(experiment_1_oligo/experiment_1_total)
  experiment_2_bf <- 1-(experiment_2_oligo/experiment_2_total)
  experiment_3_bf <- 1-(experiment_3_oligo/experiment_3_total)

  # Create a data frame only including the 'bound fraction' values
  RNA_dens <- data.frame(type, lane, tex_conc, experiment_1_bf, experiment_2_bf, experiment_3_bf)
  # Add a new column containing the derived fraction means across experiments (values in columns 4-6)
  RNA_dens$mean_bf <- rowMeans(RNA_dens[,4:6])

# Create dataframe comprised of all RNA densitometry measurements
  type <- 'DNA' # oligonucleotide type
  lane <- c(1:10) # gel lane
  tex_conc <- c(0, 2.5, 5, 10, 15, 20, 25, 30, 40, 0) # Tex protein concentration (there are two 0 lanes to ensure negative control on each side)
  # Oligonucleotide (RNA)-only band florescence (integrated density measured in ImageJ) by experiment repeat
  experiment_1_oligo <- c(32.255, 32.451, 30.668, 30.743, 27.782, 22.223, 28.076, 30.642, 30.673, 27.524)
  experiment_2_oligo <- c(33.405, 27.069, 28.146, 33.186, 18.527, 24.589, 18.429, 15.174, 17.593, 35.738)
  experiment_3_oligo <- c(39.500, 44.841, 43.817, 41.715, 32.665, 28.138, 27.059, 27.025, 35.200, 40.604)
  # Total band florescence (integrated density measured in ImageJ) by experiment repeat
  experiment_1_total <- c(34.173, 37.463, 43.766, 50.349, 45.044, 43.113, 47.112, 49.116, 44.435, 28.593)
  experiment_2_total <- c(34.283, 27.864, 39.472, 41.451, 27.380, 34.904, 35.046, 20.950, 37.172, 37.190)
  experiment_3_total <- c(40.163, 53.018, 49.707, 65.357, 68.660, 62.152, 61.820, 57.219, 66.703, 42.570)
  # Calculation of fluorensecence originating from Tex-bound oligonucleotide
  expeirment_1_bound <- experiment_1_total - experiment_1_oligo
  experiment_2_bound <- experiment_2_total - experiment_2_oligo
  experiment_3_bound <- experiment_3_total - experiment_3_oligo
  # Calculation of Tex-bound fluorescene as a fraction of the total (adapted from Johnson et al. 2008)
  experiment_1_bf <- 1-(experiment_1_oligo/experiment_1_total)
  experiment_2_bf <- 1-(experiment_2_oligo/experiment_2_total)
  experiment_3_bf <- 1-(experiment_3_oligo/experiment_3_total)
  
  # Create a data frame only including the 'bound fraction' values
  DNA_dens <- data.frame(type, lane, tex_conc, experiment_1_bf, experiment_2_bf, experiment_3_bf)
  # Add a new column containing the derived fraction means across experiments (values in columns 4-6)
  DNA_dens$mean_bf <- rowMeans(DNA_dens[,4:6])

# Create new data frame containing data from both RNA and DNA experiments
  dens_all <- rbind(RNA_dens, DNA_dens) # function rbind combines vertically when all sources have the same variables
  
# Create ggplot of densitometry data
  # Install relevant libraries
  library(ggplot2)
  library(viridis)
  
  #Basic plot information (e.g., data source, axis)
  dens_plot <- ggplot(data=dens_all,aes(x=`tex_conc`, y=`mean_bf`, xmax = 40, ymax = 1)) + 
    # Plot the datapoints by sample type, shaded area denotes standard error of the mean
    geom_smooth(aes(color = type)) +
    #Changing colour of the points to be colourblind friendly and greyscale visible
    scale_color_viridis(discrete=TRUE) +
    theme_bw() +
  # Change axis labels and chart title
  ylab("Mean Oligonucleotide Fraction Bound")+
    xlab(expression(paste("Tex Concentration (", mu, "M)"))) +
    # Create title that splits (/n) over lines
    ggtitle(expression(paste("Fraction of Oligonucleotide Bound by Tex"))) +
    # Make it pretty
    theme_minimal()
  
# Create new data frame with all derived bound fraction values
  bf <- data.frame((dens_all$type), c(dens_all$experiment_1_bf, dens_all$experiment_2_bf, dens_all$experiment_3_bf))
  # Conduct Shapiro-Wilks test for normal distribution
  shapiro.test(bf[,2])
  # p<0.05, therefore data not considered normally distributed
  # Mann-Whitney U test (MW-Wilcoxon) used to assess whether there is a difference in RNA and DNA affinity
  wilcox.test(bf[,2]~bf[,1])
