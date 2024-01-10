# Code and Data for Review and Reproducibility
This repo contains the code and data used for my senior thesis which I submitted in Dec 2023. I am currently going through and organizing this README for better reproducibility. 

Currently, this repo contains the following:
* In the neural model folder, fine-tuned ViT, SWIN transformer, and ResNet models, each fine-tuned on an original corpus of 2,000 high resolution satellite images, courtesy of Maxar's open data program.
* Raw data files
  * Raw test score data (from California standardized testing programs over the past 20 years)
    * This data set also includes geographic data for schools that allows me to connect them to air quality monitors
  * Air quality data from EPA monitors across the state of california (this data is also used as ground truth labels for the training step above)
* A set of numbered 1 to 5 R and python files that are associated with my data cleaning pipeline and final regressions that assess the effects of air quality on educational data
  
