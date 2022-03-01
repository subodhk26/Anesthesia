# Final Degree Project 
Joan Altés Depares

June 2020
__________

Source code for my final project (bachelor thesis) for my Biomedical Engineering Degree. Project workflow was conducted as follows:
1. Data processing and exploratory analysis
2. Model hyperparameter tuning
3. Model training and testing
4. Model explanation

![Alt text](Figures/workflow.png?raw=true)

Models were built using `sklearn`, `keras` and `tensorflow`, hyperparameter tuning was performed using `talos`. A total of 5 models were tested:
* Random Forest
* k-Nearest Neighbors
* Support Vector Classifier
* Deep Neural Network
* Long Short Term Memory

The dataset used for this project contains data from many anesthesia monitoring devices from a number of surgical procedures. The dataset belongs to the SPEC-M research group in Hospital Clínic in Barcelona and it is not publicly available. 

Results were good for the prediction of light hypnosis and hypotension, while movement prediction was less accurate but still acceptable. LSTM model was the best performing in both 'accurate' models with a specificity between 0.667 and 0.784 for a sensitivity of 1, confirming that the time series nature of the data must be taken into account.

![Alt text](Figures/rocresults.png?raw=true)

For detailed explanation on the work done please refer to the `.pdf` file.

Contact: jaltes98@gmail.com
