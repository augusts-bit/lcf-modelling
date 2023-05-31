# Land cover fractions (LCF) modelling 
## Using Random Forest (RF), Long Short-Term Memory (LSTM) and Markov chain models with annual and dense input/output

Repository as developed for the master thesis "IMPROVING THE ROBUSTNESS OF GLOBAL LAND COVER FRACTION CHANGE DETECTION USING TEMPORAL DEEP LEARNING".

**src** - Directory containing (Python/R) code used to process output.

**utils** - Directory containg (R) code with functions used in **src**.

**src** is divided into 4 subfolders, containg code for developing of training and validation data (**1-developing**), modelling annual and dense land cover fractions using RF and LSTM models (**2-modelling**), post-processing predictions using post-processing LSTM (PostLSTM) and Markov chain models (**3-post-processing**) and an accuracy assessment (**4-accuracy-assessment**).   

Credits to the Markov chain and large parts of the RF code go to https://github.com/roburger/lcfMapping.  

Note: This study used reference (IIASA/WUR) data sets and Landsat imagery that are expected to be in a ``Data/raw`` folder. Furthermore, code may contain errors and lack efficiency. For questions, contact: august.slomp@wur.nl or augustslomp@gmail.com.

Run in the following order:

### Developing model input

Open **src/1-developing**.

Run **trainFilter.R** and **valiFilter.R** first. These two scripts filter Landsat time series (of the training and validation locations resp.) using a *locally estimated scatterplot smoothing* (LOESS) plot to remove outliers.

Both **LSTM** and **RF** folders contain R scripts to transform the filtered Landsat time series and join it with reference data to create training and validation ``.csv`` files. **RF/loadRFTrainVali.R** creates training input for both the annual and dense RF regression models using *vegetation indices* (VIs) including with harmonic analysis (using https://github.com/JornDallinga/probaV). For training, features are derived over the entire extracted Landsat time series and joined with the larger reference data set containing LCF of 2015. In this script, also four validation files are created by deriving features over 3-year periods surrounding 2015, 2016, 2017 and 2018 for the annual RF model. In **RF/loadRFDenseVali.R** 92 validation data frames are created for the dense RF model using a sliding window in wich features are derived over 3-year periods (1.5 before and after) surrounding each acquisition date (within 2015-2018). In **LSTM/loadLSTMTrain.R** and **LSTM/loadLSTMVali.R"** create training and validation for the annual LSTM model by deriving band medians and features (as used by the RF models) over 3-year periods for each year (2015-2018) and creates a time series. Respective LCF are joined with the training data.   
