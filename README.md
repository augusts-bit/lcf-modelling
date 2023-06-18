# Land cover fractions (LCF) modelling 
## Using Random Forest (RF), Long Short-Term Memory (LSTM) and Markov chain models with annual and dense input/output

Repository as developed for the master thesis "IMPROVING THE ROBUSTNESS OF GLOBAL LAND COVER FRACTION CHANGE DETECTION USING TEMPORAL DEEP LEARNING".

**src** - Directory containing (Python notebook/R) code used to process output.

**utils** - Directory containing (R) code with functions used in **src**.

**src** is divided into 4 subfolders, containing code for developing training and validation data (**1-developing**), modelling annual and dense land cover fractions using RF and LSTM models (**2-modelling**), post-processing predictions using post-processing LSTM (PostLSTM) and Markov chain models (**3-post-processing**) and an accuracy assessment (**4-accuracy-assessment**).   

Credits of the Markov chain and large parts of the RF code go to https://github.com/roburger/lcfMapping.  

Note: This study used reference (IIASA/WUR) data sets and Landsat imagery that are expected to be in a ``Data/raw`` folder. Furthermore, code may contain errors and lack efficiency. For questions, contact: august.slomp@wur.nl or augustslomp@gmail.com.

Run in the following order:

### Developing model input

Open **src/1-developing**.

1. Run **train2015Filter.R**, **trainchangeFilter.R** and **valiFilter.R** first. These scripts filter Landsat time series (of the training, 2015 locations and 2015-2018 locations, and validation locations resp.) using a *locally estimated scatterplot smoothing* (LOESS) plot to remove outliers.

2. Both **LSTM** and **RF** folders contain R scripts to transform the filtered Landsat time series and join it with reference data to create training and validation ``.csv`` files. **RF/loadRFTrainVali.R** creates training input for both the annual and dense RF regression models using *vegetation indices* (VIs) including harmonic analysis (using https://github.com/JornDallinga/probaV). For training, features are derived over the entire extracted Landsat time series and joined with the larger reference data set containing LCF of 2015. In this script, also four validation files are created by deriving features over 3-year periods surrounding 2015, 2016, 2017 and 2018 for the annual RF model. In **RF/loadRFDenseVali.R** 92 validation data frames are created for the dense RF model using a sliding window in which features are derived over 3-year periods (1.5 before and after) surrounding each acquisition date (within 2015-2018). In **LSTM/loadLSTMTrain.R** and **LSTM/loadLSTMVali.R** training and validation data is created for the annual LSTM model by deriving band medians and features (as used by the RF models) over 3-year periods for each year (2015-2018) and creates a time series. Respective, 2015-2018 multi-annual, LCF are joined. In **LSTM/loadDenseLSTMTrain.R** and **LSTM/loadDenseLSTMVali.R** raw Landsat band values are created for model input for the dense LSTM model. For this, missing band values are linearly interpolated. *inputs* refer to input features as used for the LSTM model, whereas *targets* refer to response variables, i.e., LCF.  

3. The **differentInput** folder contains scripts for different LSTM (**differentInput/LSTM**) and RF regression (**differentInput/RF**) training and validation input. Here, different LSTM input is created in which input is derived for the 2015 training locations, as opposed to for the 2015-2018 locations, and vice versa for different RF input. Furthermore, for the LSTM input, band values are discarded to mimic similar input to that of the RF regression models before, whereas for the RF regression models, vice versa (they are included). For the annual LSTM model, VIs are derived over again three-year periods surrounding the years 2015, 2016, 2017 and 2018, but now from the 2015 locations (**differentInput/LSTM/loadLSTMTrain2015.R**). For this, the land cover fractions are repeated for those years. The validation data frame remains the same as before (band values are simply excluded). For the dense LSTM model, for training a VI time series of the 2015 locations, with missing values linearly interpolated, is used (**differentInput/LSTM/loadDenseLSTMTrain2015.R**). A VI time series of the validation locations are created in **differentInput/LSTM/loadDenseLSTMVali.R**. The RF regression models can use the training and validation data frames created before for the LSTM models (as they contain band values and the training data frame was created from the 2015-2018 locations). For the validation data frames for the dense model (**differentInput/RF/loadDenseRFValiBands.R**), similar code is used to before (**src/1-developing/RF/loadRFDenseVali.R**), except now the band median between every three-year period is also used.           

### Modelling

Open **src/2-modelling**.

4. **LSTM** and **RF** folders contain Python notebook and R (resp.) scripts to model annual (4-time steps) and dense (92-time steps) LCF. **LSTM/LSTM.ipynb** contains code for the annual LSTM model. First, input and targets are created from the created training and validation data. Then, training data is split and a model is defined and at last, a training/prediction loop is used to make predictions. Predictions can be plotted with the validation data to already see the quality. Additional functions to oversample certain classes and to use custom loss functions are included (and optional, not used in this study). **LSTM/denseLSTM.ipynb** contains the code for the dense LSTM model and uses a similar structure. It contains a function to regress annual LCF into sub-annual LCF by linearly interpolating between growing seasons. During prediction, the model already makes aggregated predictions by averaging predictions based on the growing season. These aggregated predictions are used to calculate errors (LCF were only recorded yearly). **RandomForest.R** contains the code for the annual RF model. It creates class models and applies them to the features derived for 2015-2018, whereas **denseRandomForest.R**, code for the dense RF model, does the same but uses features derived over each acquisition date. 

5. **denseRFCombine.R** and **denseRFMean.R** are scripts to combine the dense RF predictions into one big data frame and to average into an annual data frame based on the growing season resp. The first is used later during post-processing (and plotting), whereas the latter is used to calculate errors.

### Post-processing

Open **src/3-post-processing**.

6. **PostLSTM** and **Markov** folders contain Python notebooks in which the annual and dense predictions made by the previous RF and LSTM models are post-processed. **PostLSTM/PostLSTM.ipynb** is similar to **src/1-developing/LSTM/LSTM.ipynb**, except that it includes the predictions made by the LSTM and RF models. On the other hand, **PostLSTM/densePostLSTM.ipynb** includes predictions made by the dense LSTM and RF models. Both make use of sub-models, in which output is passed from sub-model to sub-model in a `greedy layer-training` wise fashion. Instead of output predictions, the annual model includes the use of hidden features to be used when passing the output to each sub-model (if preferred, not used in this study). For training, both create synthetic LCF by undertraining a separate LSTM model. Further functions to oversample change samples are optional, but not used in this study. **PostLSTM/densePostLSTM.ipynb** uses a separate loss function, in which loss over samples with change is extra penalised. For both, extra data sets are also created in which only actual LCF is used, only synth is used, LCF is not regressed or is not normalised (optional). Furthermore, the user may increase the number of sub-models (or `layers`, in this study, only two were used). In **Markov/Markov.ipynb** and **denseMarkov.ipynb**, predictions are split into years and acquisition dates for the annual and dense models resp. By iterating over a fraction trajectory, fractions are increased or decreased at each iteration (the amount is determined by the `stepsize` parameter, 1 used). Functions calculate energy and are used to keep track of energy and to stop iterating if it does not decrease and is stable. A co-occurrence matrix is calculated to develop a third constraint of unlikely co-occurrence, with `param` parameter to balance it (0.01 used).

### Accuracy Assessment

Open **src/4-accuracy-assessment**.

7. Run **subsetVali.R** first. Here, the created 2015-2018 validation data frame is split into years, as well as subsetted based on change and 'possible' change (the latter not used in this study). These are used to calculate yearly errors. Additionally, class prevalence is counted.

8. **accuracyAssessment.R** calculates OA (using a `sub-pixel confusion matrix` defined in **utils/SCM.R** and from https://github.com/GreatEmerald/SCM), RMSE and MAE. Furthermore, dissimilarity and slope coefficient is calculated by iterating over sample sites, time series and time steps. Jaccard similarity and correctly predicted 0 and 100% fractions are also created. For interested users, the *Kullback–Leibler-* (KL-)divergence is also calculated. 

9. **JaccardBars.ipynb** and **plotTimeSeries.ipynb** are Python notebooks to plot bar plots of the Jaccard similarities and time series of the predicted fractions, with validation data, resp.
