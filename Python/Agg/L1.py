import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import torch
import torch.nn as nn
from numpy import hstack
from numpy import array
from torch.autograd import Variable
from torch.utils.data import Dataset, DataLoader, TensorDataset
from sklearn.preprocessing import MinMaxScaler

traindf=pd.read_csv('train20152018.csv')
validf=pd.read_csv('vali20152018.csv')

#traindf.head()
#print(traindf.columns.values,validf.columns.values)

# Input features (make sure they are existing columns in dfs)
vars = ['b1_median', 'b2_median', 'b3_median',
       'b4_median', 'b5_median', 'b6_median', 'b7_median', 'nbr_median',
       'ndmi_median', 'ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr',
       'min', 'max', 'intercept', 'co', 'si', 'co2', 'si2', 'trend',
       'phase1', 'amplitude1', 'phase2', 'amplitude2']

# Output features (targets) (make sure they are existing columns in dfs)
targets = ['bare', 'crops',
       'grassland', 'shrub', 'tree', 'urban_built_up', 'water']

# Create X (input features) in loop
trainX_list = []
testX_list = []

for colname in vars: 
  # Get column of feature
  col_train = traindf[colname]
  col_test = validf[colname]

  # Create train and test tensor
  var_train = torch.tensor(col_train.values).view(-1, 4, 1)
  var_test = torch.tensor(col_test.values).view(-1, 4, 1)

  # Convert to float32 (used in LSTM)
  var_train = var_train.to(dtype=torch.float32)
  var_test = var_test.to(dtype=torch.float32)

  # Append to lists
  trainX_list.append(var_train)
  testX_list.append(var_test)

# Do the same for Y (targets)
trainY_list = []
testY_list = []

for colname in targets: 
  # Get column of feature
  col_train = traindf[colname]
  col_test = validf[colname]

  # Create train and test tensor
  var_train = torch.tensor(col_train.values).view(-1, 4, 1)
  var_test = torch.tensor(col_test.values).view(-1, 4, 1)

  # Convert to float32 (used in LSTM)
  var_train = var_train.to(dtype=torch.float32)
  var_test = var_test.to(dtype=torch.float32)

  # Append to lists
  trainY_list.append(var_train)
  testY_list.append(var_test)

# We now have multiple features as tensors but we want them as one. So concatenate the tensors along the last dimension
X_train = torch.cat(trainX_list, dim=2)
X_test = torch.cat(testX_list, dim=2)
Y_train = torch.cat(trainY_list, dim=2)
Y_test = torch.cat(testY_list, dim=2)

# Now check if any tensors contain nan values and if so remove them (entire matrix). LSTM will not work (correctly) with nan values.
X_train_old = X_train
X_test_old = X_test
Y_train_old = Y_train
Y_test_old = Y_test

# create an empty mask
mask = None

# If X_train contains nan, apply mask to Y_train and vice versa. Same applies for X and Y_test.
if torch.isnan(X_train).any():
    mask = torch.isnan(X_train).any(dim=1).any(dim=1)
    X_train = X_train[~mask]
    Y_train = Y_train[~mask] 
    #Y_train = torch.masked_select(Y_train, mask)
if torch.isnan(Y_train).any():
    mask = torch.isnan(Y_train).any(dim=1).any(dim=1) 
    Y_train = Y_train[~mask]
    X_train = X_train[~mask]
if torch.isnan(X_test).any():
    mask = torch.isnan(X_test).any(dim=1).any(dim=1) 
    X_test = X_test[~mask]
    Y_test = Y_test[~mask]
if torch.isnan(Y_test).any():
    mask = torch.isnan(Y_test).any(dim=1).any(dim=1) 
    Y_test = Y_test[~mask]
    X_test = X_test[~mask]

# Check final tensors (including shape)
# print("Train X contained nan values: ", torch.isnan(X_train_old).any(), "new train X contains nan values: ", torch.isnan(X_train).any())
# print("Old shape of train X: ", X_train_old.shape, "New shape: ", X_train.shape, "\n")
# print("Train Y contained nan values: ", torch.isnan(Y_train_old).any(), "new train Y contains nan values: ", torch.isnan(Y_train).any())
# print("Old shape of train Y: ", Y_train_old.shape, "New shape: ", Y_train.shape, "\n")

# print("Test X contained nan values: ", torch.isnan(X_test_old).any(), "new test X contains nan values: ", torch.isnan(X_test).any())
# print("Old shape of test X: ", X_test_old.shape, "New shape: ", X_test.shape, "\n")
# print("Test Y contained nan values: ", torch.isnan(Y_test_old).any(), "new test Y contains nan values: ", torch.isnan(Y_test).any())
# print("Old shape of test Y: ", Y_test_old.shape, "New shape: ", Y_test.shape, "\n")

# Calculate the mean and standard deviation of each feature in the training set
X_mean = X_train.mean(dim=0)
X_std = X_train.std(dim=0)

# Standardize the training set
X_train = (X_train - X_mean) / X_std

# Standardize the test set using the mean and standard deviation of the training set
X_test = (X_test - X_mean) / X_std

# Create the TensorDataset for the training set
train_dataset = TensorDataset(X_train, Y_train)

# Create the TensorDataset for the test set
test_dataset = TensorDataset(X_test, Y_test)

# Create a DataLoader for the training set
train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=64, shuffle=True) # batch size 64 (from param test)

# Create a DataLoader for the test set
test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=64, shuffle=False)

# Out of the tested options, the following combination of hyperparameters achieved the lowest loss:
# Optimizer: Adam, LR: 0.01, hidden size: 256, layers: 1, dropout: 0.0, epochs: 100, batch size: 64, weight init: xavier, activation: tanh

import torch.nn as nn
import torch.optim as optim
import torch.nn.init as init
import progressbar

# Define the LSTM model
class LSTMModel(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, num_layers=1, dropout=0, activation='tanh'):
        super().__init__()
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers=num_layers, batch_first=True)
        self.dropout = nn.Dropout(dropout)
        self.linear = nn.Linear(hidden_size, output_size)
        init.xavier_uniform_(self.linear.weight)
        
    def forward(self, x):
        x, _ = self.lstm(x)
        x = self.dropout(x)
        x = self.linear(x)
        x = torch.clamp(x, min=0, max=100)  # Constrain the output to be between 0 and 100
        return x

# Number of input and output features
input_number = len(vars)
output_number = len(targets)

# Instantiate the model
model = LSTMModel(input_size=input_number, hidden_size=256, output_size=output_number)

# Define the loss function and optimizer
loss_fn = nn.L1Loss() # L1Loss!
optimizer = torch.optim.Adam(model.parameters(), lr=0.01)

# Make lists to retain output
losses_train_epochs = []
losses_test_epochs = []

pred_test = []
actual_test = []

# Epochs
num_epochs = 50 # The higher, the longer it takes but the better idea of the model's fiting ability
bar = progressbar.ProgressBar(maxval=num_epochs).start()

# Set the model to training mode  
model.train()

# Loop over the training epochs
for epoch in range(num_epochs):

  # Progress Bar
  bar.update(epoch) 

  # Loss per epoch
  epoch_trainloss = []
  epoch_testloss = []

  # Predictions / actual values per epoch
  epoch_pred = []
  epoch_actual = []

  # Loop over the training set
  for X, Y in train_loader:

      # Clear the gradients
      optimizer.zero_grad()
      
      # Forward pass
      Y_pred = model(X)

      # Compute the loss
      loss = loss_fn(Y_pred, Y)
      
      # Backward pass
      loss.backward()
      
      # Update the parameters
      optimizer.step()

      # Append the loss to the lists
      epoch_trainloss.append(loss.item())

  # Loop over the test set
  for X, Y in test_loader:

      # Set the model to evaluation mode
      model.eval()

      # Forward pass
      Y_pred = model(X)
      
      # Convert from tensor to list
      Y_pred_list = [x.tolist() for x in Y_pred]
      Y_list = [x.tolist() for x in Y]

      # Append prediction to list
      epoch_pred.append(Y_pred_list)
      epoch_actual.append(Y_list)
      
      # Compute the loss
      loss = loss_fn(Y_pred, Y)
      
      # Append the loss to the list
      epoch_testloss.append(loss.item())

  # Add epoch losses as index (= epoch) in total loss list (easy to find later)
  losses_train_epochs.append(epoch_trainloss)
  losses_test_epochs.append(epoch_testloss)

  # Add prediction and actual values in epoch to total list
  pred_test.append(epoch_pred)
  actual_test.append(epoch_actual)
  
  from matplotlib import pyplot as plt

# Average loss per epoch
ltrain_epochs = []
for epoch_list in losses_train_epochs:
  epoch_avg = sum(epoch_list) / len(train_loader)
  ltrain_epochs.append(epoch_avg)

ltest_epochs = []
for epoch_list in losses_test_epochs:
  epoch_avg = sum(epoch_list) / len(train_loader)
  ltest_epochs.append(epoch_avg)

# Print epoch achieving minimum test loss
min_value = min(ltest_epochs)
min_index = ltest_epochs.index(min_value)

print("The minimum test loss:", round(min_value,2), "is found at epoch:", min_index, "\n")

# Plot the losses over time
plt.plot(ltrain_epochs, label="Training loss")
plt.plot(ltest_epochs, label="Test loss")
plt.xlabel('Epoch')
plt.ylabel('Average loss')
plt.legend()
plt.show()

from matplotlib import pyplot as plt

# # Make lists of predictions and actual fractions (reference)
nested_pred = pred_test[min_index] # results from the epoch with the minimum loss is taken
nested_actual = actual_test[min_index]

unnested_pred = []
for data in nested_pred:
  for batch in data:
    for timestep in batch:
      for target in range(len(timestep)):
        unnested_pred.append(timestep[target])

unnested_actual = []
for data in nested_actual:
  for batch in data:
    for timestep in batch:
      for target in range(len(timestep)):
        unnested_actual.append(timestep[target])

# These lists contain output for entire predictions 
# Now retain lists for each class
pred_class = {}
true_class = {}

# Initialize lists for each class in predictions
for i in range(len(targets)):
  pred_class[f'{targets[i]}'] = unnested_pred[i::len(targets)]

# Initialize lists for each class in reference data
for i in range(len(targets)):
  true_class[f'{targets[i]}'] = unnested_actual[i::len(targets)]

# Plot the lists as graphs

# Loop through the data and plot the actual and predicted values
for i in range(len(targets)):
  # Get the data for the current class
  actual = true_class[f'{targets[i]}']
  predicted = pred_class[f'{targets[i]}']

  # Define the x-axis data as a range of values from 0 to the length of the data
  x = range(len(actual))

  # Create a new figure
  fig = plt.figure(i)

  # Create a figure with certain size
  fig = plt.figure(figsize=(20, 3))

  # Create axes
  ax = fig.add_subplot(1, 1, 1)

  # Plot the actual and predicted values
  ax.plot(x, actual, label='Actual')
  ax.plot(x, predicted, label='Predicted')

  # Add a legend
  ax.legend()

  # Set the title using the class name
  var_name = f'{targets[i]}'
  ax.
  
  set_title(var_name)

  # Show the figure
  plt.show()
  
  # Add IDs (test data was not shuffled so can add like this --- checked with excel!)
import pandas as pd
pred_df = validf.loc[:, ['sample_id', 'location_id', 'validation_id', 'reference_year']]

# Adds predictions to df 
for i in range(len(targets)):
  data = pred_class[f'{targets[i]}']
  pred_df[targets[i]] = data 

# Show df
print(pred_df.head(32))

import math
import statistics
from sklearn.metrics import mean_squared_error

# RMSE
print("RMSE based on the prediction and validation dataframe: ")

# RMSE from columns
for target in targets:
    predicted = pred_df[target]
    actual = validf[target]
    rmse_column = mean_squared_error(predicted, actual) ** 0.5
    print(f'RMSE for {target}: {rmse_column}')
    
pred_df.to_csv('LSTM_predict_L1.csv')

