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

traindf=pd.read_csv('train20152018_noXY.csv')
validf=pd.read_csv('vali20152018_noXY.csv')

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
print("Train X contained nan values: ", torch.isnan(X_train_old).any(), "new train X contains nan values: ", torch.isnan(X_train).any())
print("Old shape of train X: ", X_train_old.shape, "New shape: ", X_train.shape, "\n")
print("Train Y contained nan values: ", torch.isnan(Y_train_old).any(), "new train Y contains nan values: ", torch.isnan(Y_train).any())
print("Old shape of train Y: ", Y_train_old.shape, "New shape: ", Y_train.shape, "\n")

print("Test X contained nan values: ", torch.isnan(X_test_old).any(), "new test X contains nan values: ", torch.isnan(X_test).any())
print("Old shape of test X: ", X_test_old.shape, "New shape: ", X_test.shape, "\n")
print("Test Y contained nan values: ", torch.isnan(Y_test_old).any(), "new test Y contains nan values: ", torch.isnan(Y_test).any())
print("Old shape of test Y: ", Y_test_old.shape, "New shape: ", Y_test.shape, "\n")

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

# Data loaders. Now commented out as different batch sizes are tested in the 'parameters loop'
# train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=5, shuffle=True)
# test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=5, shuffle=False)

print(train_dataset[0]) # Looks good (?)
print(test_dataset[0]) # Looks good (?)

# Define the LSTM model
class LSTMModel(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, batch_first=True):
        super().__init__()
        self.lstm = nn.LSTM(input_size, hidden_size, batch_first=batch_first)
        self.linear = nn.Linear(hidden_size, output_size)
        
    def forward(self, x):
        x, _ = self.lstm(x)
        x = self.linear(x)
        x = torch.clamp(x, min=0, max=100)  # Constrain the output to be between 0 and 100
        return x

# Function for training and test loop, and return RMSE/MAE
def train_model(model, optimizer, criterion, epoch, batch_size):

    # Create a DataLoaders from the training and test datasets with batch size as variable
    train_loader = torch.utils.data.DataLoader(train_dataset, batch_size = batch_size, shuffle=True)  
    test_loader = torch.utils.data.DataLoader(test_dataset, batch_size = batch_size, shuffle=False) 

    # Set the model to training mode  
    model.train()

    for number in range(epoch):
  
    # Loop over the training set
      for X, Y in train_loader:

        # Clear the gradients
        optimizer.zero_grad()
                
        # Forward pass
        Y_pred = model(X)
                
        # Compute the loss
        loss = criterion(Y_pred, Y)
                
        # Backward pass
        loss.backward()
              
        # Update the parameters
        optimizer.step()

    # Set the model to evaluation mode
    model.eval()

    with torch.no_grad():
        loss_rmse = 0
        loss_mae = 0

    # Loop over the test set
    for X, Y in test_loader:
        # Forward pass
        Y_pred = model(X)

        # Compute the loss (MAE, as well as RMSE)
        loss_mae += criterion_L1(Y_pred, Y).item()
        loss_rmse += criterion_MSE(Y_pred, Y).item()
      
    # Compute average loss
    loss_rmse /= len(test_loader)
    rmse = np.sqrt(loss_rmse)
    loss_mae /= len(test_loader)
    return rmse, loss_mae
  
 # Define the hyperparameters you want to test
optimizers = ['Adam', 'SGD', 'RMSprop']
learning_rates = [1e-1, 1e-2, 1e-3, 1e-4, 1e-5]
hidden_sizes = [64, 128, 256]
num_layers = [1]
dropouts = [0, 0.2, 0.5] 
epochs = [1, 10, 50]
batch_sizes = [16, 64, 128, 512]
weight_inits = ["xavier", "kaiming", "uniform"]
activations = ["relu", "tanh", "sigmoid"]

# Lists of results (handy to obtain for later)
rmse_list = []
mae_list = []
param_list = []

# Loop over the hyperparameters
for optimizer in optimizers:
  for learning_rate in learning_rates:
    for hidden_size in hidden_sizes:
      for num_layer in num_layers:
        for dropout in dropouts:
          for epoch in epochs:
            for batch_size in batch_sizes:
              for weight_init in weight_inits:
                for activation in activations:
          
                  # Instantiate the model with the parameters
                  model = LSTMModel(input_size=len(vars), hidden_size=hidden_size, output_size=len(targets))
                                    
                  # Set the hyperparameters in the model
                  model.lstm.hidden_size = hidden_size
                  model.lstm.num_layers = num_layer
                  model.lstm.dropout = dropout
                  model.linear.weight_init = weight_init
                  model.linear.activation = activation
                  
                  # Define the loss function and the optimizer with the current learning rate and optimizer
                  if optimizer == 'Adam':
                    optim = torch.optim.Adam
                  elif optimizer == 'SGD':
                    optim = torch.optim.SGD
                  elif optimizer == 'RMSprop':
                    optim = torch.optim.RMSprop

                  optimizer = optim(model.parameters(), lr=learning_rate)

                  criterion = nn.SmoothL1Loss() # This one is used to update the gradients
                  criterion_L1 = nn.L1Loss() 
                  criterion_MSE = nn.MSELoss() 


                  # Apply model and get metrics
                  rmse, mae = train_model(model, optimizer, criterion, epoch, batch_size)
                  params = [optimizer.__class__.__name__, learning_rate, hidden_size, num_layer, dropout, epoch, batch_size, weight_init, activation]

                  # Append to lists
                  rmse_list.append(rmse)
                  mae_list.append(mae)
                  param_list.append(params)

                  # Print the metrics and combination of hyperparameters
                  print(f"Hidden size: {hidden_size}, Num layers: {num_layer}, Learning rate: {learning_rate}, Optimizer: {optimizer.__class__.__name__}, Dropout: {dropout}, Epochs: {epoch}, Batch size: {batch_size}, Weight init: {weight_init}, Activation: {activation} -> RMSE: {rmse:.3f} -> MAE: {mae:.3f}")

                  # Create data frame
param_df = pd.DataFrame(param_list, columns=['Optimizer', 'Learning rate', 'Hidden size', 'Num layer', 'Dropout', 'Epochs', 'Batch size', 'Weight init', 'Activation'])
param_df['RMSE'] = rmse_list
param_df['MAE'] = mae_list

print(param_df)

# RMSE and MAE dfs
RMSE_df = param_df.sort_values('RMSE', ascending=True)
MAE_df = param_df.sort_values('MAE', ascending=True)

# Print best 5 combinations
pd.set_option('display.width', 1000)
print("Best combis for low RMSE: ")
print(RMSE_df.head(5), "\n")
print("Best combis for low MAE: ")
print(MAE_df.head(5))
