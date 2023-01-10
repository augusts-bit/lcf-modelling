## Drop x and y columns (private) so that data can be used in colab ##

# Read csv
change_train <- read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Input/train20152018.csv")
change_vali <- read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Input/vali20152018.csv")

# Drop columns (get rid of 'X' as well)
change_train$x <- NULL
change_train$y <- NULL
change_train$X <- NULL

change_vali$x <- NULL
change_vali$y <- NULL
change_vali$X <- NULL

# Write new csv
write.csv(change_train, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Input/train20152018_noXY.csv")
write.csv(change_vali, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Input/vali20152018_noXY.csv")