# Yogurt Sales Prediction

## Data
The data for training is stored in the folder [data_hs21](./data_hs21).

The data for the prediction is stored in the folder [data_hs21_pred](./data_hs21_pred).

## Training the models

To train the models run the RMD-file [yogurt_usecase-evaluate.Rmd](./yogurt_usecase-evaluate.Rmd).
(To see the plots you can also knit the RMD).

This is where the models are trained with the data. 

Afterwards a data frame is received which contains the best model for each yogurt. This is required for the prediction.

## Prediction 

To run this prediction please run [yogurt_usecase-evaluate.Rmd](./yogurt_usecase-evaluate.Rmd) first.

In a second step run [yogurt_usecase-predict.Rmd](./yogurt_usecase-predict.Rmd).