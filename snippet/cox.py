# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 00:44:05 2019

@author: HIEU
"""

import pandas as pd
import numpy as np

def cox_basehaz(lp, time, dead):
#Find baseline hazard for Cox model using Breslow method
#Adapted from https://stats.stackexchange.com/questions/46532/cox-baseline-hazard
#Inputs are Numpy arrays.
#lp=Cox model linear predictor values
#time=vector of failure/censoring times
#dead=boolean, did patient fail/die
#
#Returns:
#1: unique failure times
#2: baseline hazard function at these times
    time=pd.Series(time)
    dead=pd.Series(dead)
    prediction = np.expand_dims(np.exp(lp),1)
    failed_times = time.loc[dead==1]
    y = failed_times.value_counts().sort_index().index.values #ordered distinct event times
    d = failed_times.value_counts().sort_index().values #number of events
    h0 = np.zeros(len(y))
    for l in range(len(y)):
        h0[l] = d[l] / np.sum(prediction[time >= y[l]])
    H0 = np.cumsum(h0)
	#surv_baseline = np.exp(-H0)
    return (y, H0)


def cox_pred_surv(lp, H0):
#Predicted survival curves from Cox model
#Inputs are Numpy arrays.
#lp=Cox model linear predictor values
#H0=basline hazard function
#
#Returns: predicted survival rate at each follow-up time
	prediction = np.expand_dims(np.exp(lp),1)
	return np.exp(-np.matmul(prediction, np.expand_dims(H0,0)))

#valid_df_dead = valid_df.event.values.astype(bool)
#times, H0 = cox_basehaz(model.predict_risk(train_data['x'].astype('float32')).
#                        flatten(), train_df.time.values, train_df.event.values.astype(bool))
#
#y_pred = cox_pred_surv(model.predict_risk(valid_data['x'].astype('float32')).flatten(), H0)
#
#csv
#pred_surv = y_pred
#trained_time = times

#plt.plot(times/365.25, y_pred[2,:])

