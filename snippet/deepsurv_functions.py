# -*- coding: utf-8 -*-
"""
Created on Thu Oct 10 12:39:38 2019

@author: HIEU
"""
import numpy as np
def dataframe_to_deepsurv_ds(df, event_col = 'Event', time_col = 'Time'):
    # Extract the event and time columns as numpy arrays
    e = df[event_col].values.astype(np.int32)
    t = df[time_col].values.astype(np.float32)
    # Extract the patient's covariates as a numpy array
    x_df = df.drop([event_col, time_col], axis = 1)
    x = x_df.values.astype(np.float32)
    # Return the deep surv dataframe
    return {
            'x' : x,
            'e' : e,
            't' : t
}