# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 02:29:27 2019

@author: HIEU
"""

import pandas as pd

def read_pickle_file(file):
    pickle_data = pd.read_pickle(file)
    return pickle_data