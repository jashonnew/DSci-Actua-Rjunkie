# -*- coding: utf-8 -*-
"""
Created on Wed Feb 12 15:13:39 2020

@author: jasho
"""

import numpy as np
import os
import pandas as pd
import seaborn as sn
import matplotlib
import matplotlib.pyplot as plt
import sklearn.metrics
from sklearn.naive_bayes import GaussianNB
from sklearn import tree
from sklearn.preprocessing import LabelEncoder
from statistics import mode
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from imblearn.over_sampling import RandomOverSampler
from collections import Counter

 #%%
#os.chdir("Users\jasho\OneDrive\Desktop\Winter_2020\CS_450\prove_4")
names = {"Gender" : {"F" : 0, "M" : 1, "U" : 2}, "JointMarried" : {"Y" : 1, "N" : 0}, "MemberType" : {"open loan - dl only" : 0, "closed loan - never converted" : 1, "converted member" : 2}}

wmdat = pd.read_csv("dat_targ.csv")
class_names = wmdat["MemberType"].unique()
wmdat.replace(names, inplace = True)
wm_targ = wmdat["MemberType"]
wmdat = wmdat.drop("MemberType", axis = 1)

wm, wm_test, wm_targets, wm_test_targets = train_test_split(wmdat, wm_targ)

ros = RandomOverSampler(random_state=0)
wm_resampled, wm_targets_resampled = ros.fit_resample(wm, wm_targets)
#%%
classifier = GaussianNB()
classifier.fit(wm_resampled, wm_targets_resampled)
targets_predicted = classifier.predict(wm_test)
#print(targets_predicted)
acc = sklearn.metrics.accuracy_score(wm_test_targets, targets_predicted)
rec = sklearn.metrics.recall_score(wm_test_targets, targets_predicted, average='macro')
prec = sklearn.metrics.precision_score(wm_test_targets, targets_predicted, average='macro')
print("Accuracy : {} \n Recall : {} \n Precision : {}".format(acc, rec, prec))

#%%

array = confusion_matrix(wm_test_targets, targets_predicted)
df_wm = pd.DataFrame(array)
plt.figure(figsize = (10,7))
sn.heatmap(df_wm, annot = True, fmt = 'g')
plt.xlabel("Predicted")
plt.ylabel("Actual")
plt.title("WM confusion matrix")

