# -*- coding: utf-8 -*-
"""
Created on Sun Oct 18 02:22:30 2020

@author: itzro
"""

import pandas as pd
import numpy as np
import seaborn as sns
sns.set()
import matplotlib.pyplot as plt  

df = pd.read_csv("C:/Users/itzro/Downloads/heart_failure_clinical_records_dataset.csv")
df.head
df.isnull().any().any()
import plotly.figure_factory as ff
hist_data =[df["age"].values]
group_labels = ['age'] 

fig = ff.create_distplot(hist_data, group_labels)
fig.show()
import plotly.express as px
fig = px.box(df, x='sex', y='age')
fig.show()
import plotly.graph_objs as go
male = df[df["sex"]==1]
female = df[df["sex"]==0]
df.anaemia.value_counts()
df.high_blood_pressure.value_counts()
df.smoking.value_counts()

male_survi = male[df["DEATH_EVENT"]==0]
male_not = male[df["DEATH_EVENT"]==1]
female_survi = female[df["DEATH_EVENT"]==0]
female_not = female[df["DEATH_EVENT"]==1]
df.hist()

labels = ['Male - Survived','Male - Not Survived', "Female -  Survived", "Female - Not Survived"]
values = [len(male[df["DEATH_EVENT"]==0]),len(male[df["DEATH_EVENT"]==1]),
         len(female[df["DEATH_EVENT"]==0]),len(female[df["DEATH_EVENT"]==1])]
fig = go.Figure(data=[go.Pie(labels=labels, values=values, hole=.4)])
fig.show()
svd = df[df['DEATH_EVENT']==0]['serum_sodium']
not_svd = df[df['DEATH_EVENT']==1]['serum_sodium']
hist_data = [svd,not_svd]
group_labels = ['Survived', 'Not Survived']
fig = ff.create_distplot(hist_data, group_labels, bin_size=0.5)
fig.show()
plt.figure(figsize=(10,10))
sns.heatmap(df.corr(), vmin=-1, annot=True);
fig = px.histogram(df, x="serum_creatinine", color="DEATH_EVENT", marginal="violin", hover_data=df.columns)
fig.show()
features = ['time','ejection_fraction','serum_creatinine','age']
x = df[features]
y = df["DEATH_EVENT"]
from sklearn.model_selection import train_test_split
x_train,x_test,y_train,y_test = train_test_split(x,y, test_size=0.2, random_state=0)

#Logistic Regression
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix, accuracy_score
logit = LogisticRegression()
logit.fit(x_train, y_train)
logit_pred = logit.predict(x_test)
logit_acc = accuracy_score(y_test, logit_pred)
accuracy_list= []
accuracy_list.append(100*logit_acc)
100*logit_acc


#Kneigbors Classification
from sklearn.neighbors import KNeighborsClassifier as clf
accuracy=[]
for k in range(1,30):
    model= clf(n_neighbors=k)
    model.fit(x_train,y_train)
    score = model.score(x_test,y_test)
    print("k=%d, accuracy=%.2f%%" % (k, score * 100))
    #accuracy.append(score)

#Decsion Trees
from sklearn.tree import DecisionTreeClassifier as dtf 
dtf_cl=dtf(max_leaf_nodes=3, random_state=0, criterion='entropy')
dtf_cl.fit(x_train,y_train)
dtf_predict= dtf_cl.predict(x_test)
dtf_cl.score(x_test,y_test)

#gradientboosting
from sklearn.ensemble import GradientBoostingClassifier
gbclass=GradientBoostingClassifier(max_depth=1,random_state=0)
modelgbf= gbclass.fit(x_train,y_train)
gbypred=gbclass.predict(x_test)
gbclass.score(x_test, y_test) 
print(modelgbf.feature_importances_)

#XGBoost
from xgboost import XGBClassifier
from xgboost import plot_importance
xgbc=  XGBClassifier()
xgbc.fit(x_train,y_train)
xgbc_predict = xgbc.predict(x_test)
xgbc.score(x_test,y_test)
plot_importance(xgbc)
plt.show()


