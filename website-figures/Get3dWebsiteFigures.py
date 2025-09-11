#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
===========================================================================
Replication Code for 3D CART Figures
===========================================================================

This module contains the replication code that generates all 3D visualizations
of decision boundaries based on the training dataset `train_data.csv`.

Dependencies:
- pandas
- numpy
- scikit-learn
- matplotlib
- plotly
- kaleido (required for exporting images in notebooks)

Installation:
- Install all at once: pip install -r requirements.txt
- Or individually: pip install pandas numpy scikit-learn matplotlib plotly kaleido
- For kaleido in notebooks:
    # !pip install -U kaleido
    # Then restart the runtime/session

Last edited: 05.07.2025
===========================================================================
"""

# imports 
import pandas as pd
import numpy as np
from sklearn.tree import DecisionTreeClassifier, plot_tree
from sklearn.preprocessing import LabelEncoder
import matplotlib.pyplot as plt
import plotly.graph_objects as go
from itertools import product
import plotly.io as pio

# prepare data 
train_data = pd.read_csv("train_data.csv")
features = ["Passquote", "Fouls", "Zweikampf"]
X = train_data[features]
y = train_data["CL"]
le = LabelEncoder()
y_encoded = le.fit_transform(y)

# fit tree
my_tree = DecisionTreeClassifier(
    criterion="gini",
    max_depth=3,
    min_samples_leaf=1,
    min_samples_split=2,
    random_state=55,
    min_impurity_decrease=0.02
)
my_tree.fit(X, y_encoded)

# plot 2D tree
plt.figure(figsize=(20, 10))
plot_tree(
    my_tree,
    feature_names=features,
    class_names=le.classes_,
    filled=True,
    rounded=True,
    fontsize=10
)
plt.title("CART Tree", fontsize=16)
plt.show()

# prepare 3D decision boundaries plot 
X_values = X.values
tree_struct = my_tree.tree_
feature_bounds = np.array([[X_values[:,i].min(), X_values[:,i].max()] for i in range(3)])
colors = ["#cbfb04", "#87b2f3"]          # colors of fu:stat website 
color_points = ["#729806", "#2e79e9"]    # colors of fu:stat website 

# auxiliary function: create cube for tree node 
def create_cube(bounds, color='blue', opacity=0.3):
    corners = np.array(list(product(*bounds)))
    faces = [[0,1,3,2], [4,5,7,6], [0,1,5,4], [2,3,7,6], [0,2,6,4], [1,3,7,5]]
    x, y, z = corners[:,0], corners[:,1], corners[:,2]
    i, j, k = [], [], []
    for face in faces:
        i += [face[0], face[0]]
        j += [face[1], face[2]]
        k += [face[2], face[3]]
    return go.Mesh3d(x=x, y=y, z=z, i=i, j=j, k=k, color=color, opacity=opacity, flatshading=True, showscale=False)

# plot 3D decision boundaries
fig = go.Figure()

# auxiliary function: recursive tree traversal to create cubes 
def traverse(node_id, bounds, depth=0):
    """Traverse the tree recursively and add cubes for leaf nodes"""
    if tree_struct.children_left[node_id] == tree_struct.children_right[node_id]:  
        class_id = np.argmax(tree_struct.value[node_id][0])
        color = colors[class_id % len(colors)]
        fig.add_trace(create_cube(bounds, color=color, opacity=0.2 + 0.1*depth))
        return
    feat = tree_struct.feature[node_id]
    thr = tree_struct.threshold[node_id]
    left_bounds = bounds.copy()
    left_bounds[feat, 1] = thr
    right_bounds = bounds.copy()
    right_bounds[feat, 0] = thr
    traverse(tree_struct.children_left[node_id], left_bounds, depth+1)
    traverse(tree_struct.children_right[node_id], right_bounds, depth+1)

# start traversal from root node
traverse(0, feature_bounds)

# add training data points to plot
fig.add_trace(go.Scatter3d(
    x=X_values[:, 0], y=X_values[:, 1], z=X_values[:, 2],
    mode='markers',
    marker=dict(size=7, color=y_encoded, colorscale=color_points, opacity=0.8),
    name='Training Data'
))

# final touches
fig.update_layout(
    scene=dict(
        xaxis=dict(title=features[0], titlefont=dict(size=18, color='blue'), tickfont=dict(size=14, color='blue'),
                   gridcolor='blue', zerolinecolor='blue', backgroundcolor='rgba(0,0,0,0)'),
        yaxis=dict(title=features[1], titlefont=dict(size=18, color='blue'), tickfont=dict(size=14, color='blue'),
                   gridcolor='blue', zerolinecolor='blue', backgroundcolor='rgba(0,0,0,0)'),
        zaxis=dict(title=features[2], titlefont=dict(size=18, color='blue'), tickfont=dict(size=14, color='blue'),
                   gridcolor='blue', zerolinecolor='blue', backgroundcolor='rgba(0,0,0,0)')
    ),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)',
    margin=dict(l=0, r=0, b=0, t=0),
    legend=dict(x=0.01, y=0.99, font=dict(color='blue'))
)

fig.show()

# export from different camera views
# !pip install -U kaleido then go to Runtime -> Restart Session!!!

camera_views = [
    dict(eye=dict(x=1.5, y=1.5, z=1.5)),
    dict(eye=dict(x=-1.5, y=1.5, z=1.5)),
    dict(eye=dict(x=1.5, y=-1.5, z=1.5)),
]

for i, cam in enumerate(camera_views):
    fig.update_layout(scene_camera=cam)
    fig.write_image(f"3Dplot_{i+1}.png", width=800, height=800, scale=2)
