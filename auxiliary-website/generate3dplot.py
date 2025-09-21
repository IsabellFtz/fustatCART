"""
Dieses Skript trainiert einen Entscheidungsbaum mit drei Features auf den 1. Fußball Bundesligadaten und 
visualisiert die Baumstruktur (mit matplotlib) und 3D-Entscheidungsgrenzen (mit plotly). 
Letzeres wird als interaktiven plot als html file und aus drei Kameraperspektiven als png exportiert. 
"""


# import libraries
import pandas as pd
from sklearn.tree import DecisionTreeClassifier, plot_tree # pip install scikit-learn
from sklearn.preprocessing import LabelEncoder # pip install scikit-learn
import matplotlib.pyplot as plt
from itertools import product
import numpy as np
import plotly.graph_objects as go
import plotly.io as pio
# pip install -U kaleido # for camera view screenshots 


# load and prepare data
df_train = pd.read_csv("data_trainF3.csv")
features = ["Passquote", "Fouls","Zweikampf"]
X = df_train[features]
y = df_train["CL"]
le = LabelEncoder()
y_encoded = le.fit_transform(y)


# fit CART 
mytree = DecisionTreeClassifier(
    criterion="gini",
    ccp_alpha=0,
    max_depth=3,
    min_samples_leaf=1,
    min_samples_split=2,
    random_state=55,
    min_impurity_decrease=0.02  
)
mytree.fit(X, y_encoded)


# plot CART
plt.figure(figsize=(20, 10))
plot_tree(
    mytree,
    feature_names=features,
    class_names=le.classes_,
    filled=True,
    rounded=True,
    fontsize=10
)
plt.title("Tree", fontsize=16)
#plt.show()


# prepare for decision boundaries in 3D plot
X = X.values  
tree = mytree.tree_
feature_bounds = np.array([[X[:,i].min(), X[:,i].max()] for i in range(3)])
colors = ["#cbfb04", "#87b2f3"]
color_points = ["#729806","#2e79e9"]


# auxiliary function: create a cube
def create_cube(bounds, color='blue', opacity=0.3):
    corners = np.array(list(product(*bounds)))
    faces = [
        [0,1,3,2], [4,5,7,6],
        [0,1,5,4], [2,3,7,6],
        [0,2,6,4], [1,3,7,5]
    ]
    x, y, z = corners[:,0], corners[:,1], corners[:,2]
    i, j, k = [], [], []
    for face in faces:
        i += [face[0], face[0]]
        j += [face[1], face[2]]
        k += [face[2], face[3]]
    return go.Mesh3d(
        x=x, y=y, z=z,
        i=i, j=j, k=k,
        color=color,
        opacity=opacity,
        flatshading=True,
        showscale=False
    )
fig = go.Figure()


# auxiliary function: traverse tree and create cubes
def traverse(node_id, bounds, depth=0):
    if tree.children_left[node_id] == tree.children_right[node_id]:  # leaf
        values = tree.value[node_id][0]
        class_id = np.argmax(values)
        color = colors[class_id % len(colors)]
        fig.add_trace(create_cube(bounds, color=color, opacity=0.2 + 0.1*depth))
        return
    feat = tree.feature[node_id]
    thr = tree.threshold[node_id]
    left_bounds = bounds.copy()
    left_bounds[feat, 1] = thr
    right_bounds = bounds.copy()
    right_bounds[feat, 0] = thr
    traverse(tree.children_left[node_id], left_bounds, depth+1)
    traverse(tree.children_right[node_id], right_bounds, depth+1)

traverse(0, feature_bounds)


# add training data points to figure
fig.add_trace(go.Scatter3d(
    x=X[:, 0], y=X[:, 1], z=X[:, 2],
    mode='markers',
    marker=dict(size=7, color=y_encoded, colorscale=color_points, opacity=0.8),
    name='Training data'
))


# some final touches
fig.update_layout(
    scene=dict(
        xaxis=dict(title=dict(text=features[0], font=dict(size=18, color='lightgray')),
                   tickfont=dict(size=14, color='lightgray'),
                   gridcolor='lightgray', zerolinecolor='lightgray', color='lightgray', backgroundcolor='rgba(0,0,0,0)'),
        yaxis=dict(title=dict(text=features[1], font=dict(size=18, color='lightgray')),
                   tickfont=dict(size=14, color='lightgray'),
                   gridcolor='lightgray', zerolinecolor='lightgray', color='lightgray', backgroundcolor='rgba(0,0,0,0)'),
        zaxis=dict(title=dict(text=features[2], font=dict(size=18, color='lightgray')),
                   tickfont=dict(size=14, color='lightgray'),
                   gridcolor='lightgray', zerolinecolor='lightgray', color='lightgray', backgroundcolor='rgba(0,0,0,0)')
    ),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)',
    margin=dict(l=0, r=0, b=100, t=0),
    legend=dict(x=0.01, y=0.99, font=dict(color='lightgray'))
)

# fig.show()
# export as html file 
fig.write_html("plot.html", include_plotlyjs='cdn')


# take screenshots from different camera view and export as png files
camera_views = [
    dict(eye=dict(x=1.5, y=1.5, z=1.5)),
    dict(eye=dict(x=-1.5, y=1.5, z=1.5)),
    dict(eye=dict(x=1.5, y=-1.5, z=1.5)),
]
for i, cam in enumerate(camera_views):
    fig.update_layout(scene_camera=cam)
    fig.write_image(f"boundary3D_{i+1}.png", width=800, height=800, scale=2)