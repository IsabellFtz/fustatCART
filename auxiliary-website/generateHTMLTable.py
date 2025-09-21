"""
Dieses Skript lädt und bereinigt den 1. Fußball Bundesligadatensatz, erstellt Trainings- 
und Testdaten, berechnet deskriptive Statistiken für alle Features und generiert eine interaktive 
HTML-Tabelle mit Auswahlmöglichkeit zwischen Gesamt-, Trainings- und Testdatensatz.
"""

# import libraries
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from itertools import product
import matplotlib.pyplot as plt


# load datset
df = pd.read_csv("bundesliga_dataset.csv", na_values=["NA"], delimiter=";")
df.columns = df.columns.str.strip()  
df = df.copy()
df_train = pd.read_csv("data_trainF3.csv", na_values=["NA"], delimiter=",")


# prepare variables
df["CL"] = df["Zielklasse"].apply(lambda x: "CL" if x == "CL" else "Nicht-CL")
df["CL"] = df["CL"].astype("category")
df["CL_int"] = df["Zielklasse"].apply(lambda x: 1 if x == "CL" else 0).astype(int)
df["year"] = df["Club"].str[-2:]
df["Trainerwechsel_Vorsaison"] = pd.to_numeric(df["Trainerwechsel_Vorsaison"])
df["Titel_Vorsaison"] = pd.to_numeric(df["Titel_Vorsaison"])
df["Passquote_Vorsaison"] = pd.to_numeric(df["Passquote_Vorsaison"])
df["Ballbesitz_Vorsaison"] = pd.to_numeric(df["Ballbesitz_Vorsaison"])
df["Zweikampf_Vorsaison"] = pd.to_numeric(df["Zweikampf_Vorsaison"]) / 34
df["Zweikampf_Vorsaison"] = df["Zweikampf_Vorsaison"].round(0)
df["Fouls_Vorsaison"] = pd.to_numeric(df["Fouls_Vorsaison"])
df.rename(columns=lambda x: x.replace('_Vorsaison', ''), inplace=True)
df = df.dropna()


# merge df_train and df to identify train/test samples (as defined in R)
df_new = df.merge(df_train.drop_duplicates(), 
                  how='left', 
                  indicator=True)
df_new['in_train'] = (df_new['_merge'] == 'both').astype(int)
df_new = df_new.drop(columns=['_merge'])
df = df_new
df_test = df[df['in_train'] == 0].copy()
df_train = df[df['in_train'] == 1].copy()


# prepare for summary statistics table
def summary_statistics(df: pd.DataFrame, cols: list) -> pd.DataFrame:
    summary = pd.DataFrame({
        "Mittelwert": df[cols].mean(),
        "Std. Abw.": df[cols].std(),
        "Minimum": df[cols].min(),
        "10% Quantil": df[cols].quantile(0.1),
        "25% Quantil": df[cols].quantile(0.25),
        "Median": df[cols].median(),
        "75% Quantil": df[cols].quantile(0.75),
        "90% Quantil": df[cols].quantile(0.9),
        "Maximum": df[cols].max(),
        "Anzahl": df[cols].count()
    }) 
    always_two_decimals = ["Std. Abw."]
    two_decimal_vars = ["Trainerwechsel", "Titel", "CL_int"]

    for col in summary.columns:
        for var in summary.index:
            if col in always_two_decimals or var in two_decimal_vars:
                summary.at[var, col] = round(summary.at[var, col], 2)
            else:
                summary.at[var, col] = int(round(summary.at[var, col], 0))

    return summary


# compute summary statistics for all three datasets
cols = ["Ballbesitz", "Zweikampf", "Passquote", "Fouls", "Trainerwechsel", "Titel", "CL_int"]
summary_full = summary_statistics(df, cols)
summary_train = summary_statistics(df_train, cols)
summary_test = summary_statistics(df_test, cols)

rename_dict = {
    "index": "Feature",
}
html_full = summary_full.reset_index().rename(columns=rename_dict).to_html(classes='table table-striped', index=False)
html_train = summary_train.reset_index().rename(columns=rename_dict).to_html(classes='table table-striped', index=False)
html_test = summary_test.reset_index().rename(columns=rename_dict).to_html(classes='table table-striped', index=False)


# my html template 
html_content = f"""
<html>
<head>
<meta charset="UTF-8">
    <style>
        body {{
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
            margin: 40px;
            background-color: white;
            color: #333;
        }}
        h2 {{
            font-weight: 600;
            margin-bottom: 20px;
        }}
        .custom-select {{
            position: relative;
            width: 220px;
            user-select: none;
        }}
        select {{
            width: 100%;
            padding: 10px 15px;
            border: 2px solid #4a90e2;
            border-radius: 8px;
            background-color: white;
            font-size: 16px;
            font-weight: 600;
            color: #4a4a4a;
            appearance: none;
            cursor: pointer;
            transition: border-color 0.3s ease;
        }}
        select:hover, select:focus {{
            border-color: #357ABD;
            outline: none;
            box-shadow: 0 0 6px rgba(53, 122, 189, 0.5);
        }}
        .custom-select::after {{
            content: "▼";
            position: absolute;
            right: 15px;
            top: 50%;
            transform: translateY(-50%);
            pointer-events: none;
            color: #4a90e2;
            font-size: 12px;
            font-weight: 700;
        }}
        .table {{
            border-collapse: collapse;
            width: 100%;
            background-color: white;
            box-shadow: 0 2px 6px rgb(0 0 0 / 0.1);
            border-radius: 8px;
            overflow: hidden;
        }}
        .table th, .table td {{
            border: 1px solid #ddd;
            padding: 10px 12px;
            text-align: right;
            font-size: 14px;
        }}
        .table th {{
            background-color: #f2f2f2;
            font-weight: 600;
            text-align: left;
        }}
        .table td:first-child {{
            text-align: left;
            font-weight: 500;
            color: #222;
            width: 200px;
        }}
    </style>
    <script>
        function showTable() {{
            var selectBox = document.getElementById("dataset-select");
            var selectedValue = selectBox.value;
            document.getElementById("full").style.display = "none";
            document.getElementById("train").style.display = "none";
            document.getElementById("test").style.display = "none";
            document.getElementById(selectedValue).style.display = "block";
        }}
        window.onload = function() {{
            showTable();
        }};
    </script>
</head>
<body>
    <h2></h2>
    <div class="custom-select">
        <select id="dataset-select" onchange="showTable()">
            <option value="full">Gesamt Datensatz</option>
            <option value="train">Trainings Datensatz</option>
            <option value="test">Test Datensatz</option>
        </select>
    </div>
    <br><br>
    <div id="full">{html_full}</div>
    <div id="train" style="display:none;">{html_train}</div>
    <div id="test" style="display:none;">{html_test}</div>
</body>
</html>
"""

# export html page
with open("table.html", "w", encoding="utf-8") as f:
    f.write(html_content)
