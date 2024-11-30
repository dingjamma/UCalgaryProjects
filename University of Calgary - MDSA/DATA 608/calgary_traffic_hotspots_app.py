import streamlit as st
import pandas as pd
import folium
from folium.plugins import HeatMap
import plotly.express as px
from streamlit_folium import st_folium
import boto3
import time

# AWS settings
DATABASE = 'calgarytrafficincidents_project608'
TABLE = 'final_calgarytrafficincidents_clean_csv'
S3_OUTPUT = 's3://calgarytrafficincidents/'
REGION = 'us-east-1'

# Create Athena client
athena_client = boto3.client('athena', region_name=REGION)

# Function to run Athena query
def run_athena_query(query):
    response = athena_client.start_query_execution(
        QueryString=query,
        QueryExecutionContext={
            'Database': DATABASE
        },
        ResultConfiguration={
            'OutputLocation': S3_OUTPUT,
        }
    )
    return response

# Function to check query execution status
def check_query_status(query_execution_id):
    while True:
        response = athena_client.get_query_execution(QueryExecutionId=query_execution_id)
        status = response['QueryExecution']['Status']['State']
        if status in ['SUCCEEDED', 'FAILED', 'CANCELLED']:
            return status
        time.sleep(5)

# Function to get query results
def get_query_results(query_execution_id):
    paginator = athena_client.get_paginator('get_query_results')
    results = paginator.paginate(QueryExecutionId=query_execution_id)
    columns = None
    rows = []

    for result in results:
        if columns is None:
            columns = [col['Label'] for col in result['ResultSet']['ResultSetMetadata']['ColumnInfo']]

        for row in result['ResultSet']['Rows'][1:]:
            rows.append([data.get('VarCharValue', data.get('DoubleValue', None)).replace('"', '') if isinstance(data.get('VarCharValue', None), str) else data.get('VarCharValue', data.get('DoubleValue', Non>

    return pd.DataFrame(rows, columns=columns)

# Function to load data from Athena
@st.cache_data
def load_data():
    query = f'SELECT * FROM "{DATABASE}"."{TABLE}";'
    response = run_athena_query(query)
    query_execution_id = response['QueryExecutionId']
    status = check_query_status(query_execution_id)
    if status == 'SUCCEEDED':
        df = get_query_results(query_execution_id)
        return df
    else:
        st.error("Query failed")
        return pd.DataFrame()

# Load data
df = load_data()

# Convert start_dt to datetime
df['start_dt'] = pd.to_datetime(df['start_dt'], errors='coerce')


# Drop rows with null datetime values
df = df.dropna(subset=['start_dt'])

# Extract year and month as integers
df['Year'] = df['start_dt'].dt.year.astype(int)
df['Month'] = df['start_dt'].dt.month.astype(int)

# Streamlit app
st.title("Calgary Traffic Incidents")

# Filters
st.sidebar.header("Filters")

selected_year = st.sidebar.selectbox("Select Year", sorted(df['Year'].unique(), reverse=True))
selected_month = st.sidebar.selectbox("Select Month", sorted(df['Month'].unique(), reverse=True))

# Convert selected values to integers
selected_year = int(selected_year)
selected_month = int(selected_month)

# Filter data based on selections
filtered_df = df[(df['Year'] == selected_year) & (df['Month'] == selected_month)]

st.header(f"Data Overview for {selected_year}-{selected_month:02d}")
st.write(filtered_df.head())

# Line graph
st.header("Incidents Over Time")
filtered_df['Date'] = filtered_df['start_dt'].dt.date
daily_counts = filtered_df.groupby('Date').size().reset_index(name='Count')
fig = px.line(daily_counts, x='Date', y='Count', title='Daily Incident Counts')
st.plotly_chart(fig)

# Heatmap
st.header("Incident Heatmap")


# Calculate map center
map_center = [filtered_df['latitude'].astype(float).mean(), filtered_df['longitude'].astype(float).mean()]

m = folium.Map(location=map_center, zoom_start=12)
heat_data = [[row['latitude'], row['longitude']] for index, row in filtered_df.iterrows()]
HeatMap(heat_data).add_to(m)

st_folium(m, width=700, height=500)
