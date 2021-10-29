from sqlalchemy import create_engine
engine = create_engine('sqlite:///../data/indexKaggle.sqlite')

import pandas as pd

query = """
  SELECT IndexMeta.region, IndexPrice.stock_index, 
         IndexPrice.date, 
         IndexPrice.adj_close, IndexPrice.volume, 
         IndexMeta.currency
  FROM IndexPrice INNER JOIN IndexMeta
      ON IndexPrice.stock_index = IndexMeta.stock_index
  WHERE IndexMeta.region in ('United States', 'Europe') and 
      IndexPrice.date >= '2019-01-01' and
      IndexPrice.adj_close is not null
"""

df_usa_eu_prices = pd.read_sql(query, engine)
# df_usa_eu_prices.set_index('date')
df_usa_eu_prices.head()

# numeric
df_usa_eu_prices['adj_close'] = pd.to_numeric(df_usa_eu_prices['adj_close'], errors='coerce')

# df_usa_eu_prices['adj_close'][1466]
df_usa_eu_prices['adj_close'] = df_usa_eu_prices['adj_close'].fillna(
  df_usa_eu_prices.groupby('stock_index')['adj_close'].transform('mean')
)

df_usa_eu_prices['adj_close'].isna().sum()

# What we have to do is creating three different tables, one for each index, 
# with three columns: 
# - The date. 
# - The adjusted close price. 
# - A smoothed close price series.

indexes = df_usa_eu_prices['stock_index'].unique()
indexes
new_tables = {}

for idx in indexes:
  df_filtered = df_usa_eu_prices.loc[
    df_usa_eu_prices['stock_index'] == idx, ['date', 'adj_close']
  ]
  df_filtered['smoothed'] = df_filtered['adj_close'].rolling(15).mean()
  
  new_tables[idx] = df_filtered

import matplotlib.pyplot as plt

fig, axes = plt.subplots(nrows=1, ncols=3)
list(new_tables.keys())
for i in range(len(indexes)):
  df_to_plot = new_tables[indexes[i]]
  df_to_plot = df_to_plot.set_index('date')
  plot_number = i+1
  df_to_plot.loc[:,['adj_close', 'smoothed']].plot(ax=axes[i])
  # plt.title(indexes[i])
  
plt.show()


from sqlalchemy import Table, Column, Float, Date, MetaData
meta = MetaData()

for idx in indexes:
  students = Table(
    idx, meta, 
    Column('date', Date, primary_key = True), 
    Column('adj_close', Float), 
    Column('smoothed', Float), 
  )
  
meta.create_all(engine)
engine.table_names()

for idx in indexes:
  new_tables[idx].to_sql(idx, engine, if_exists='replace')

query = """
  SELECT *
  FROM NYA
"""
df_check = pd.read_sql(query, engine)
df_check.tail()

for idx in indexes:
  result = engine.execute('DROP TABLE IF EXISTS '+idx+';')
engine.table_names()
