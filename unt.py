from sqlalchemy import create_engine
engine = create_engine('sqlite:///data/elections2016.sqlite')

import pandas as pd

# for downloading the entire table
#pd.read_sql('IndexMeta', engine)

# Cargamos en un DataFrame el resultado de una query SQL
an_entire_table = pd.read_sql('SELECT * FROM elections2016', engine)

an_entire_table.head()

# Exploración de tablas
from sqlalchemy import inspect
inspector = inspect(engine)
print(inspector.get_table_names())

inspector.get_columns('IndexPrice')

# Para limitar el número de filas
limited_prices = pd.read_sql('SELECT * FROM IndexPrice LIMIT 10', engine)
print(limited_prices)

limited_prices.close

# Para queries más complejas:
# 
# - The evolution of the indexes from United States and Europe. 
#   This region information is available in the `IndexMeta` table, 
#   so we'll need a `JOIN` sentence. 
# - We will download the price at the close and the volume, daily. 
# - From 2019 and so forth. 
# - We are also downloading the currency.


query = """
  SELECT IndexMeta.region, IndexPrice.stock_index, 
         IndexPrice.date, 
         IndexPrice.adj_close, IndexPrice.volume, 
         IndexMeta.currency
  FROM IndexPrice INNER JOIN IndexMeta
      ON IndexPrice.stock_index = IndexMeta.stock_index
  WHERE IndexMeta.region in ('United States', 'Europe') and 
      IndexPrice.date >= '2019-01-01'
"""

df_usa_eu_prices = pd.read_sql(query, engine)
df_usa_eu_prices.head()


df_usa_eu_prices.describe()

