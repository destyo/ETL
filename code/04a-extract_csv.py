import pandas as pd

df_hipotecas = pd.read_csv('~/data/hipotecas/hipotecas_lectura')
df_hipotecas.head()

cols_usar = ['period', 'total_nacional']

df_hipotecas = pd.read_csv('../data/hipotecas/hipotecas_lectura', usecols=cols_usar)
df_hipotecas.head()

df_hipotecas = pd.read_csv('../data/hipotecas/hipotecas_lectura',
                           skiprows=7, 
                           nrows=6, 
                           header=None)
df_hipotecas.head()
