
# In[ ]
import panel
import holoviews as hv, datashader as ds
import dask.dataframe as dd
from holoviews.operation.datashader import rasterize

hv.extension('bokeh')

# In[ ]
path = '../../../datashader/examples/data/nyc_taxi_wide.parq'
df = dd.read_parquet(path).persist()

opts = dict(width=700, height=300, logz=True, tools=['hover'], colorbar=True)
points = hv.Points(df, ['dropoff_x', 'dropoff_y'], ['dropoff_hour','pickup_hour'])


# In[ ]
panel.panel(rasterize(points).options(**opts))
