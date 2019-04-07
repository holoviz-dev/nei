
# In[ ]
import holoviews as hv, datashader as ds
from holoviews import opts
import dask.dataframe as dd
from holoviews.operation.datashader import rasterize

hv.extension('bokeh')

# In[ ]
path = '../../datashader/examples/data/nyc_taxi_wide.parq'
df = dd.read_parquet(path).persist()

points = hv.Points(df, ['dropoff_x', 'dropoff_y'], ['dropoff_hour','pickup_hour'])


# In[ ]
options = opts.Image( data_aspect=1, responsive=True, logz=True,
                      tools=['hover'], colorbar=True)
rasterize(points).options(options)


# In[ ]
import panel
panel.panel(rasterize(points).options(**opts))
