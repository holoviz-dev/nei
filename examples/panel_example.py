
# In[ ]
import panel as pn
import pandas as pd
import holoviews as hv
from PIL import Image
hv.extension('bokeh')

# In[ ]
stocks = {name:pd.read_csv('data/{name}.csv'.format(name=name),
                           index_col='Date', parse_dates=True)
          for name in ['aapl', 'goog']}

def stock_price(stock):
    chosen = stocks[stock]
    return (hv.Curve((chosen['Open'].index, chosen['Open']))
            * hv.Curve((chosen['Close'].index, chosen['Close']))
            ).options(width=700,   height=400, legend_position='top_left')

# In[ ]
stock_logos = {'goog':Image.open('./data/google_logo.png'),
               'aapl':Image.open('./data/apple_logo.png')}

def stock_price_with_logo(stock):
    return pn.Row(pn.panel(stock_logos[stock], height=200), stock_price(stock))

stock_explorer = pn.interact(stock_price_with_logo, stock=['goog', 'aapl'])
stock_explorer

title = """<span style="font-weight:bold;font-size:x-large">Stock price dashboard</span>"""
pn.Column(pn.panel(title, height=10), stock_explorer)

dashboard =  pn.Column(pn.Row(pn.Spacer(width=350),
                 pn.panel(title, height=10)), stock_explorer)
dashboard
