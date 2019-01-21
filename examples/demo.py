# -*- mode: python; nei-env : "analytics"; eval: (nei-mode)-*-

"Code before the first cell is ignored."

# In[ ]
print("This is an example of code execution: %s is %d" % ("1+1", 1+1))


"""
### Markdown is supported and live!!

* Lists are live
* **bold**
* *italic*
"""

# In[ ]
import holoviews as hv
import numpy as np
hv.extension('bokeh')


# In[ ]
xs = np.linspace(1,10,100)
hv.Curve((xs, np.sin(xs)))
