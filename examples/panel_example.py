
# In[ ]
import panel
import holoviews as hv
hv.extension('bokeh')


# In[ ]
panel.panel(hv.Curve([1,2,3]))


# In[ ]
p = panel.panel('Example Text')
p

# In[ ]
c = panel.Column(p)
c

# In[ ]
c.append(panel.panel('Appended'))
c.append(panel.panel('Also Appended'))
