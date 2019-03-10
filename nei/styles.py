import pkgutil
try:
    import cssutils
except:
    cssutils = None

def parse_CSS(css):
    """
    Given some CSS, parse it and return a dictionary of dictionaries:

    { selector : {property: value}}
    """
    sheet = cssutils.parseString(css)
    css_dict={}
    for rule in sheet:
        selector = rule.selectorText
        styles = rule.style.cssText
        prop_dict = {}
        for prop in rule.style:
            prop_dict[prop.name] = prop.value
        css_dict[selector] = prop_dict
    return css_dict



def css_processor(code_generator):
    """
    Create a CSS processor based on the given code generator.
    """
    def inner(notebook, connection, css):
        if cssutils is None:
            print('WARNING: cssutil is not available to handle CSS')
            return
        code = code_generator(parse_CSS(css))
        if code is not None:
            notebook.exec_silently(connection, code)
    return inner



def apply_holoviews_theme(parsed_css):
    """
    A code generator used to set the holoviews theme from the parsed CSS.
    """
    hvavailable = pkgutil.find_loader('holoviews')
    if hvavailable is None: return
    if parsed_css == {}:
        return ''

    lines = ["import holoviews as hv",
             "from holoviews.plotting import bokeh",
             "from bokeh.themes.theme import Theme",
             "hv.renderer('bokeh').theme = Theme(json={json})"]

    bgcolor = parsed_css['body']['background-color']
    bgcolor = bgcolor if bgcolor != '#fff' else  '#ffffff'
    opposed = 'white' if bgcolor != '#ffffff' else 'black'
    theme = {
            'attrs' : {
                'Figure' : {
                    'background_fill_color': bgcolor,
                    'border_fill_color':     bgcolor,
                    'outline_line_color': '#444444',
                },
                'Grid': {
                    'grid_line_dash': [6, 4],
                    'grid_line_alpha': .3,
                },
                'Title': {
                    'text_color': opposed
                },
                'Axis': {
                    'axis_label_text_color': opposed,
                    'axis_line_color' : opposed,
                    'minor_tick_line_color' : opposed,
                    'major_label_text_color' : opposed,
                    'major_tick_line_color': opposed
                },
                'ColorBar': {
                    'background_fill_color' : bgcolor,
                    'title_text_color': opposed,
                    'major_label_text_color': opposed
                    }
            }
        }

    template = "\n".join(lines)
    return template.format(json=repr(theme))



process_css = css_processor(apply_holoviews_theme)
