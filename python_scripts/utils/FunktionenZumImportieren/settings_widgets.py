'''
This script is responsible for providing the widget interaction capabilities of the main pipeline (where you can select different settings).
'''

import ipywidgets as widgets

class WidgetValues:
    def __init__(self, annotation_algorithm, rank, InsectDetect, ApolloNet, BioClip, save_visualisation, start_image):
        self.annotation_algorithm = annotation_algorithm
        self.rank = rank
        self.InsectDetect = InsectDetect
        self.ApolloNet = ApolloNet
        self.BioClip = BioClip
        self.save_visualisation = save_visualisation
        self.start_image = start_image

def create_interactive_widgets():
    style = {'description_width': 'initial'}

    # Create widgets
    annotation_algorithm = widgets.Dropdown(
        options=[('BioClip', 'BioClip'), ('ApolloNet', 'ApolloNet'), ('InsectDetect', 'InsectDetect')],
        description='Annotation algorithm:',
        value='BioClip',
        ensure_option=True,
        disabled=False,
        style=style,
        layout=widgets.Layout(width='50%', height='30px')
    )

    rank = widgets.Dropdown(
        options=[('kingdom', 'kingdom'), ('phylum', 'phylum'), ('class', 'class'), ('order', 'order'), ('family', 'family'), ('genus', 'genus'), ('species', 'species'), (None, None)], 
        description='Taxonomic rank:',
        value='family',
        ensure_option=True,
        disabled=False,
        style=style,
        layout=widgets.Layout(width='50%', height='30px')
    )

    InsectDetect = widgets.Checkbox(
        value=False,
        description='Perform image classification with InsectDetect',
        disabled=False,
        indent=False
    )

    ApolloNet = widgets.Checkbox(
        value=False,
        description='Perform image classification with ApolloNet',
        disabled=False,
        indent=False
    )

    BioClip = widgets.Checkbox(
        value=True,
        description='Perform image classification with BioClip',
        disabled=False,
        indent=False
    )

    save_visualisation = widgets.Checkbox(
        value=False,
        description='Save visualisations',
        disabled=False,
        indent=False
    )

    start_image = widgets.Text(
        value='',
        placeholder='Image Name',
        description='If you want to start at a specific image:',
        disabled=False,
        style=style,
        layout=widgets.Layout(width='50%', height='30px')
    )

    # Function to update the dropdown options
    def update_dropdown_options(change):
        options = []
        if BioClip.value:
            options.append(('BioClip', 'BioClip'))
        if ApolloNet.value:
            options.append(('ApolloNet', 'ApolloNet'))
        if InsectDetect.value:
            options.append(('InsectDetect', 'InsectDetect'))
        annotation_algorithm.options = options
        if annotation_algorithm.value not in [opt[0] for opt in options]:
            annotation_algorithm.value = options[0][0] if options else None

    # Function to update the rank dropdown visibility
    def update_rank_visibility(change):
        rank.disabled = not BioClip.value

    # Observe changes in checkboxes
    BioClip.observe(update_dropdown_options, names='value')
    ApolloNet.observe(update_dropdown_options, names='value')
    InsectDetect.observe(update_dropdown_options, names='value')
    BioClip.observe(update_rank_visibility, names='value')

    # Initial call to set the dropdown options
    update_dropdown_options(None)


    # Display widgets
    display(annotation_algorithm)
    display(BioClip)
    display(rank if BioClip.value else None)
    display(ApolloNet)
    display(InsectDetect)
    display(save_visualisation)
    display(start_image)
    # Function to get all widget values
    def get_widget_values():
        return WidgetValues(
            annotation_algorithm=annotation_algorithm.value,
            rank=rank.value,
            InsectDetect=InsectDetect.value,
            ApolloNet=ApolloNet.value,
            BioClip=BioClip.value,
            save_visualisation=save_visualisation.value,
            start_image=start_image.value
        )

    return get_widget_values