# -*- coding: utf-8 -*-

"""
Dual-Power Model Visualization

This module provides an interactive web application for visualizing
the Dual-Power Model of subjective value and effort. It allows users
to manipulate model parameters and observe their effects on the valuation
curve, as well as view predefined preference profiles from published papers.
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "MIT"

import numpy as np
import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go

# Initialize app
app = dash.Dash(__name__)
server = app.server

# Modify index string to include custom CSS
app.index_string = '''
<!DOCTYPE html>
<html>
    <head>
        {%metas%}
        <title>{%title%}</title>
        {%favicon%}
        {%css%}
        <style>
            /* Hide spinner arrows in input[type=number] */
            input[type=number]::-webkit-outer-spin-button,
            input[type=number]::-webkit-inner-spin-button {
                -webkit-appearance: none;
                margin: 0;
            }
            input[type=number] {
                -moz-appearance: textfield;
            }
        </style>
    </head>
    <body>
        {%app_entry%}
        <footer>
            {%config%}
            {%scripts%}
            {%renderer%}
        </footer>
    </body>
</html>
'''

# Define paper parameter configurations
PAPER_CONFIGS = {
    'Increasing': {
        'δ₁': 12.595,
        'γ₁': 0.6043103,
        'δ₂': 0.4693622,
        'γ₂': 0.469386,
        'ω': 0.1462272
    },
    'Decreasing-Increasing': {
        'δ₁': 9.522435,
        'γ₁': 10.58172,
        'δ₂': 1.901854,
        'γ₂': 1.491141,
        'ω': 0.4928382
    },
    'Decreasing': {
        'δ₁': 1e-07,
        'γ₁': 9.582383e-07,
        'δ₂': 4.55966,
        'γ₂': 3.306229,
        'ω': 1e-07
    },
    'Increasing-Decreasing': {
        'δ₁': 3.492561,
        'γ₁': 2.43447,
        'δ₂': 3.258896,
        'γ₂': 25.08094,
        'ω': 0.382991
    }
}

# Set default parameter values
default_params = {
    'ω': 0.5,
    'δ₁': 0,
    'γ₁': 1,
    'δ₂': 0,
    'γ₂': 1
}

# Define parameter bounds
param_bounds = {
    'ω': {'min': 0, 'max': 1},
    'δ₁': {'min': 0, 'max': 100},
    'γ₁': {'min': 0, 'max': 100},
    'δ₂': {'min': 0, 'max': 100},
    'γ₂': {'min': 0, 'max': 100}
}


def create_parameter_input(param_name, param_label, min_value, max_value,
                           default_value, step):
    """Create a parameter input control with label and description,
    including increment/decrement buttons."""
    descriptions = {
        'ω': 'System weight',
        'δ₁': 'Positive steepness',
        'γ₁': 'Positive curvature',
        'δ₂': 'Negative steepness',
        'γ₂': 'Negative curvature'
    }

    control = html.Div(
        style={'marginBottom': '20px'},
        children=[
            html.Label(
                [
                    param_label,
                    html.Span(
                        f" {descriptions[param_label]}",
                        style={
                            'fontSize': '14px',
                            'fontWeight': 'normal',
                            'color': '#666'
                        }
                    )
                ],
                style={
                    'fontSize': '18px',
                    'fontWeight': 'bold',
                    'display': 'block',
                    'marginBottom': '5px'
                }
            ),
            html.Div(
                style={'display': 'flex', 'alignItems': 'center'},
                children=[
                    html.Button(
                        '-',
                        id=f'{param_name}-decrement',
                        n_clicks=0,
                        style={'marginRight': '5px'}
                    ),
                    dcc.Input(
                        id=f'{param_name}-input',
                        type='number',
                        min=min_value,
                        max=max_value,
                        step='any',
                        value=default_value,
                        style={
                            'width': '80px',
                            'textAlign': 'center'
                            # Hide spinner arrows via CSS in app.index_string
                        },
                        debounce=True,
                        persistence=True,
                        required=False
                    ),
                    html.Button(
                        '+',
                        id=f'{param_name}-increment',
                        n_clicks=0,
                        style={'marginLeft': '5px'}
                    ),
                ]
            )
        ]
    )
    return control


# Generate parameter controls
parameter_controls = []
for param in ['ω', 'δ₁', 'γ₁', 'δ₂', 'γ₂']:
    min_value = param_bounds[param]['min']
    max_value = param_bounds[param]['max']
    default_value = default_params[param]
    step = 0.1
    control = create_parameter_input(
        param_name=param,
        param_label=param,
        min_value=min_value,
        max_value=max_value,
        default_value=default_value,
        step=step
    )
    parameter_controls.append(control)

# Define layout
app.layout = html.Div(
    style={
        'fontFamily': 'Segoe UI',
        'maxWidth': '1300px',
        'margin': '0 auto',
        'padding': '20px'
    },
    children=[
        html.Div(
            style={
                'position': 'relative',
                'marginBottom': '30px'
            },
            children=[
                html.H2(
                    'Dual-Power Model Visualization',
                    style={
                        'textAlign': 'center',
                        'marginBottom': '0'
                    }
                ),
                # Info Icon
                html.Button(
                    'i',
                    id='info-button',
                    n_clicks=0,
                    style={
                        'position': 'absolute',
                        'right': '0',
                        'top': '0',
                        'backgroundColor': 'white',
                        'border': '2px solid black',
                        'borderRadius': '50%',
                        'fontSize': '16px',
                        'fontWeight': 'bold',
                        'width': '30px',
                        'height': '30px',
                        'textAlign': 'center',
                        'padding': '0',
                        'cursor': 'pointer',
                        'fontFamily': 'Segoe UI',
                        'color': 'black',
                        'lineHeight': '26px'
                    }
                ),
            ]
        ),
        # Info Modal
        html.Div(
            id='info-modal',
            style={
                'display': 'none',
                'position': 'fixed',
                'zIndex': 1,
                'left': 0,
                'top': 0,
                'width': '100%',
                'height': '100%',
                'overflow': 'auto',
                'backgroundColor': 'rgba(0,0,0,0.4)'
            },
            children=[
                html.Div(
                    style={
                        'backgroundColor': '#fefefe',
                        'margin': '15% auto',
                        'padding': '20px',
                        'border': '1px solid #888',
                        'width': '80%',
                        'maxWidth': '500px',
                        'position': 'relative'
                    },
                    children=[
                        html.Button(
                            '×',
                            id='close-info-modal',
                            n_clicks=0,
                            style={
                                'position': 'absolute',
                                'top': '10px',
                                'right': '15px',
                                'background': 'none',
                                'border': 'none',
                                'fontSize': '28px',
                                'fontWeight': 'bold',
                                'color': '#aaa',
                                'cursor': 'pointer'
                            }
                        ),
                        html.H2('Model Visualizer'),
                        html.P(
                            'This application visualizes the Dual-Power Model of '
                            'subjective value and effort. Adjust the parameters to see '
                            'how they affect the valuation curve. You can also view '
                            'predefined preference profiles from published papers.'
                        )
                    ]
                )
            ]
        ),
        dcc.Store(id='toggle-state', data=False),
        html.Div(
            style={
                'display': 'flex',
                'flexDirection': 'row',
                'justifyContent': 'center',
                'gap': '40px'
            },
            children=[
                # Left: Plot
                html.Div(
                    style={'width': '600px', 'flexShrink': 0},
                    children=[
                        dcc.Graph(
                            id='sv-plot',
                            config={'displayModeBar': False},
                            style={'width': '600px', 'height': '600px'}
                        )
                    ]
                ),
                # Middle: Equation and System Values Display
                html.Div(
                    style={
                        'padding': '0 20px',
                        'width': '400px',
                        'flexShrink': 0,
                        'borderLeft': '1px solid #eee',
                        'borderRight': '1px solid #eee',
                    },
                    children=[
                        html.H4('Equation:'),
                        html.P(
                            "SV = X·[1 + (ω·(δ₁·E^γ₁) - (1-ω)·(δ₂·E^γ₂))]",
                            style={
                                'fontSize': '16px',
                                'whiteSpace': 'nowrap'
                            }
                        ),
                        html.Div(
                            id='current-params',
                            style={
                                'fontSize': '16px',
                                'marginBottom': '20px',
                                'whiteSpace': 'nowrap'
                            }
                        ),
                        html.Div(
                            id='system-values',
                            style={'fontSize': '16px'}
                        ),
                        html.Div(
                            id='paper-config-display',
                            style={'fontSize': '14px'}
                        )
                    ]
                ),
                # Right: Parameter Controls
                html.Div(
                    style={'width': '220px', 'flexShrink': 0},
                    children=[
                        html.H4('Parameters'),
                        html.Div(
                            style={'width': '100%'},
                            children=parameter_controls
                        ),
                        html.Button(
                            id='toggle-button',
                            n_clicks=0,
                            style={
                                'marginBottom': '20px',
                                'width': '100%'
                            }
                        )
                    ]
                )
            ]
        )
    ]
)


@app.callback(
    Output('toggle-button', 'children'),
    Input('toggle-state', 'data')
)
def update_button_text(toggle_state):
    """Update the toggle button text based on state."""
    return 'Show Custom Values' if toggle_state else 'Show Paper Results'


@app.callback(
    Output('toggle-state', 'data'),
    Input('toggle-button', 'n_clicks'),
    State('toggle-state', 'data')
)
def toggle_paper_configs(n_clicks, toggle_state):
    """Toggle between paper configurations and custom values."""
    if n_clicks is None:
        return toggle_state
    return not toggle_state


@app.callback(
    Output('sv-plot', 'figure'),
    Output('current-params', 'children'),
    Output('system-values', 'children'),
    Output('paper-config-display', 'children'),
    Input('ω-input', 'value'),
    Input('δ₁-input', 'value'),
    Input('γ₁-input', 'value'),
    Input('δ₂-input', 'value'),
    Input('γ₂-input', 'value'),
    Input('toggle-state', 'data')
)
def update_plot_and_values(omega, delta1, gamma1, delta2, gamma2, toggle_state):
    """Update the plot and values based on parameter inputs."""
    # Handle None values
    omega = omega if omega is not None else default_params['ω']
    delta1 = delta1 if delta1 is not None else default_params['δ₁']
    gamma1 = gamma1 if gamma1 is not None else default_params['γ₁']
    delta2 = delta2 if delta2 is not None else default_params['δ₂']
    gamma2 = gamma2 if gamma2 is not None else default_params['γ₂']

    # Convert to float
    omega = float(omega)
    delta1 = float(delta1)
    gamma1 = float(gamma1)
    delta2 = float(delta2)
    gamma2 = float(gamma2)

    E = np.linspace(0, 1, 100)
    X = 1

    # Calculate Subjective Value
    SV = X * (1 + (omega * (delta1 * E ** gamma1) -
               (1 - omega) * (delta2 * E ** gamma2)))

    # Create plot
    traces = []

    if not toggle_state:
        # Show single black line for current parameters
        traces.append(
            go.Scatter(
                x=E,
                y=SV,
                mode='lines',
                name='Current Parameters',
                line=dict(color='black')
            )
        )
        # Display current parameters horizontally
        current_params = (
            f"X=1, ω={omega:.3f}, δ₁={delta1:.3f}, γ₁={gamma1:.3f}, "
            f"δ₂={delta2:.3f}, γ₂={gamma2:.3f}"
        )
        paper_config_display = ''
    else:
        # Show all paper configurations
        colors = ['red', 'green', 'blue', 'purple']
        paper_params_list = [
            html.H4(
                'Preference Profiles:',
                style={'marginTop': '0px', 'marginBottom': '10px'}
            )
        ]

        for idx, (config_name, params) in enumerate(PAPER_CONFIGS.items()):
            delta1_paper = params['δ₁']
            gamma1_paper = params['γ₁']
            delta2_paper = params['δ₂']
            gamma2_paper = params['γ₂']
            omega_paper = params['ω']

            SV_paper = X * (1 + (
                omega_paper * (delta1_paper * E ** gamma1_paper) -
                (1 - omega_paper) * (delta2_paper * E ** gamma2_paper)
            ))

            traces.append(
                go.Scatter(
                    x=E,
                    y=SV_paper,
                    mode='lines',
                    name=config_name,
                    line=dict(color=colors[idx % len(colors)])
                )
            )

            # Format parameter values for display
            paper_params_list.append(
                html.Div([
                    html.H5(
                        config_name,
                        style={
                            'marginBottom': '5px',
                            'marginTop': '15px',
                            'color': colors[idx % len(colors)]
                        }
                    ),
                    html.P(
                        f"X=1, ω={omega_paper:.3f}, δ₁={delta1_paper:.3f}, "
                        f"γ₁={gamma1_paper:.3f}, δ₂={delta2_paper:.3f}, "
                        f"γ₂={gamma2_paper:.3f}",
                        style={'whiteSpace': 'nowrap'}
                    )
                ])
            )

        current_params = ''
        paper_config_display = paper_params_list

    # Create figure
    figure = {
        'data': traces,
        'layout': go.Layout(
            xaxis={'title': 'Effort', 'range': [0, 1]},
            yaxis={'title': 'Subjective Value'},
            showlegend=False,
            width=600,
            height=600,
            hovermode='closest'
        )
    }

    # System values
    if not toggle_state:
        E1 = 0.5
        positive_system = omega * (delta1 * E1 ** gamma1)
        negative_system = -(1 - omega) * (delta2 * E1 ** gamma2)

        system_values = html.Div([
            html.H4('System Values at E=0.5:'),
            html.P([
                "Positive System (ω·(δ₁·E^γ₁)): ",
                html.Span(
                    f"+{positive_system:.3f}",
                    style={'fontWeight': 'bold', 'color': 'green'}
                )
            ]),
            html.P([
                "Negative System (-(1-ω)·(δ₂·E^γ₂)): ",
                html.Span(
                    f"{negative_system:.3f}",
                    style={'fontWeight': 'bold', 'color': 'red'}
                )
            ])
        ])
    else:
        system_values = ''

    return figure, current_params, system_values, paper_config_display


# Callbacks for increment and decrement buttons
increment = 0.1  # Increment/decrement value


def create_increment_decrement_callback(param_name):
    @app.callback(
        Output(f'{param_name}-input', 'value'),
        Input(f'{param_name}-increment', 'n_clicks'),
        Input(f'{param_name}-decrement', 'n_clicks'),
        State(f'{param_name}-input', 'value'),
        prevent_initial_call=True
    )
    def update_param(increment_clicks, decrement_clicks, current_value):
        ctx = dash.callback_context
        if not ctx.triggered or current_value is None:
            return current_value

        button_id = ctx.triggered[0]['prop_id'].split('.')[0]
        new_value = current_value
        if button_id == f'{param_name}-increment':
            new_value += increment
        elif button_id == f'{param_name}-decrement':
            new_value -= increment

        # Enforce min/max bounds
        min_value = param_bounds[param_name]['min']
        max_value = param_bounds[param_name]['max']
        new_value = max(min_value, min(new_value, max_value))

        # Round new_value to 3 decimal places
        new_value = round(new_value, 3)

        return new_value


# Create callbacks for each parameter
for param in ['ω', 'δ₁', 'γ₁', 'δ₂', 'γ₂']:
    create_increment_decrement_callback(param)


# Callback for Info Modal
@app.callback(
    Output('info-modal', 'style'),
    [Input('info-button', 'n_clicks'), Input('close-info-modal', 'n_clicks')],
    [State('info-modal', 'style')]
)
def toggle_info_modal(open_clicks, close_clicks, current_style):
    if current_style is None:
        current_style = {'display': 'none'}
    ctx = dash.callback_context
    if not ctx.triggered:
        return current_style
    button_id = ctx.triggered[0]['prop_id'].split('.')[0]
    if button_id == 'info-button':
        return {
            'display': 'block',
            'position': 'fixed',
            'zIndex': 1,
            'left': 0,
            'top': 0,
            'width': '100%',
            'height': '100%',
            'overflow': 'auto',
            'backgroundColor': 'rgba(0,0,0,0.4)'
        }
    elif button_id == 'close-info-modal':
        return {'display': 'none'}
    return current_style


if __name__ == '__main__':
    app.run_server(debug=True)