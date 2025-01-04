# -*- coding: utf-8 -*-

"""
Dual-Power Model Visualization

This module provides an interactive web application for visualizing
the Dual-Power Model of subjective value and effort. It allows users
to manipulate model parameters and observe their effects on the valuation
curve, as well as view predefined preference profiles from publications.
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "GPL 3.0"

import numpy as np
import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go

# Initialize app
app = dash.Dash(__name__, title="Model Visualizer")
server = app.server

# Modify index string to include custom CSS
app.index_string = """
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
"""

# Figure 1 parameter presets
FIGURE1_PRESETS = {
    "Decreasing": {"δ₁": 0, "δ₂": 2, "γ₁": 0, "γ₂": 2, "ω": 0},
    "Increasing": {"δ₁": 2, "δ₂": 0, "γ₁": 0.5, "γ₂": 0, "ω": 1},
    "Decreasing-Increasing": {"δ₁": 3, "δ₂": 3, "γ₁": 15, "γ₂": 3, "ω": 0.5},
    "Increasing-Decreasing": {"δ₁": 3.5, "δ₂": 3.5, "γ₁": 1, "γ₂": 3, "ω": 0.5},
}

# Figure 5 parameter presets
FIGURE5_PRESETS = {
    "Increasing": {
        "δ₁": 12.595,
        "γ₁": 0.4693622,
        "δ₂": 0.6043103,
        "γ₂": 0.469386,
        "ω": 0.1462272,
    },
    "Decreasing-Increasing": {
        "δ₁": 9.522435,
        "γ₁": 1.901854,
        "δ₂": 10.58172,
        "γ₂": 1.491141,
        "ω": 0.4928382,
    },
    "Decreasing": {
        "δ₁": 1e-07,
        "γ₁": 9.582383e-07,
        "δ₂": 4.55966,
        "γ₂": 3.306229,
        "ω": 1e-07,
    },
    "Increasing-Decreasing": {
        "δ₁": 3.492561,
        "γ₁": 3.258896,
        "δ₂": 2.43447,
        "γ₂": 25.08094,
        "ω": 0.382991,
    },
}

# Set default parameter values
default_params = {"ω": 0.5, "δ₁": 0, "γ₁": 1, "δ₂": 0, "γ₂": 1}

# Define parameter bounds
param_bounds = {
    "ω": {"min": 0, "max": 1},
    "δ₁": {"min": 0, "max": 100},
    "γ₁": {"min": 0, "max": 100},
    "δ₂": {"min": 0, "max": 100},
    "γ₂": {"min": 0, "max": 100},
}


def create_parameter_input(
    param_name, param_label, min_value, max_value, default_value, step
):
    """Create a parameter input control with label and description,
    including increment/decrement buttons and bounds labels"""
    descriptions = {
        "ω": "System weight",
        "δ₁": "Positive steepness",
        "γ₁": "Positive curvature",
        "δ₂": "Negative steepness",
        "γ₂": "Negative curvature",
    }

    control = html.Div(
        style={"marginBottom": "20px"},
        children=[
            html.Label(
                [
                    param_label,
                    html.Span(
                        f" {descriptions[param_label]}",
                        style={
                            "fontSize": "14px",
                            "fontWeight": "normal",
                            "color": "#666",
                        },
                    ),
                ],
                style={
                    "fontSize": "18px",
                    "fontWeight": "bold",
                    "display": "block",
                    "marginBottom": "5px",
                },
            ),
            html.Div(
                style={"display": "flex", "alignItems": "center"},
                children=[
                    html.Label(f"{min_value}", style={"marginRight": "5px"}),
                    html.Button(
                        "-",
                        id=f"{param_name}-decrement",
                        n_clicks=0,
                        style={"marginRight": "5px"},
                    ),
                    dcc.Input(
                        id=f"{param_name}-input",
                        type="number",
                        min=min_value,
                        max=max_value,
                        step=step,
                        value=default_value,
                        style={"width": "80px", "textAlign": "center"},
                        persistence=True,
                        required=False,
                    ),
                    html.Button(
                        "+",
                        id=f"{param_name}-increment",
                        n_clicks=0,
                        style={"marginLeft": "5px"},
                    ),
                    html.Label(f"{max_value}", style={"marginLeft": "5px"}),
                ],
            ),
        ],
    )
    return control


# Generate parameter controls
parameter_controls = []
for param in ["ω", "δ₁", "γ₁", "δ₂", "γ₂"]:
    min_value = param_bounds[param]["min"]
    max_value = param_bounds[param]["max"]
    default_value = default_params[param]
    step = 0.001
    control = create_parameter_input(
        param_name=param,
        param_label=param,
        min_value=min_value,
        max_value=max_value,
        default_value=default_value,
        step=step,
    )
    parameter_controls.append(control)

# Define layout
app.layout = html.Div(
    style={
        "fontFamily": "Segoe UI",
        "maxWidth": "1300px",
        "margin": "0 auto",
        "padding": "20px",
    },
    children=[
        html.Div(
            style={
                "position": "relative",
                "marginBottom": "30px",
                "display": "flex",
                "justifyContent": "center",
                "alignItems": "center",
                "gap": "10px",
            },
            children=[
                html.H2("Dual-Power Model Visualization", style={"margin": "0"}),
                html.Button(
                    "i",
                    id="info-button",
                    n_clicks=0,
                    style={
                        "backgroundColor": "white",
                        "border": "2px solid black",
                        "borderRadius": "50%",
                        "fontSize": "16px",
                        "fontWeight": "bold",
                        "width": "30px",
                        "height": "30px",
                        "textAlign": "center",
                        "padding": "0",
                        "cursor": "pointer",
                        "fontFamily": "Segoe UI",
                        "color": "black",
                        "lineHeight": "26px",
                    },
                ),
            ],
        ),
        html.Div(
            id="info-modal",
            style={
                "display": "none",
                "position": "fixed",
                "zIndex": 1,
                "left": 0,
                "top": 0,
                "width": "100%",
                "height": "100%",
                "overflow": "auto",
                "backgroundColor": "rgba(0,0,0,0.4)",
            },
            children=[
                html.Div(
                    style={
                        "backgroundColor": "#fefefe",
                        "margin": "15% auto",
                        "padding": "20px",
                        "border": "1px solid #888",
                        "width": "80%",
                        "maxWidth": "500px",
                        "position": "relative",
                    },
                    children=[
                        html.Button(
                            "×",
                            id="close-info-modal",
                            n_clicks=0,
                            style={
                                "position": "absolute",
                                "top": "10px",
                                "right": "15px",
                                "background": "none",
                                "border": "none",
                                "fontSize": "28px",
                                "fontWeight": "bold",
                                "color": "#aaa",
                                "cursor": "pointer",
                            },
                        ),
                        html.H2("Model Visualizer"),
                        html.P(
                            "This application visualizes the Dual-Power Model of "
                            "subjective value and effort. Adjust the parameters to see "
                            "how they affect the valuation curve. You can also view "
                            "predefined preference profiles from the manuscript."
                        ),
                    ],
                )
            ],
        ),
        dcc.Store(id="display-mode", data="custom"),
        html.Div(
            style={
                "display": "flex",
                "flexDirection": "row",
                "justifyContent": "center",
                "gap": "40px",
            },
            children=[
                # Left: Plot and Figure Description
                html.Div(
                    style={"width": "600px", "flexShrink": 0},
                    children=[
                        dcc.Graph(
                            id="sv-plot",
                            config={"displayModeBar": False},
                            style={"width": "600px", "height": "600px"},
                        ),
                        html.Div(
                            id="figure-description",
                            style={
                                "marginTop": "20px",
                                "fontSize": "14px",
                                "textAlign": "justify",
                            },
                        ),
                    ],
                ),
                # Middle: Equation and System Values Display
                html.Div(
                    style={
                        "padding": "0 20px",
                        "width": "400px",
                        "flexShrink": 0,
                        "borderLeft": "1px solid #eee",
                        "borderRight": "1px solid #eee",
                    },
                    children=[
                        html.H4("Equation:"),
                        html.P(
                            "sv(x) = x·[1 + (ω·(δ₁·E^γ₁) - (1-ω)·(δ₂·E^γ₂))]",
                            style={"fontSize": "16px", "whiteSpace": "nowrap"},
                        ),
                        html.Div(
                            id="current-params",
                            style={
                                "fontSize": "16px",
                                "marginBottom": "20px",
                                "whiteSpace": "nowrap",
                            },
                        ),
                        html.Div(id="system-values", style={"fontSize": "16px"}),
                        html.Div(id="paper-config-display", style={"fontSize": "14px"}),
                        html.Div(
                            style={
                                "marginTop": "40px",
                                "paddingTop": "20px",
                                "borderTop": "1px solid #eee",
                            },
                            children=[
                                html.H4("where:"),
                                html.Ul(
                                    style={
                                        "listStyleType": "none",
                                        "margin": "0",
                                        "paddingLeft": "40px",
                                    },
                                    children=[
                                        html.Li(
                                            "sv(x): subjective value relative to nominal value"
                                        ),
                                        html.Li("x: nominal value of outcome"),
                                        html.Li(
                                            "E: effort level as proportion of max effort"
                                        ),
                                        html.Li("ω: relative system weight"),
                                        html.Li("δ₁: steepness of positive system"),
                                        html.Li("γ₁: curvature of positive system"),
                                        html.Li("δ₂: steepness of negative system"),
                                        html.Li("γ₂: curvature of negative system"),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
                # Right: Parameter Controls
                html.Div(
                    style={"width": "220px", "flexShrink": 0},
                    children=[
                        html.H4("Parameters"),
                        html.Div(style={"width": "100%"}, children=parameter_controls),
                        html.Button(
                            "Show Figure 1 Values",
                            id="show-figure1-button",
                            n_clicks=0,
                            style={"marginBottom": "10px", "width": "100%"},
                        ),
                        html.Button(
                            "Show Figure 5 Values",
                            id="show-figure5-button",
                            n_clicks=0,
                            style={"marginBottom": "10px", "width": "100%"},
                        ),
                        html.Button(
                            "Show Custom Values",
                            id="show-custom-button",
                            n_clicks=0,
                            style={"marginBottom": "20px", "width": "100%"},
                        ),
                    ],
                ),
            ],
        ),
    ],
)


@app.callback(
    Output("display-mode", "data"),
    Input("show-figure1-button", "n_clicks"),
    Input("show-figure5-button", "n_clicks"),
    Input("show-custom-button", "n_clicks"),
)
def update_display_mode(n_clicks_fig1, n_clicks_fig5, n_clicks_custom):
    """Update display mode based on button clicks"""
    ctx = dash.callback_context
    if not ctx.triggered:
        return "custom"
    button_id = ctx.triggered[0]["prop_id"].split(".")[0]
    if button_id == "show-figure1-button":
        return "figure1"
    elif button_id == "show-figure5-button":
        return "figure5"
    elif button_id == "show-custom-button":
        return "custom"
    return "custom"


@app.callback(
    [
        Output("sv-plot", "figure"),
        Output("current-params", "children"),
        Output("system-values", "children"),
        Output("paper-config-display", "children"),
        Output("figure-description", "children"),
    ],
    [
        Input("ω-input", "value"),
        Input("δ₁-input", "value"),
        Input("γ₁-input", "value"),
        Input("δ₂-input", "value"),
        Input("γ₂-input", "value"),
        Input("display-mode", "data"),
    ],
)
def update_plot_and_values(
    omega_input, delta1_input, gamma1_input, delta2_input, gamma2_input, display_mode
):
    """Update the plot and values based on parameter inputs and display mode"""
    E = np.linspace(0, 1, 100)
    x = 1
    traces = []
    figure_description = ""

    if display_mode == "custom":
        # Handle None values
        omega = float(omega_input) if omega_input is not None else default_params["ω"]
        delta1 = (
            float(delta1_input) if delta1_input is not None else default_params["δ₁"]
        )
        gamma1 = (
            float(gamma1_input) if gamma1_input is not None else default_params["γ₁"]
        )
        delta2 = (
            float(delta2_input) if delta2_input is not None else default_params["δ₂"]
        )
        gamma2 = (
            float(gamma2_input) if gamma2_input is not None else default_params["γ₂"]
        )

        # Compute positive system
        positive_system = x * (1 + omega * (delta1 * E**gamma1))
        traces.append(
            go.Scatter(
                x=E,
                y=positive_system,
                mode="lines",
                name="Positive System",
                line=dict(color="green", width=2, dash="dash"),
                opacity=0.5,
            )
        )

        # Compute negative system
        negative_system = x * (1 - (1 - omega) * (delta2 * E**gamma2))
        traces.append(
            go.Scatter(
                x=E,
                y=negative_system,
                mode="lines",
                name="Negative System",
                line=dict(color="red", width=2, dash="dash"),
                opacity=0.5,
            )
        )

        # Compute combined sv(x)
        sv = x * (
            1 + (omega * (delta1 * E**gamma1) - (1 - omega) * (delta2 * E**gamma2))
        )
        traces.append(
            go.Scatter(
                x=E,
                y=sv,
                mode="lines",
                name="Combined System",
                line=dict(color="black", width=3),
            )
        )

        current_params = (
            f"x=1, ω={omega:.3f}, δ₁={delta1:.3f}, γ₁={gamma1:.3f}, "
            f"δ₂={delta2:.3f}, γ₂={gamma2:.3f}"
        )

        # Compute system values at E=0.5
        E_mid = 0.5
        positive_contribution = omega * (delta1 * E_mid**gamma1)
        negative_contribution = -(1 - omega) * (delta2 * E_mid**gamma2)
        net_effect = positive_contribution + negative_contribution

        system_values = html.Div(
            [
                html.H4("System Values at E=0.5:"),
                html.P(
                    [
                        "Weighted Positive System (ω·(δ₁·E^γ₁)): ",
                        html.Span(
                            f"{positive_contribution:+.3f}",
                            style={"fontWeight": "bold", "color": "green"},
                        ),
                    ]
                ),
                html.P(
                    [
                        "Weighted Negative System (-(1-ω)·(δ₂·E^γ₂)): ",
                        html.Span(
                            f"{negative_contribution:+.3f}",
                            style={"fontWeight": "bold", "color": "red"},
                        ),
                    ]
                ),
                html.P(
                    [
                        "Net System Effect: ",
                        html.Span(
                            f"{net_effect:.3f}",
                            style={"fontWeight": "bold"},
                        ),
                    ]
                ),
            ]
        )

        paper_config_display = ""

        figure_description = html.P(
            [
                html.Strong("Interactive Mode."),
                " Adjust the parameters using the controls on the right to explore \
              different value function shapes. The weighted positive system (green) \
              and negative system (red) values at E=0.5 are shown in the middle panel. \
              Parameters can be modified using the +/- buttons or by directly entering values. \
              The plot shows individual contributions of the positive (green) and negative (red) \
              systems, with their combined effect in black.",
            ]
        )

    elif display_mode == "figure1":
        # [Rest of the figure1 code remains the same]
        colors = ["red", "green", "blue", "purple"]
        labels = [
            "Decreasing",
            "Increasing",
            "Decreasing-Increasing",
            "Increasing-Decreasing",
        ]
        params_list = [html.H4("Figure 1 Presets:")]

        for idx, label in enumerate(labels):
            params = FIGURE1_PRESETS[label]
            delta1 = params["δ₁"]
            delta2 = params["δ₂"]
            gamma1 = params["γ₁"]
            gamma2 = params["γ₂"]
            omega = params["ω"]

            sv = x * (
                1 + (omega * (delta1 * E**gamma1) - (1 - omega) * (delta2 * E**gamma2))
            )

            traces.append(
                go.Scatter(
                    x=E,
                    y=sv,
                    mode="lines",
                    name=label,
                    line=dict(color=colors[idx % len(colors)]),
                )
            )

            params_list.append(
                html.Div(
                    [
                        html.H5(
                            f"{label} Profile",
                            style={
                                "marginBottom": "5px",
                                "marginTop": "15px",
                                "color": colors[idx % len(colors)],
                            },
                        ),
                        html.P(
                            f"x=1, ω={omega:.3f}, δ₁={delta1:.3f}, γ₁={gamma1:.3f}, "
                            f"δ₂={delta2:.3f}, γ₂={gamma2:.3f}",
                            style={"whiteSpace": "nowrap"},
                        ),
                    ]
                )
            )

        current_params = ""
        system_values = ""
        paper_config_display = params_list
        figure_description = html.P(
            [
                html.Strong("Figure 1."),
                " Model explanation of different effort preference profiles. \
             Example value function shapes that illustrate the different \
             preference profiles accounted for under the Dual-Power (DPOWER) model.",
            ]
        )

    elif display_mode == "figure5":
        # [Rest of the figure5 code remains the same]
        colors = ["red", "green", "blue", "purple"]
        labels = [
            "Decreasing",
            "Increasing",
            "Decreasing-Increasing",
            "Increasing-Decreasing",
        ]
        params_list = [html.H4("Figure 5 Presets:")]

        for idx, label in enumerate(labels):
            params = FIGURE5_PRESETS[label]
            delta1 = params["δ₁"]
            delta2 = params["δ₂"]
            gamma1 = params["γ₁"]
            gamma2 = params["γ₂"]
            omega = params["ω"]

            sv = x * (
                1 + (omega * (delta1 * E**gamma1) - (1 - omega) * (delta2 * E**gamma2))
            )

            traces.append(
                go.Scatter(
                    x=E,
                    y=sv,
                    mode="lines",
                    name=label,
                    line=dict(color=colors[idx % len(colors)]),
                )
            )

            params_list.append(
                html.Div(
                    [
                        html.H5(
                            f"{label} Profile",
                            style={
                                "marginBottom": "5px",
                                "marginTop": "15px",
                                "color": colors[idx % len(colors)],
                            },
                        ),
                        html.P(
                            f"x=1, ω={omega:.3f}, δ₁={delta1:.3f}, γ₁={gamma1:.3f}, "
                            f"δ₂={delta2:.3f}, γ₂={gamma2:.3f}",
                            style={"whiteSpace": "nowrap"},
                        ),
                    ]
                )
            )

        current_params = ""
        system_values = ""
        paper_config_display = params_list
        figure_description = html.P(
            [
                html.Strong("Figure 5."),
                " Example value functions based on individual parameter estimates \
              of the Dual-Power (DPOWER) model. Shown are value functions that \
              decrease or increase monotonically (decreasing or increasing profile, \
              respectively), or initially decrease or increase and then reverse \
              in evaluation after given effort is reached (decreasing-increasing \
              or increasing-decreasing profile).",
            ]
        )

    # Create figure
    figure = {
        "data": traces,
        "layout": go.Layout(
            xaxis={
                "title": "Level of Effort",
                "range": [0, 1],
            },
            yaxis={"title": "Subjective Value"},
            showlegend=False,
            width=600,
            height=600,
            hovermode="closest",
        ),
    }

    return (
        figure,
        current_params,
        system_values,
        paper_config_display,
        figure_description,
    )


# Callbacks for increment and decrement buttons
increment_values = {"ω": 0.1, "δ₁": 0.1, "γ₁": 0.1, "δ₂": 0.1, "γ₂": 0.1}


def create_increment_decrement_callback(param_name):
    @app.callback(
        Output(f"{param_name}-input", "value"),
        Input(f"{param_name}-increment", "n_clicks"),
        Input(f"{param_name}-decrement", "n_clicks"),
        State(f"{param_name}-input", "value"),
        prevent_initial_call=True,
    )
    def update_param(increment_clicks, decrement_clicks, current_value):
        ctx = dash.callback_context
        if not ctx.triggered or current_value is None:
            return current_value

        button_id = ctx.triggered[0]["prop_id"].split(".")[0]
        increment = increment_values[param_name]
        new_value = current_value
        if button_id == f"{param_name}-increment":
            new_value += increment
        elif button_id == f"{param_name}-decrement":
            new_value -= increment

        # Enforce min/max bounds
        min_value = param_bounds[param_name]["min"]
        max_value = param_bounds[param_name]["max"]
        new_value = max(min_value, min(new_value, max_value))

        # Round new_value to 3 decimal places
        new_value = round(new_value, 3)

        return new_value


# Create callbacks for each parameter
for param in ["ω", "δ₁", "γ₁", "δ₂", "γ₂"]:
    create_increment_decrement_callback(param)


# Callback for Info Modal
@app.callback(
    Output("info-modal", "style"),
    [Input("info-button", "n_clicks"), Input("close-info-modal", "n_clicks")],
    [State("info-modal", "style")],
)
def toggle_info_modal(open_clicks, close_clicks, current_style):
    """Toggle the display of the info modal"""
    if current_style is None:
        current_style = {"display": "none"}
    ctx = dash.callback_context
    if not ctx.triggered:
        return current_style
    button_id = ctx.triggered[0]["prop_id"].split(".")[0]
    if button_id == "info-button":
        return {
            "display": "block",
            "position": "fixed",
            "zIndex": 1,
            "left": 0,
            "top": 0,
            "width": "100%",
            "height": "100%",
            "overflow": "auto",
            "backgroundColor": "rgba(0,0,0,0.4)",
        }
    elif button_id == "close-info-modal":
        return {"display": "none"}
    return current_style


if __name__ == "__main__":
    app.run_server(debug=True)
