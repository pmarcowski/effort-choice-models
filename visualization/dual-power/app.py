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
            /* --- Base Styles --- */
            body {
                font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, Helvetica, Arial, sans-serif;
                margin: 0;
                padding: 0;
                background: #ffffff;
                color: #222;
                -webkit-font-smoothing: antialiased;
            }

            p, li, div { line-height: 1.6; color: #333; }
            h2 { font-weight: 600; color: #111; margin-top: 0; margin-bottom: 0; letter-spacing: -0.5px; }
            h4 { font-weight: 600; color: #333; margin-top: 0; margin-bottom: 12px; font-size: 18px; }
            h5 { font-weight: 600; margin-bottom: 0.25rem; margin-top: 0; font-size: 15px; }

            /* --- Layout Grid --- */
            .main-container {
                max-width: 1400px;
                margin: 0 auto;
                padding: 40px;
            }

            .main-flex-row {
                display: flex;
                flex-direction: row;
                justify-content: center;
                align-items: flex-start;
                gap: 0;
                width: 100%;
                box-sizing: border-box;
            }

            /* --- Columns --- */
            
            /* Plot Column */
            .flex-col-plot {
                flex: 1 1 550px;
                min-width: 320px;
                box-sizing: border-box;
                padding-right: 50px;
            }

            /* Middle Column: Math & Stats */
            .flex-col-middle {
                flex: 0 1 auto;
                min-width: 400px;
                box-sizing: border-box;
                border-left: 1px solid #e5e5e5;
                border-right: 1px solid #e5e5e5;
                padding: 0 45px;
            }

            /* Controls Column */
            .flex-col-params {
                flex: 0 0 280px;
                box-sizing: border-box;
                padding-left: 45px;
            }

            /* --- Typography Components --- */
            
            .section-label {
                font-size: 12px;
                font-weight: 700;
                text-transform: uppercase;
                letter-spacing: 0.5px;
                color: #777;
                margin-bottom: 15px;
                margin-top: 30px;
                display: block;
            }
            .section-label:first-child { margin-top: 0; }

            /* Equation Box Styling - RESTORED */
            .equation-box {
                font-family: "Times New Roman", Times, serif;
                font-size: 20px;
                font-style: italic;
                background: #f9f9f9;
                padding: 18px 24px;
                border-radius: 6px;
                border: 1px solid #e0e0e0;
                text-align: center;
                margin-bottom: 25px;
                color: #111;
            }

            .stat-value {
                font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
                font-size: 14px;
                color: #333;
            }

            .legend-list li {
                margin-bottom: 6px;
                font-size: 14px;
                color: #555;
            }
            .legend-list strong {
                color: #222;
                font-weight: 600;
                margin-right: 4px;
            }

            /* --- UI Components --- */

            /* Parameter Buttons (+/-) */
            .param-btn {
                background-color: #fff;
                border: 1px solid #d1d5db;
                border-radius: 4px;
                color: #555;
                font-weight: bold;
                font-size: 14px;
                width: 30px;
                height: 30px;
                cursor: pointer;
                transition: all 0.1s ease;
                display: flex;
                align-items: center;
                justify-content: center;
                padding: 0;
            }
            .param-btn:hover { background-color: #f3f4f6; border-color: #9ca3af; color: #111; }
            .param-btn:active { background-color: #e5e7eb; }

            /* Inputs */
            .param-input input {
                border: 1px solid #d1d5db;
                border-radius: 4px;
                padding: 4px;
                color: #111;
                font-family: "SFMono-Regular", Consolas, monospace;
                font-weight: 600;
                font-size: 15px;
                background: #fff;
                height: 30px;
                box-sizing: border-box;
            }
            .param-input input:focus { outline: none; border-color: #333; }
            
            /* Action Buttons */
            .action-btn {
                background-color: #fff;
                border: 1px solid #d1d5db;
                color: #374151;
                padding: 10px 16px;
                border-radius: 6px;
                font-size: 14px;
                font-weight: 600;
                cursor: pointer;
                transition: all 0.1s ease;
                text-align: center;
                font-family: inherit;
            }
            .action-btn:hover { background-color: #f3f4f6; border-color: #9ca3af; color: #111; }
            
            /* Primary Button */
            .action-btn-primary {
                background-color: #222;
                border: 1px solid #222;
                color: #fff;
            }
            .action-btn-primary:hover { background-color: #444; border-color: #444; color: #fff; }

            /* --- Mobile Responsiveness (< 1100px) --- */
            @media (max-width: 1100px) {
                .main-container { padding: 20px; }
                .main-flex-row {
                    flex-direction: column;
                    gap: 40px;
                    align-items: center;
                }
                .flex-col-plot, .flex-col-middle, .flex-col-params {
                    width: 100%;
                    max-width: 700px;
                    padding: 0;
                    border: none;
                }
                .flex-col-middle {
                    border-top: 1px solid #eee;
                    border-bottom: 1px solid #eee;
                    padding: 30px 0;
                }
            }
            
            /* Hide spinner arrows */
            input[type=number]::-webkit-outer-spin-button,
            input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
            input[type=number] { -moz-appearance: textfield; }
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

# --- Data Presets (Normalized Order) ---

FIGURE1_PRESETS = {
    "Decreasing": {"δ₁": 0, "δ₂": 2, "γ₁": 0, "γ₂": 2, "ω": 0},
    "Increasing": {"δ₁": 2, "δ₂": 0, "γ₁": 0.5, "γ₂": 0, "ω": 1},
    "Decreasing-Increasing": {"δ₁": 3, "δ₂": 3, "γ₁": 15, "γ₂": 3, "ω": 0.5},
    "Increasing-Decreasing": {"δ₁": 3.5, "δ₂": 3.5, "γ₁": 1, "γ₂": 3, "ω": 0.5},
}

# Normalized to match Figure 1 order (Dec -> Inc -> Dec-Inc -> Inc-Dec)
FIGURE5_PRESETS = {
    "Decreasing": {
        "δ₁": 1e-07,
        "γ₁": 9.582383e-07,
        "δ₂": 4.55966,
        "γ₂": 3.306229,
        "ω": 1e-07,
    },
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
    "Increasing-Decreasing": {
        "δ₁": 3.492561,
        "γ₁": 3.258896,
        "δ₂": 2.43447,
        "γ₂": 25.08094,
        "ω": 0.382991,
    },
}

default_params = {"ω": 0.5, "δ₁": 0, "γ₁": 1, "δ₂": 0, "γ₂": 1}

param_bounds = {
    "ω": {"min": 0, "max": 1},
    "δ₁": {"min": 0, "max": 100},
    "γ₁": {"min": 0, "max": 100},
    "δ₂": {"min": 0, "max": 100},
    "γ₂": {"min": 0, "max": 100},
}

# --- Helper Functions ---


def create_parameter_input(
    param_name, param_label, min_value, max_value, default_value, step
):
    """Create a styled parameter input control."""
    descriptions = {
        "ω": "System weight",
        "δ₁": "Positive steepness",
        "γ₁": "Positive curvature",
        "δ₂": "Negative steepness",
        "γ₂": "Negative curvature",
    }

    return html.Div(
        style={"marginBottom": "24px"},
        children=[
            # Label Row (Symbol + Description)
            html.Div(
                children=[
                    html.Span(
                        param_label,
                        style={
                            "fontSize": "18px",
                            "fontWeight": "700",
                            "marginRight": "8px",
                        },
                    ),
                    html.Span(
                        descriptions[param_label],
                        style={"fontSize": "15px", "color": "#666"},
                    ),
                ],
                style={"marginBottom": "8px"},
            ),
            # Controls Row (Min - Btn - Input - Btn - Max)
            html.Div(
                style={
                    "display": "flex",
                    "alignItems": "center",
                    "whiteSpace": "nowrap",
                },
                children=[
                    html.Label(
                        f"{min_value}",
                        style={
                            "marginRight": "8px",
                            "color": "#999",
                            "fontSize": "13px",
                            "minWidth": "10px",
                        },
                    ),
                    html.Button(
                        "-",
                        id=f"{param_name}-decrement",
                        n_clicks=0,
                        className="param-btn",
                    ),
                    dcc.Input(
                        id=f"{param_name}-input",
                        type="number",
                        min=min_value,
                        max=max_value,
                        step=step,
                        value=default_value,
                        className="param-input",
                        style={
                            "width": "60px",
                            "textAlign": "center",
                            "margin": "0 6px",
                        },
                        persistence=True,
                        required=False,
                    ),
                    html.Button(
                        "+",
                        id=f"{param_name}-increment",
                        n_clicks=0,
                        className="param-btn",
                    ),
                    html.Label(
                        f"max: {max_value}",
                        style={
                            "marginLeft": "8px",
                            "color": "#999",
                            "fontSize": "13px",
                        },
                    ),
                ],
            ),
        ],
    )


# --- Layout Construction ---

parameter_controls = []
for param in ["ω", "δ₁", "γ₁", "δ₂", "γ₂"]:
    min_val = param_bounds[param]["min"]
    max_val = param_bounds[param]["max"]
    step = 0.001
    parameter_controls.append(
        create_parameter_input(
            param, param, min_val, max_val, default_params[param], step
        )
    )

app.layout = html.Div(
    className="main-container",
    children=[
        # Header
        html.Div(
            style={
                "position": "relative",
                "marginBottom": "50px",
                "display": "flex",
                "justifyContent": "center",
                "alignItems": "center",
                "gap": "15px",
            },
            children=[
                html.H2("Dual-Power Model Visualization"),
                html.Button(
                    "i",
                    id="info-button",
                    n_clicks=0,
                    style={
                        "backgroundColor": "#fff",
                        "border": "2px solid #333",
                        "borderRadius": "50%",
                        "fontSize": "16px",
                        "fontWeight": "bold",
                        "width": "32px",
                        "height": "32px",
                        "textAlign": "center",
                        "padding": "0",
                        "cursor": "pointer",
                        "color": "#333",
                        "lineHeight": "28px",
                        "fontFamily": "Segoe UI, sans-serif",
                    },
                ),
            ],
        ),
        # Info Modal
        html.Div(
            id="info-modal",
            style={
                "display": "none",
                "position": "fixed",
                "zIndex": 1000,
                "left": 0,
                "top": 0,
                "width": "100%",
                "height": "100%",
                "overflow": "auto",
                "backgroundColor": "rgba(0,0,0,0.5)",
                "backdropFilter": "blur(3px)",
            },
            children=[
                html.Div(
                    style={
                        "backgroundColor": "#fff",
                        "margin": "10% auto",
                        "padding": "40px",
                        "borderRadius": "8px",
                        "boxShadow": "0 10px 25px rgba(0,0,0,0.1)",
                        "width": "90%",
                        "maxWidth": "550px",
                        "position": "relative",
                    },
                    children=[
                        html.Button(
                            "×",
                            id="close-info-modal",
                            n_clicks=0,
                            style={
                                "position": "absolute",
                                "top": "15px",
                                "right": "20px",
                                "background": "none",
                                "border": "none",
                                "fontSize": "28px",
                                "color": "#999",
                                "cursor": "pointer",
                            },
                        ),
                        html.H2(
                            "Model Visualizer",
                            style={"marginTop": 0, "marginBottom": "20px"},
                        ),
                        html.P(
                            "This application visualizes the Dual-Power Model of "
                            "subjective value and effort. Adjust the parameters to see "
                            "how they affect the valuation curve. You can also view "
                            "predefined preference profiles from the manuscript.",
                            style={
                                "fontSize": "15px",
                                "color": "#555",
                                "lineHeight": "1.6",
                            },
                        ),
                    ],
                )
            ],
        ),
        dcc.Store(id="display-mode", data="custom"),
        # Main Grid
        html.Div(
            className="main-flex-row",
            children=[
                # Left Column: Plot
                html.Div(
                    className="flex-col-plot",
                    children=[
                        dcc.Graph(
                            id="sv-plot",
                            config={"displayModeBar": False, "responsive": True},
                            style={"width": "100%", "height": "550px"},
                        ),
                        html.Div(
                            id="figure-description",
                            style={
                                "marginTop": "30px",
                                "fontSize": "15px",
                                "color": "#444",
                                "lineHeight": "1.7",
                                "textAlign": "justify",
                            },
                        ),
                    ],
                ),
                # Middle Column: Math & Stats
                html.Div(
                    className="flex-col-middle",
                    children=[
                        html.Label("Equation:", className="section-label"),
                        html.P(
                            "sv(x) = x·[1 + (ω·(δ₁·E^γ₁) - (1-ω)·(δ₂·E^γ₂))]",
                            className="equation-box",
                        ),
                        html.Label(
                            "Current Parameters:",
                            id="current-params-label",
                            className="section-label",
                        ),
                        html.Div(
                            id="current-params",
                            className="stat-value",
                            style={
                                "marginBottom": "30px",
                                "color": "#333",
                                "fontSize": "15px",
                            },
                        ),
                        html.Div(id="system-values"),
                        html.Div(id="paper-config-display"),
                        html.Div(
                            style={
                                "marginTop": "45px",
                                "paddingTop": "25px",
                                "borderTop": "1px solid #eaeaea",
                            },
                            children=[
                                html.Label("where:", className="section-label"),
                                html.Ul(
                                    className="legend-list",
                                    style={
                                        "listStyleType": "none",
                                        "paddingLeft": "20px",
                                        "margin": 0,
                                    },
                                    children=[
                                        html.Li(
                                            [
                                                html.Strong("sv(x):"),
                                                " subjective value relative to nominal value",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("x:"),
                                                " nominal value of outcome",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("E:"),
                                                " effort level as proportion of max effort",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("ω:"),
                                                " relative system weight",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("δ₁:"),
                                                " steepness of positive system",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("γ₁:"),
                                                " curvature of positive system",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("δ₂:"),
                                                " steepness of negative system",
                                            ]
                                        ),
                                        html.Li(
                                            [
                                                html.Strong("γ₂:"),
                                                " curvature of negative system",
                                            ]
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
                # Right Column: Controls
                html.Div(
                    className="flex-col-params",
                    children=[
                        html.Label("PARAMETERS:", className="section-label"),
                        html.Div(children=parameter_controls),
                        html.Hr(
                            style={
                                "border": "0",
                                "borderTop": "1px solid #eaeaea",
                                "margin": "35px 0 25px 0",
                            }
                        ),
                        html.Label("PRESETS:", className="section-label"),
                        html.Button(
                            "Figure 1 Values",
                            id="show-figure1-button",
                            n_clicks=0,
                            className="action-btn",
                            style={"marginBottom": "12px", "width": "100%"},
                        ),
                        html.Button(
                            "Figure 5 Values",
                            id="show-figure5-button",
                            n_clicks=0,
                            className="action-btn",
                            style={"marginBottom": "12px", "width": "100%"},
                        ),
                        html.Button(
                            "Custom",
                            id="show-custom-button",
                            n_clicks=0,
                            className="action-btn action-btn-primary",
                            style={"width": "100%"},
                        ),
                    ],
                ),
            ],
        ),
    ],
)

# --- Callbacks ---


@app.callback(
    Output("display-mode", "data"),
    [
        Input("show-figure1-button", "n_clicks"),
        Input("show-figure5-button", "n_clicks"),
        Input("show-custom-button", "n_clicks"),
    ],
)
def update_display_mode(n_clicks_fig1, n_clicks_fig5, n_clicks_custom):
    ctx = dash.callback_context
    if not ctx.triggered:
        return "custom"
    button_id = ctx.triggered[0]["prop_id"].split(".")[0]
    if button_id == "show-figure1-button":
        return "figure1"
    if button_id == "show-figure5-button":
        return "figure5"
    return "custom"


@app.callback(
    [
        Output("sv-plot", "figure"),
        Output("current-params", "children"),
        Output("system-values", "children"),
        Output("paper-config-display", "children"),
        Output("figure-description", "children"),
        Output("current-params-label", "style"),
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
    E = np.linspace(0, 1, 100)
    x = 1
    traces = []
    figure_description = ""
    current_params = ""
    system_values = ""
    paper_config_display = ""
    label_style = {"display": "block"}

    # Common layout settings
    layout_settings = go.Layout(
        xaxis={
            "title": "Level of Effort",
            "range": [0, 1],
            "automargin": True,
            "title_standoff": 25,
            "zeroline": False,
        },
        yaxis={
            "title": "Subjective Value",
            "automargin": True,
            "title_standoff": 25,
            "zeroline": False,
        },
        showlegend=False,
        margin=dict(l=80, r=30, t=20, b=50),
        hovermode="closest",
        template="plotly_white",
        font=dict(family="Segoe UI, sans-serif", size=13, color="#333"),
    )

    if display_mode == "custom":
        # Handle inputs safely
        w = float(omega_input) if omega_input is not None else default_params["ω"]
        d1 = float(delta1_input) if delta1_input is not None else default_params["δ₁"]
        g1 = float(gamma1_input) if gamma1_input is not None else default_params["γ₁"]
        d2 = float(delta2_input) if delta2_input is not None else default_params["δ₂"]
        g2 = float(gamma2_input) if gamma2_input is not None else default_params["γ₂"]

        pos_sys = x * (1 + w * (d1 * E**g1))
        neg_sys = x * (1 - (1 - w) * (d2 * E**g2))
        sv = x * (1 + (w * (d1 * E**g1) - (1 - w) * (d2 * E**g2)))

        traces = [
            go.Scatter(
                x=E,
                y=pos_sys,
                mode="lines",
                name="Positive System",
                line=dict(color="#2ca02c", width=3, dash="dash"),
                opacity=0.6,
            ),
            go.Scatter(
                x=E,
                y=neg_sys,
                mode="lines",
                name="Negative System",
                line=dict(color="#d62728", width=3, dash="dash"),
                opacity=0.6,
            ),
            go.Scatter(
                x=E,
                y=sv,
                mode="lines",
                name="Combined System",
                line=dict(color="#1f2937", width=4),
            ),
        ]

        # Disable plot legend in custom mode (using dots in middle col instead)
        layout_settings.showlegend = False

        current_params = (
            f"x=1, ω={w:.3f}, δ₁={d1:.3f}, γ₁={g1:.3f}, δ₂={d2:.3f}, γ₂={g2:.3f}"
        )

        # Calc values at E=0.5
        e_mid = 0.5
        v_pos = w * (d1 * e_mid**g1)
        v_neg = -(1 - w) * (d2 * e_mid**g2)
        v_net = v_pos + v_neg

        system_values = html.Div(
            [
                # Green Dot + Positive
                html.Div(
                    [
                        html.Span(
                            "● ",
                            style={
                                "color": "#2ca02c",
                                "fontSize": "18px",
                                "lineHeight": "1",
                                "marginRight": "8px",
                            },
                        ),
                        # Added marginRight here to force spacing
                        html.Span(
                            "Weighted Positive System (ω·(δ₁·E^γ₁)):",
                            style={"color": "#555", "marginRight": "8px"},
                        ),
                        html.Span(
                            f"{v_pos:+.3f}",
                            style={"color": "#2ca02c", "fontWeight": "bold"},
                        ),
                    ],
                    style={
                        "whiteSpace": "nowrap",
                        "marginBottom": "6px",
                        "display": "flex",
                        "alignItems": "center",
                    },
                ),
                # Red Dot + Negative
                html.Div(
                    [
                        html.Span(
                            "● ",
                            style={
                                "color": "#d62728",
                                "fontSize": "18px",
                                "lineHeight": "1",
                                "marginRight": "8px",
                            },
                        ),
                        # Added marginRight here
                        html.Span(
                            "Weighted Negative System (-(1-ω)·(δ₂·E^γ₂)):",
                            style={"color": "#555", "marginRight": "8px"},
                        ),
                        html.Span(
                            f"{v_neg:+.3f}",
                            style={"color": "#d62728", "fontWeight": "bold"},
                        ),
                    ],
                    style={
                        "whiteSpace": "nowrap",
                        "marginBottom": "6px",
                        "display": "flex",
                        "alignItems": "center",
                    },
                ),
                # Black Dot + Net
                html.Div(
                    [
                        html.Span(
                            "● ",
                            style={
                                "color": "#1f2937",
                                "fontSize": "18px",
                                "lineHeight": "1",
                                "marginRight": "8px",
                            },
                        ),
                        # Added marginRight here
                        html.Span(
                            "Net System Effect:",
                            style={"color": "#555", "marginRight": "8px"},
                        ),
                        html.Span(
                            f"{v_net:+.3f}",
                            style={"fontWeight": "bold", "color": "#1f2937"},
                        ),
                    ],
                    style={
                        "whiteSpace": "nowrap",
                        "marginBottom": "15px",
                        "display": "flex",
                        "alignItems": "center",
                    },
                ),
                # Footnote
                html.Div(
                    "System Values at E=0.5",
                    style={
                        "fontSize": "12px",
                        "color": "#999",
                        "fontWeight": "bold",
                    },
                ),
            ]
        )

        figure_description = html.P(
            [
                html.Strong("Interactive Mode."),
                " Adjust the parameters using the controls on the right to explore "
                "different value function shapes. The weighted positive system (green) "
                "and negative system (red) values at E=0.5 are shown in the middle panel. "
                "Parameters can be modified using the +/- buttons or by directly entering values. "
                "The plot shows individual contributions of the positive (green) and "
                "negative (red) systems, with their combined effect in black.",
            ]
        )

    else:
        # Figure Mode
        label_style = {"display": "none"}
        presets = FIGURE1_PRESETS if display_mode == "figure1" else FIGURE5_PRESETS
        colors = ["#d62728", "#2ca02c", "#1f77b4", "#9467bd"]

        info_items = [
            html.Label(
                f"{'Figure 1' if display_mode == 'figure1' else 'Figure 5'} Presets:",
                className="section-label",
            )
        ]

        for idx, (label, p) in enumerate(presets.items()):
            sv = x * (
                1
                + (
                    p["ω"] * (p["δ₁"] * E ** p["γ₁"])
                    - (1 - p["ω"]) * (p["δ₂"] * E ** p["γ₂"])
                )
            )
            col = colors[idx % len(colors)]
            traces.append(
                go.Scatter(
                    x=E, y=sv, mode="lines", name=label, line=dict(color=col, width=4)
                )
            )

            info_items.append(
                html.Div(
                    [
                        html.Div(
                            [
                                html.Span(
                                    "● ",
                                    style={
                                        "color": col,
                                        "fontSize": "18px",
                                        "lineHeight": "1",
                                        "marginRight": "8px",
                                    },
                                ),
                                html.H5(
                                    f"{label} Profile",
                                    style={
                                        "color": "#333",
                                        "fontSize": "15px",
                                        "margin": 0,
                                    },
                                ),
                            ],
                            style={
                                "display": "flex",
                                "alignItems": "center",
                                "marginBottom": "4px",
                            },
                        ),
                        html.Div(
                            f"x=1, ω={p['ω']:.3f}, δ₁={p['δ₁']:.3f}, γ₁={p['γ₁']:.3f}, δ₂={p['δ₂']:.3f}, γ₂={p['γ₂']:.3f}",
                            className="stat-value",
                            style={
                                "fontSize": "12px",
                                "color": "#666",
                                "marginLeft": "22px",
                            },
                        ),
                    ],
                    style={"marginBottom": "18px"},
                )
            )

        paper_config_display = html.Div(info_items)

        fig_label = "Figure 1." if display_mode == "figure1" else "Figure 5."

        if display_mode == "figure1":
            desc_text = " Model explanation of different effort preference profiles. Example value function shapes that illustrate the different preference profiles accounted for under the Dual-Power (DPOWER) model."
        else:
            desc_text = " Example value functions based on individual parameter estimates of the Dual-Power (DPOWER) model. Shown are value functions that decrease or increase monotonically (decreasing or increasing profile, respectively), or initially decrease or increase and then reverse in evaluation after given effort is reached (decreasing-increasing or increasing-decreasing profile)."

        figure_description = html.P([html.Strong(fig_label), desc_text])

    return (
        {"data": traces, "layout": layout_settings},
        current_params,
        system_values,
        paper_config_display,
        figure_description,
        label_style,
    )


# Increment/Decrement Callbacks
def create_callback(param):
    @app.callback(
        Output(f"{param}-input", "value"),
        [
            Input(f"{param}-increment", "n_clicks"),
            Input(f"{param}-decrement", "n_clicks"),
        ],
        [State(f"{param}-input", "value")],
        prevent_initial_call=True,
    )
    def update(inc, dec, val):
        ctx = dash.callback_context
        if not ctx.triggered or val is None:
            return val
        btn = ctx.triggered[0]["prop_id"].split(".")[0]

        step = 0.1  # Button step size
        new_val = val + step if "increment" in btn else val - step

        # Clip
        mn, mx = param_bounds[param]["min"], param_bounds[param]["max"]
        return round(max(mn, min(mx, new_val)), 3)


for p in ["ω", "δ₁", "γ₁", "δ₂", "γ₂"]:
    create_callback(p)


# Modal Callback
@app.callback(
    Output("info-modal", "style"),
    [Input("info-button", "n_clicks"), Input("close-info-modal", "n_clicks")],
    [State("info-modal", "style")],
)
def toggle_modal(open_clicks, close_clicks, style):
    if not dash.callback_context.triggered:
        return style or {"display": "none"}
    is_open = style and style.get("display") == "block"
    return (
        {"display": "none"}
        if is_open
        else {
            "display": "block",
            "position": "fixed",
            "zIndex": 1000,
            "left": 0,
            "top": 0,
            "width": "100%",
            "height": "100%",
            "overflow": "auto",
            "backgroundColor": "rgba(0,0,0,0.5)",
            "backdropFilter": "blur(3px)",
        }
    )


if __name__ == "__main__":
    app.run(debug=True)
