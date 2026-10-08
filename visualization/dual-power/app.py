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

import re

import numpy as np
import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go

# Initialize app
app = dash.Dash(__name__, title="Model Visualizer")

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

PARAMS = ["ω", "δ₁", "γ₁", "δ₂", "γ₂"]

PARAM_DESCRIPTIONS = {
    "ω": "System weight",
    "δ₁": "Positive steepness",
    "γ₁": "Positive curvature",
    "δ₂": "Negative steepness",
    "γ₂": "Negative curvature",
}

LEGEND = [
    ("sv(x):", " subjective value relative to nominal value"),
    ("x:", " nominal value of outcome"),
    ("E:", " effort level as proportion of max effort"),
    ("ω:", " relative system weight"),
    ("δ₁:", " steepness of positive system"),
    ("γ₁:", " curvature of positive system"),
    ("δ₂:", " steepness of negative system"),
    ("γ₂:", " curvature of negative system"),
]

# --- Colors ---

# A figure cannot read the stylesheet, so the chart repeats its tokens here.
CHART_INK = "#14161A"
CHART_SUBTLE = "#5C6169"
CHART_HAIRLINE = "rgba(20, 22, 26, 0.12)"
CHART_FONT = '"Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, Helvetica, Arial, sans-serif'

# Each curve has one color, shared by its line and its dot.
SYSTEM_COLORS = {"positive": "#15983D", "negative": "#A73030", "combined": CHART_INK}
PROFILE_COLORS = ["#A73030", "#15983D", "#0C5BB0", "#9C6ADE"]

# A parameter symbol wears the color of the system it shapes, and the system
# weight stays black like the combined curve. The green is darker than the
# curve's, because text needs more contrast than a line.
SYMBOL_COLORS = {"positive": "#0F7A30", "negative": SYSTEM_COLORS["negative"]}
SYMBOL_CLASSES = {
    "ω": "sym sym-weight",
    "δ₁": "sym sym-positive",
    "γ₁": "sym sym-positive",
    "δ₂": "sym sym-negative",
    "γ₂": "sym sym-negative",
}
SYMBOL_PATTERN = re.compile("(" + "|".join(SYMBOL_CLASSES) + ")")

# --- Helper Functions ---


def weighted_systems(E, w, d1, g1, d2, g2):
    """Return the weighted positive and negative system terms at effort E."""
    return w * (d1 * E**g1), (1 - w) * (d2 * E**g2)


def mark_symbols(text):
    """Split text into children, wrapping each parameter symbol in a styled span."""
    return [
        html.Span(part, className=SYMBOL_CLASSES[part]) if part in SYMBOL_CLASSES else part
        for part in SYMBOL_PATTERN.split(text)
        if part
    ]


def format_signed(value):
    """Format a system value with its sign, printing zero as +0.000."""
    text = f"{value:+.3f}"
    return "+0.000" if text == "-0.000" else text


def create_parameter_input(param, min_value, max_value, default_value, step):
    """Create a parameter input control."""
    return html.Div(
        className="param",
        children=[
            # Label Row (Symbol + Description)
            html.Label(
                htmlFor=f"{param}-input",
                className="param-label",
                children=[
                    html.Span(param, className=f"param-symbol {SYMBOL_CLASSES[param]}"),
                    html.Span(PARAM_DESCRIPTIONS[param], className="param-desc"),
                ],
            ),
            # Controls Row (Min - Btn - Input - Btn - Max)
            html.Div(
                className="param-row",
                children=[
                    html.Span(f"{min_value}", className="param-bound"),
                    html.Button(
                        "-",
                        id=f"{param}-decrement",
                        n_clicks=0,
                        className="param-btn",
                    ),
                    dcc.Input(
                        id=f"{param}-input",
                        type="number",
                        min=min_value,
                        max=max_value,
                        step=step,
                        value=default_value,
                        className="param-input",
                        persistence=True,
                        required=False,
                    ),
                    html.Button(
                        "+",
                        id=f"{param}-increment",
                        n_clicks=0,
                        className="param-btn",
                    ),
                    html.Span(f"max: {max_value}", className="param-bound"),
                ],
            ),
        ],
    )


# --- Layout Construction ---

parameter_controls = [
    create_parameter_input(
        param,
        param_bounds[param]["min"],
        param_bounds[param]["max"],
        default_params[param],
        step=0.001,
    )
    for param in PARAMS
]

app.layout = html.Div(
    className="app",
    style={
        "--sym-positive": SYMBOL_COLORS["positive"],
        "--sym-negative": SYMBOL_COLORS["negative"],
    },
    children=[
        # Header
        html.Header(
            className="masthead",
            children=[
                html.H1("Dual-Power Model Visualization", className="title"),
                html.Button(
                    "?",
                    id="info-button",
                    n_clicks=0,
                    className="info-btn",
                    **{"aria-haspopup": "dialog", "aria-controls": "info-modal"},
                ),
            ],
        ),
        # Info Modal
        html.Div(
            id="info-modal",
            className="modal",
            style={"display": "none"},
            role="dialog",
            **{"aria-modal": "true", "aria-labelledby": "info-title"},
            children=[
                html.Div(
                    className="modal-card",
                    children=[
                        html.Button(
                            "×",
                            id="close-info-modal",
                            n_clicks=0,
                            className="modal-close",
                        ),
                        html.H2("Model Visualizer", id="info-title", className="modal-title"),
                        html.P(
                            "This application visualizes the Dual-Power Model of "
                            "subjective value and effort. Adjust the parameters to see "
                            "how they affect the valuation curve. You can also view "
                            "predefined preference profiles from the manuscript.",
                        ),
                    ],
                )
            ],
        ),
        dcc.Store(id="display-mode", data="custom"),
        # Three zones on wide screens: plot and caption, model, controls
        html.Main(
            className="zones",
            children=[
                # Plot (the graph fills the height the stylesheet gives its wrapper)
                html.Div(
                    className="plot",
                    children=dcc.Graph(
                        id="sv-plot",
                        config={"displayModeBar": False, "responsive": True},
                    ),
                ),
                # Controls
                html.Section(
                    id="controls",
                    className="controls mode-custom",
                    children=[
                        html.H2("PARAMETERS:", className="section-label"),
                        *parameter_controls,
                        html.H2("PRESETS:", className="section-label"),
                        html.Div(
                            className="presets",
                            children=[
                                html.Button(
                                    "Figure 1 Values",
                                    id="show-figure1-button",
                                    n_clicks=0,
                                    className="preset-btn",
                                ),
                                html.Button(
                                    "Figure 5 Values",
                                    id="show-figure5-button",
                                    n_clicks=0,
                                    className="preset-btn",
                                ),
                                html.Button(
                                    "Custom",
                                    id="show-custom-button",
                                    n_clicks=0,
                                    className="preset-btn",
                                ),
                            ],
                        ),
                    ],
                ),
                # Caption
                html.Div(id="figure-description", className="caption"),
                # Model: Math & Stats
                html.Section(
                    className="model",
                    children=[
                        html.H2("Equation:", className="section-label"),
                        html.P(
                            mark_symbols("sv(x) = x·[1 + (ω·(δ₁·E^γ₁) - (1-ω)·(δ₂·E^γ₂))]"),
                            className="equation",
                        ),
                        html.H2(
                            "Current Parameters:",
                            id="current-params-label",
                            className="section-label",
                        ),
                        html.Div(id="current-params", className="param-line"),
                        html.Div(id="system-values"),
                        html.Div(id="paper-config-display"),
                        html.Div(
                            className="where",
                            children=[
                                html.H2("where:", className="section-label"),
                                html.Ul(
                                    className="legend-list",
                                    children=[
                                        html.Li([html.Strong(mark_symbols(term)), text])
                                        for term, text in LEGEND
                                    ],
                                ),
                            ],
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


@app.callback(Output("controls", "className"), Input("display-mode", "data"))
def mark_active_mode(display_mode):
    """Expose the mode as a class, so the stylesheet can mark the active preset."""
    return f"controls mode-{display_mode}"


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
    axis_style = {
        "automargin": True,
        "title_standoff": 14,
        "zeroline": False,
        "gridcolor": CHART_HAIRLINE,
        "linecolor": CHART_HAIRLINE,
        "tickfont": {"color": CHART_SUBTLE},
        "title_font": {"color": CHART_SUBTLE},
    }
    layout_settings = go.Layout(
        xaxis={"title": "Level of Effort", "range": [0, 1], **axis_style},
        yaxis={"title": "Subjective Value", **axis_style},
        showlegend=False,
        margin=dict(l=56, r=12, t=12, b=48),
        hovermode="closest",
        template="plotly_white",
        paper_bgcolor="rgba(0, 0, 0, 0)",
        plot_bgcolor="rgba(0, 0, 0, 0)",
        font=dict(family=CHART_FONT, size=14, color=CHART_INK),
    )

    if display_mode == "custom":
        # Handle inputs safely
        w = float(omega_input) if omega_input is not None else default_params["ω"]
        d1 = float(delta1_input) if delta1_input is not None else default_params["δ₁"]
        g1 = float(gamma1_input) if gamma1_input is not None else default_params["γ₁"]
        d2 = float(delta2_input) if delta2_input is not None else default_params["δ₂"]
        g2 = float(gamma2_input) if gamma2_input is not None else default_params["γ₂"]

        positive, negative = weighted_systems(E, w, d1, g1, d2, g2)
        pos_sys = x * (1 + positive)
        neg_sys = x * (1 - negative)
        sv = x * (1 + (positive - negative))

        traces = [
            go.Scatter(
                x=E,
                y=pos_sys,
                mode="lines",
                name="Positive System",
                line=dict(color=SYSTEM_COLORS["positive"], width=2, dash="dash"),
            ),
            go.Scatter(
                x=E,
                y=neg_sys,
                mode="lines",
                name="Negative System",
                line=dict(color=SYSTEM_COLORS["negative"], width=2, dash="dash"),
            ),
            go.Scatter(
                x=E,
                y=sv,
                mode="lines",
                name="Combined System",
                line=dict(color=SYSTEM_COLORS["combined"], width=3),
            ),
        ]

        current_params = mark_symbols(
            f"x=1, ω={w:.3f}, δ₁={d1:.3f}, γ₁={g1:.3f}, δ₂={d2:.3f}, γ₂={g2:.3f}"
        )

        # Calc values at E=0.5
        e_mid = 0.5
        v_pos, v_neg = weighted_systems(e_mid, w, d1, g1, d2, g2)
        v_neg = -v_neg
        v_net = v_pos + v_neg

        # The dots double as the plot's legend
        rows = [
            ("positive", "Weighted Positive System (ω·(δ₁·E^γ₁)):", v_pos),
            ("negative", "Weighted Negative System (-(1-ω)·(δ₂·E^γ₂)):", v_neg),
            ("combined", "Net System Effect:", v_net),
        ]
        system_values = html.Div(
            className="readout",
            children=[
                html.Div(
                    className="readout-row",
                    children=[
                        html.Span("● ", style={"color": SYSTEM_COLORS[key]}),
                        html.Span(mark_symbols(label), className="readout-label"),
                        html.Span(format_signed(value), className="readout-value"),
                    ],
                )
                for key, label, value in rows
            ]
            + [html.Div("System Values at E=0.5", className="readout-note")],
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

        info_items = [
            html.H2(
                f"{'Figure 1' if display_mode == 'figure1' else 'Figure 5'} Presets:",
                className="section-label",
            )
        ]

        for idx, (label, p) in enumerate(presets.items()):
            positive, negative = weighted_systems(
                E, p["ω"], p["δ₁"], p["γ₁"], p["δ₂"], p["γ₂"]
            )
            sv = x * (1 + (positive - negative))
            col = PROFILE_COLORS[idx % len(PROFILE_COLORS)]
            traces.append(
                go.Scatter(
                    x=E, y=sv, mode="lines", name=label, line=dict(color=col, width=3)
                )
            )

            info_items.append(
                html.Div(
                    className="profile",
                    children=[
                        html.Div(
                            className="profile-head",
                            children=[
                                html.Span("● ", style={"color": col}),
                                html.H3(f"{label} Profile", className="profile-name"),
                            ],
                        ),
                        html.Div(
                            mark_symbols(
                                f"x=1, ω={p['ω']:.3f}, δ₁={p['δ₁']:.3f}, γ₁={p['γ₁']:.3f}, δ₂={p['δ₂']:.3f}, γ₂={p['γ₂']:.3f}"
                            ),
                            className="param-line",
                        ),
                    ],
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


for p in PARAMS:
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
    return {"display": "none"} if is_open else {"display": "block"}


if __name__ == "__main__":
    app.run(debug=True)
