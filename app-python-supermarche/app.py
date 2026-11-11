# M1-ECAP
# ETUDIANT: Patrice Kolié
# Analyse interactive des ventes par sexe et par ville


# A) IMPORT ET DONNÉES
import warnings
warnings.filterwarnings('ignore', category=FutureWarning)


import pandas as pd
import dash
from dash import dcc, html, Input, Output
import dash_bootstrap_components as dbc
import plotly.express as px
import plotly.graph_objects as go


# B) CHARGEMENT DES DONNÉES
df = pd.read_csv("supermarket_sales.csv")

df.columns = [
    "invoice_id", "branch", "city", "customer_type", "gender",
    "product_line", "unit_price", "quantity", "tax", "total",
    "date", "time", "payment", "cogs", "gross_margin",
    "gross_income", "rating"
]

df["date"] = pd.to_datetime(df["date"], format="%m/%d/%Y")
df.sort_values("date", inplace=True)


# C) COULEURS
COULEUR_FOND = "#f8f9fa"
COULEUR_HEADER = "#2c3e50"
COULEUR_CARTE = "#ffffff"
COULEUR_TEXTE = "#2c3e50"
COULEUR_TEXTE_CLAIR = "#7f8c8d"

COULEURS_GENRE = {
    "Male": "#3498db",
    "Female": "#e74c3c"
}

COULEURS_VILLE = {
    "Yangon": "#3498db",
    "Mandalay": "#e74c3c",
    "Naypyitaw": "#2ecc71"
}

COULEURS_PRODUITS = [
    "#3498db", "#e74c3c", "#2ecc71",
    "#f39c12", "#9b59b6", "#1abc9c"
]


# D) FONCTION FORMAT

def format_nombre(nombre):
    if nombre >= 1000000:
        return f"${nombre/1000000:.1f}M"
    elif nombre >= 1000:
        return f"${nombre/1000:.1f}K"
    return f"${nombre:.0f}"


# E) GRAPHIQUES
def plot_barres(data):
    df_grouped = (
        data.groupby(["city", "gender"])["invoice_id"]
        .count()
        .reset_index()
        .rename(columns={"invoice_id": "nb_achats"})
    )

    fig = px.bar(
        df_grouped,
        x="city",
        y="nb_achats",
        color="gender",
        barmode="group",
        color_discrete_map=COULEURS_GENRE
    )

    fig.update_layout(
        title="Total number of purchases",
        paper_bgcolor="white",
        plot_bgcolor="white",
        height=350
    )

    return fig


def plot_pie(data):
    df_grouped = (
        data.groupby("product_line")["invoice_id"]
        .count()
        .reset_index()
        .rename(columns={"invoice_id": "nb"})
    )

    fig = px.pie(
        df_grouped,
        names="product_line",
        values="nb",
        hole=0.4,
        color_discrete_sequence=COULEURS_PRODUITS
    )

    fig.update_layout(
        title="Breakdown by product category",
        height=350
    )

    return fig


def plot_line(data):
    df_grouped = (
        data.groupby([pd.Grouper(key="date", freq="W-MON"), "city"])["total"]
        .sum()
        .reset_index()
    )

    fig = px.line(
        df_grouped,
        x="date",
        y="total",
        color="city",
        markers=True,
        color_discrete_map=COULEURS_VILLE
    )

    fig.update_layout(
        title="Évolution hebdomadaire du montant total",
        paper_bgcolor="white",
        plot_bgcolor="white",
        height=350
    )

    return fig


# F) OPTIONS
options_villes = [{"label": v, "value": v} for v in sorted(df["city"].unique())]
options_sexes = [{"label": s, "value": s} for s in sorted(df["gender"].unique())]


# G) APPLICATION
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])
app.title = "Dashboard Supermarket"

server = app.server 

# H) LAYOUT
app.layout = html.Div(
    style={"backgroundColor": COULEUR_FOND, "minHeight": "100vh"},
    children=[

        # HEADER
        html.Div(
            style={
                "backgroundColor": COULEUR_HEADER,
                "padding": "25px"
            },
            children=[
                html.H1(
                    "🛒 Dashboard Supermarket Sales",
                    style={"color": "white"}
                ),

                dbc.Row([
                    dbc.Col(
                        dcc.Dropdown(
                            id="ville-dropdown",
                            options=options_villes,
                            placeholder="Choisir une ville"
                        ),
                        md=6
                    ),

                    dbc.Col(
                        dcc.Dropdown(
                            id="sexe-dropdown",
                            options=options_sexes,
                            placeholder="Choisir un sexe"
                        ),
                        md=6
                    )
                ])
            ]
        ),

        dbc.Container(
            fluid=True,
            style={"padding": "30px"},
            children=[

                # KPI
                dbc.Row([

                    dbc.Col(
                        html.Div([
                            html.H5("Average Rating"),
                            html.H2(id="kpi-rating")
                        ], className="p-4 shadow bg-white rounded"),
                        md=6
                    ),

                    dbc.Col(
                        html.Div([
                            html.H5("Total purchase amount"),
                            html.H2(id="kpi-total")
                        ], className="p-4 shadow bg-white rounded"),
                        md=6
                    )

                ], className="mb-4"),

                # Graphique ligne
                dbc.Row([
                    dbc.Col(
                        html.Div(
                            dcc.Graph(id="graph-line"),
                            className="p-3 shadow bg-white rounded"
                        )
                    )
                ], className="mb-4"),

                # Deux graphiques
                dbc.Row([

                    dbc.Col(
                        html.Div(
                            dcc.Graph(id="graph-bar"),
                            className="p-3 shadow bg-white rounded"
                        ),
                        md=6
                    ),

                    dbc.Col(
                        html.Div(
                            dcc.Graph(id="graph-pie"),
                            className="p-3 shadow bg-white rounded"
                        ),
                        md=6
                    )

                ])
            ]
        )
    ]
)


# I) CALLBACK
@app.callback(
    Output("kpi-rating", "children"),
    Output("kpi-total", "children"),
    Output("graph-line", "figure"),
    Output("graph-bar", "figure"),
    Output("graph-pie", "figure"),

    Input("ville-dropdown", "value"),
    Input("sexe-dropdown", "value")
)
def update_dashboard(ville, sexe):

    dff = df.copy()

    if ville:
        dff = dff[dff["city"] == ville]

    if sexe:
        dff = dff[dff["gender"] == sexe]

    if len(dff) == 0:
        fig_vide = go.Figure()
        fig_vide.add_annotation(
            text="Aucune donnée",
            x=0.5,
            y=0.5,
            showarrow=False
        )
        return "--", "--", fig_vide, fig_vide, fig_vide

    # KPI
    rating_moyen = dff["rating"].mean()
    montant_total = dff["total"].sum()

    # Graphiques
    fig_line = plot_line(dff)
    fig_bar = plot_barres(dff)
    fig_pie = plot_pie(dff)

    return (
        f"{rating_moyen:.2f}",
        format_nombre(montant_total),
        fig_line,
        fig_bar,
        fig_pie
    )


# J) LANCEMENT
if __name__ == "__main__":
    app.run_server(debug=True)