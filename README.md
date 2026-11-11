# DOSSIER-ACCADEMIQUE-M1
## Contenu du dépôt

Chaque dossier correspond à un projet réalisé dans le cadre d'un enseignement du
Master 1. Certains projets sont individuels, d'autres ont été réalisés en binôme
ou en trinôme (co-auteurs indiqués dans le tableau).

| Dossier | Description | Co-auteurs |
|---|---|---|
| `analyse-inegalites` | Analyse des inégalités entre les pays du monde à partir d'indicateurs socio-économiques, démographiques et sanitaires : statistiques univariées et bivariées (tests de Shapiro-Wilk, ANOVA, Khi-2, corrélations de Spearman), AFC, ACP et classification | Jules Bouscaud |
| `memoire-fin-vie-vetements` | Mémoire de recherche : modélisation des choix de fin de vie des vêtements (jeter, donner, revendre) par logit multinomial général | Individuel |
| `package-r` | Création d'un package R dans le cadre du cours de R avancé | À préciser |
| `app-shiny-football` | Prototype d'application Shiny autour de compétitions de football, avec gestion des amis, portefeuille et pronostics amicaux | À préciser |
| `biostatistiques` | Analyse socio-économique et sanitaire des pays du monde, centrée sur le traitement des données : détection des valeurs manquantes et comparaison de plusieurs méthodes d'imputation (données externes, MCO, moyenne, ACP, imputation multiple MICE) | Diane Anato |
| `dashboard-powerbi` | Tableau de bord interactif de suivi des ventes (données Contoso) : pages Accueil, Boutiques, Produits et Ventes, avec segments croisés, mesures DAX et comparaison du CA à l'année précédente | Individuel |
| `portefeuille-fama-french` | Analyse de portefeuille à partir des rendements des 49 portefeuilles industriels (Fama-French) et des facteurs de risque du modèle à 5 facteurs | Individuel |
| `app-python-supermarche` | Tableau de bord interactif des ventes d'un supermarché avec Dash et Plotly : filtres par ville et sexe, indicateurs de ventes et de notation, graphiques par ville, catégorie de produits et semaine | Individuel |
| `series-temporelles` | Travaux sur les processus stochastiques : stationnarité des processus ARMA, simulations et autocorrélogrammes, modèles saisonniers SARMA et SARIMA (simulation, estimation et diagnostics), chocs permanents et transitoires, tests de racine unitaire (Dickey-Fuller, KPSS) sur le PIB des États-Unis, et modélisation de la volatilité réalisée (modèle HAR sur l'indice KOSPI 200) | Amélie Pires, Ikram Abouzayd, Diane Anato, Néné Gallé Ly, Ainara Martínez Pérez |
| `prevision-production-industrielle` | Prévision de la production industrielle de gaz et électricité (2003-2022) : désaisonnalisation, comparaison de modèles sur la base de l'AIC et de l'AICc, évaluation hors échantillon (MSE, R²OOS) et tests de Diebold-Mariano | Diane Anato |
| `classification-phonemes` | Classification de phonèmes à partir de variables latentes : ACP, classification non supervisée (HCPC, k-means), analyse factorielle discriminante (AFD) et PLS-DA avec sélection de variables, validées par validation croisée et courbes ROC | Jules Bouscaud |
| `econometrie-salaires` | Impact des inégalités sur les attentes salariales des étudiants : enquête auprès de 196 étudiants et régression linéaire sur le logarithme du salaire espéré | Saussereau Jameson |



## Structure type d'un dossier

1. **Rapport final (PDF)** : le rendu compilé, prêt à lire.
2. **Code source Quarto (`.qmd`)** : tout le traitement, de l'import des données
   à la production des tableaux et graphiques. Exécuter le document reproduit
   l'intégralité des analyses et le PDF.
3. **Base de données** : les données utilisées par le code (CSV, Excel ou autre).

## Outils et logiciels

R (RStudio, ggplot2, forecast, urca, FactoMineR), Python (pandas, NumPy,
matplotlib), SQL (BigQuery), Stata, Power BI, Quarto pour la reproductibilité.
