# US Monetary Policy Analysis: A Dynamic Econometrics Approach

## Project Overview
This project was developed as a final assignment for the **Dynamic Econometrics** (Ekonometria Dynamiczna) course. It is now part of my professional portfolio to demonstrate my skills in time-series analysis, econometric modeling, and economic data interpretation.

The study focuses on analyzing the macroeconomic response to unexpected changes (shocks) in the United States' monetary policy. Using monthly data from 1985 to 2024, the project empirically verifies the transmission mechanism of the Federal Reserve (Fed) and its impact on real economic activity (industrial production) and inflation.

## Key Features & Methodology
* **Vector Autoregression (VAR) & VEC Models**: The analysis transitions from a VAR model on levels to a model on increments to ensure dynamic stability. It also incorporates a Vector Error Correction (VEC) model to analyze long-term equilibrium.
* **Cointegration Analysis**: Application of the Johansen test to identify stable long-run relationships between variables.
* **Structural Analysis**: 
    * **Impulse Response Functions (IRF)**: To trace the effects of policy shocks over a 36-month horizon[cite: 521].
    * **Forecast Error Variance Decomposition (FEVD)**: To determine the relative importance of different shocks in explaining variable volatility.
    * **Granger Causality**: To verify the direction of relationships between the monetary and real spheres of the economy.
* **Advanced Diagnostics**: The model includes rigorous testing for normality (Jarque-Bera), autocorrelation (Portmanteau, Edgerton-Shukur), homoskedasticity (ARCH-LM), and parameter stability (OLS-CUSUM).

## Technical Details
* **Language**: The analysis is performed entirely in **R**.
* **Libraries Used**: `vars`, `urca`, `tseries`, `zoo`, `plotrix`.
* **Data Source**: Monthly data retrieved from the **FRED** (Federal Reserve Economic Data) database.
* **Language Note**: Please note that the comprehensive project report (PDF) and all comments within the R script are written in **Polish**.

## Authors
This project was co-authored by:
* **Michał Siarka** 
* **Konrad Bodziony** 
* **Albert Dańko** 
