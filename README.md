# US Mutual Fund Performance Evaluation & Portfolio Optimization

A comprehensive financial analytics project evaluating the performance of 90 USA mutual funds (July 1963 – July 2019) and constructing optimal portfolios using a range of increasingly sophisticated return/risk models — from simple sample estimates to GARCH-type and Fama-French multi-factor models. Completed as the final project for the *Financial Analytics* course, MSc in Statistics, Athens University of Economics and Business (AUEB).

## Repository Contents

| File | Description |
|---|---|
| `US_FUND_DATA.xlsx` | Raw dataset — monthly returns for 251 mutual funds (`US_Funds` sheet) and 8 explanatory factors (`Factors` sheet: Mkt-RF, SMB, HML, RMW, CMA, MOM, BAB, CAR), July 1963–July 2019. |
| `Project_Description.pdf` | Original assignment brief from Prof. Ioannis Vrontos, outlining Part A (performance evaluation) and Part B (portfolio construction) requirements. |
| `Project_Code_In_R.txt` / `Project_Code.txt` | Full R script implementing the analysis (two copies of the same code, for convenience). |
| `Project_Report.pdf` | Final written report with methodology, figures, tables, and conclusions. |

## Data

- **In-sample (estimation) period**: July 1963 – July 2015
- **Out-of-sample (evaluation) period**: August 2015 – July 2019
- 90 of the 251 available funds were analyzed, with missing values handled via custom covariance calculations (funds start/end at different times).

## Part A — Performance Evaluation

Equally weighted portfolios were built from the top 20% (18) of 90 funds, ranked separately under four performance measures:
- **Sharpe Ratio**, **Treynor Ratio**, **Sortino Ratio**
- **Jensen's Alpha**, estimated via:
  - Single Factor Model (CAPM, S&P 500 as market factor)
  - Multiple Regression Model (stepwise factor selection)
  - Multiple Regression + **GARCH(1,1)**
  - Multiple Regression + **GARCH(2,2)** and **EGARCH(1,1)** (alternative approaches capturing volatility clustering and, for EGARCH, the leverage effect)

**Result**: GARCH(1,1), GARCH(2,2), and EGARCH(1,1) models produced the most consistent, highest Jensen's Alpha values and strongest cumulative out-of-sample returns, outperforming the simpler Single Index and Multiple Regression models.

## Part B — Portfolio Construction

Both **Minimum Variance** and **Mean-Variance** optimal portfolios (solved via quadratic programming) were constructed using the mean vector and covariance matrix estimated from:

1. Sample estimates (mean/covariance of historical returns)
2. Single Index Model
3. Multivariate Multiple Regression
4. Constant Conditional Correlation (CCC) model
5. Fama-French 3-Factor model (Mkt-RF, SMB, HML)
6. Fama-French 4-Factor model (adds Momentum)

Each approach was evaluated out-of-sample (8/2015–7/2019) using realized return, portfolio risk, cumulative returns, and Conditional Sharpe Ratio.

**Result**: Across every modeling approach, the **Minimum Variance Portfolio** consistently outperformed the Mean-Variance Portfolio out-of-sample, achieving higher cumulative returns and Conditional Sharpe Ratios with substantially lower risk. The CCC model produced the best risk-adjusted Minimum Variance portfolio overall (Conditional Sharpe Ratio ≈ 1.05).

## Key Takeaways

- More sophisticated models that incorporate multiple risk factors, volatility clustering, and conditional correlation structures deliver better risk-adjusted performance than simple sample-based estimates.
- Minimum Variance portfolios outperformed Mean-Variance portfolios out-of-sample under every modeling framework tested.
- GARCH-family models were the strongest performers for Jensen's Alpha-based fund selection in Part A.

## Requirements

Built with R, using packages including:

```r
quadprog, rugarch, tseries, PerformanceAnalytics, readxl
```

## Usage

1. Clone the repository.
2. Open `Project_Code_In_R.txt` (or `Project_Code.txt`) in R/RStudio and update the file path for `US_FUND_DATA.xlsx`.
3. Run the script to reproduce the performance evaluation (Part A) and portfolio construction/optimization (Part B) results.

## Author

Konstantinos Grammenos — MSc in Statistics, AUEB
Supervisor: Prof. Ioannis Vrontos
