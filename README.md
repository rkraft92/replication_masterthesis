## Replication-Code for the Masterthesis "BOOSTING STRUCTURED ADDITIVE MODELS IN HIGH-AND LOW-DIMENSIONAL REGRESSION PROBLEMS"

### Requirements:

* Programming-Language: `R`

* Packages: `dplyr`, `mboost`, `caret`, `earth`, `gam`, `MASS`, `glmnet`, `TH.data`



### Figures 2.1/2.2/2.3/3.3:

* *eigenvalues_smoothingspline.R*: simulates a cubic smoothing spline with several smoothing parameters (Figure 2.1) and generates the eigenvalues and vectors of a cubic smoothing spline and its boosted version (Figures 2.2/2.3 and 2.3)



### Figures 3.1/3.2

* *splines_residualplot.R*: generates the model fit and residual fit plots for boosted cubic smoothing splines



### Figure 3.4:

- *toy_boosting_iterations_bias_variance.R*: simulates the squared-bias and variance of boosted cubic smoothing splines as a function of the Boosting iterations
- *toy_boosting_iterations.R*: simulates the MSE for boosted cubic smoothing splines as a function of the Boosting iterations
- *toy_cs_spline_bias_variance.R*: simulates the squared-bias and variance of cubic smoothing splines as a function of the degrees of freedom
- *toy_cs_spline_df.R*: simulates the MSE for cubic smoothing splines as a function of the degrees of freedom
- *toy_mse_bv_plot.R*: generates the MSE decomposition subplot (Figure 3.4)



### Figures 5.1/5.2/B.1/B.2 and Table 5.1

* *42_boost_iter_bias_variance.R*: simulates the squared-bias, variance and MSE for boosted GLMs as a function of the Boosting iterations (uncorrelated case)
* *42_model_boost_iter_mse_bv_plot.R*: generates the plot (Figure 5.1) for *42_boost_iter_bias_variance.R* (uncorrelated case)
* *42_model_coefficients.R*: generates the coefficient paths (Figure 5.2) for one selected Monte Carlo iteration (uncorrelated case)
* *42_model_summary.R*: generates Table 5.1 (uncorrelated case)
* *43_boost_iter_bias_variance.R*:  simulates the MSE for boosted GLMs as a function of the Boosting iterations (correlated case)
* *43_boost_iter_mse_var_bias_plot.R*: generates the plot (Figure B.1) for *43_boost_iter_bias_variance.R* (correlated case)
* *43_model_coefficients.R*: generates the coefficient paths (Figure B.2) for one selected Monte Carlo iteration (correlated case)
* *43_model_summary.R*: generates Table 5.1 (correlated case)
* *v_matrix.R*: function for generating the blockwise correlated design matrix from equation (5.3)



### Figure 5.3 and Table 5.2

* *44_model_beta_coefficients.R*: generates Figure 5.3

* *44_model.R*: generates Table 5.2



### Figure 5.4/5.5/5.6 and Table 5.3:

* *schmid_hothorn_boost_iter_bias_variance.R*: simulates the squared-bias, variance and MSE for boosted GAMs as a function of the Boosting iterations 
* *schmid_hothorn_mse_decomp_plot.R*: generates the plot (Figure 5.4) for *schmid_hothorn_boost_iter_bias_variance.R*

* *schmid_hothorn_df.R*: simulates the MSE distributions as a function of the degrees of freedom for boosted GAMs
* *schmid_hothorn_plot_boxplot.R*: generates Figure 5.6 from *schmid_hothorn_df.R*

* *schmid_hothorn_fit_true_boost_plot.R*: simulates and generates Figure 5.5

* *schmid_hothorn_mse_summary.R*: generates Table 5.3



### Figures 6.1/6.2/6.3/B.3/B.4/B.5 and Table 6.1

- *train_test_split_noise.R*: generates the data for the train and test split

- *garcia_benchmark_results.R*: simulates MSE for Boosting and considered benchmarks for original dataset
- *garcia_noisy_predictors_extern_data.R*: simulates MSE for Boosting and considered benchmarks for regression setup with synthetic covariates

- *garcia_results_table_creation.R*: generates Table 6.1 from *garcia_benchmark_results.R* and *garcia_noisy_predictors_extern_data.R*

* *garcia_mse_development_gam.R*: MSE development of boosted GAMs as a function of the Boosting iterations

* *garcia_mse_dev_plot.R*: generates Figure 6.1 from *garcia_mse_development_gam.R*

* *garcia_partial_effects_gamboost_plot.R*: generates Figure 6.2  for one Monte Carlo iteration

* *garcia_partial_effects_plots.R*: generates Figue B.3/B.4/B.5 for one Monte Carlo iteration

- *garcia_df_base_splits.R*: simulates MSE distribution for boosted GAM as a function of the degrees of freedom

- *garcia_df_boxplot.R*: generates Figure 6.3 from *garcia_df_base_splits.R*



### References

1. Bühlmann, Peter and Bin Yu (2003). “Boosting With the L2 Loss: Regression and Clas- sification”. In: Journal of the American Statistical Association 98, pp. 324–339. 
2. Bühlmann, Peter (2006). “Boosting for high-dimensional linear models”. In: The Annals of Statistics 34.2, pp. 559–583. 
3. Schmid, Matthias and Torsten Hothorn (2008). “Boosting additive models using
   component-wise P-Splines”. In: Computational Statistics & Data Analysis 53, pp. 298–
   311.
4. Mayr, Andreas, Harald Binder, Olaf Gefeller, and Matthias Schmid (2014). “The Evolution
   of Boosting Algorithms. From Machine Learning to Statistical Modelling”. In: Methods
   of information in medicine 53.