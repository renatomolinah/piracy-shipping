# SHORTCUTS (IN ALL CAPS)
# For content
CONTENT=results/figures_and_tables/

# For data
RAW_DATA=data/raw_data/
PROCESSED_DATA=data/processed/
OUTPUT_DATA=data/output/

# ==============================================================================
# ALIASES
# ==============================================================================
all: gridcell_pipeline trip_pipeline cost_pipeline
# By pipeline
gridcell_pipeline: $(CONTENT)cell_post_regression.tex $(CONTENT)cell_level_event_study_2x2.png $(CONTENT)grid_level_event_study_multiple_gt_dyn.pdf
trip_pipeline: $(CONTENT)all_features.png $(PROCESSED_DATA)voyages.rds $(CONTENT)spec_distance.tex $(CONTENT)spec_speed.tex
cost_pipeline: $(CONTENT)counterfactual_maps.png $(CONTENT)total_map.png $(CONTENT)counterfactual_costs.tex $(CONTENT)counterfactual_emissions.tex

# DAG
workflow.png: _workflow/build_dag.sh Makefile
		bash $(<D)/$(<F)

# ==============================================================================
# 1) DESCRIPTIVE FIGURES AND TABLES
# ==============================================================================

# Summary stats tables ---------------------------------------------------------
$(CONTENT)summary_statistics.tex: code/1.0_summary.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)

# ==============================================================================
# 2) ANALYSIS AND RESULTS
# ==============================================================================

# Gridcell level analysis ------------------------------------------------------
# 2.1 Download data from BigQuery
$(PROCESSED_DATA)attacks_and_activity_by_grid.rds: code/2.1_gridcell_download_attacks_and_activity.R
		cd $(<D);Rscript $(<F)
# 2.2 Build event study panel
$(PROCESSED_DATA)ev_panel.rds: code/2.2_gridcell_build_event_study_panel.R $(PROCESSED_DATA)attacks_and_activity_by_grid.rds
		cd $(<D);Rscript $(<F)
# 2.3 Estimate models
$(OUTPUT_DATA)gridcell_models.RData: code/2.3_gridcell_estimation.R $(PROCESSED_DATA)ev_panel.rds
		cd $(<D);Rscript $(<F)
# 2.4 Build figures and tables
## Regression table
$(CONTENT)cell_post_regression.tex: code/2.4_gridcell_tables_and_figures.R $(OUTPUT_DATA)gridcell_models.RData
		cd $(<D);Rscript $(<F)
## Event-study plot
$(CONTENT)cell_level_event_study_2x2.png: code/2.4_gridcell_tables_and_figures.R $(OUTPUT_DATA)gridcell_models.RData
		cd $(<D);Rscript $(<F)
## A la clement
$(CONTENT)grid_level_event_study_multiple_gt_dyn.pdf: code/2.5b_gridcell_estimation_a_la_Clement.R $(PROCESSED_DATA)attacks_and_activity_by_grid.rds
		cd $(<D);Rscript $(<F)

# Trip level analysis ----------------------------------------------------------
# 3.1 Trip feature regressions
$(PROCESSED_DATA)voyages.rds: code/0.0_data_set_up.R $(PROCESSED_DATA)voyage_data_5_v_20250521/*.parquet
		cd $(<D);Rscript $(<F)
#3.2_trip_feature_regressions.R
$(CONTENT)spec_distance.tex: code/3.1_trip_feature_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_time.tex: code/3.1_trip_feature_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_speed.tex: code/3.1_trip_feature_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
#3.3_main_estimation.R
$(OUTPUT_DATA)feature_coefficients.rds: code/3.2_main_estimation.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(OUTPUT_DATA)feature_coefficients_clean.rds: code/3.2_main_estimation.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)all_features.png: code/3.2_main_estimation.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)

# Costs and emissions ----------------------------------------------------------
# Costs
$(PROCESSED_DATA)cost_pred_global.rds: code/4.1_cost_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)cost.tex: code/4.1_cost_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_fuel.tex: code/4.1_cost_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_labor.tex: code/4.1_cost_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_total.tex: code/4.1_cost_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)

# Emissions
$(PROCESSED_DATA)emissions_pred_global.rds: code/4.2_emissions_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)emissions.tex: code/4.2_emissions_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_co2.tex: code/4.2_emissions_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_nox.tex: code/4.2_emissions_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)
$(CONTENT)spec_sox.tex: code/4.2_emissions_regressions.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)

# Combine
$(PROCESSED_DATA)full_pred_global.rds: code/4.3_merge_pred.R $(PROCESSED_DATA)emissions_pred_global.rds $(PROCESSED_DATA)cost_pred_global.rds
		cd $(<D);Rscript $(<F)

# Upload to BigQuery
full_pred_global_bigquery: code/4.4_upload_full_pred_global_data.sh $(PROCESSED_DATA)full_pred_global.rds
		$(<D);bash $(<F)

# Make figures and tables
$(CONTENT)counterfactual_maps.png: code/4.5_counterfactual_maps.R full_pred_global_bigquery
		cd $(<D);Rscript $(<F)
$(CONTENT)total_map.png: code/4.5_counterfactual_maps.R full_pred_global_bigquery
		cd $(<D);Rscript $(<F)
$(CONTENT)counterfactual_costs.tex: code/4.6_counterfactual_tables.R full_pred_global_bigquery
		cd $(<D);Rscript $(<F)
$(CONTENT)counterfactual_emissions.tex: code/4.6_counterfactual_tables.R full_pred_global_bigquery
		cd $(<D);Rscript $(<F)
