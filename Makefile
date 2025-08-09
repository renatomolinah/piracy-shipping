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
all: figures tables
# By type of conent
figures: $(CONTENT)cell_level_event_study_2x2.png
tables: $(CONTENT)summary_statistics.tex $(CONTENT)cell_post_regression.tex
# By pipeline
gridcell_pipeline: $(CONTENT)cell_post_regression.tex $(CONTENT)cell_level_event_study_2x2.png
#trip_pipeline:

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

# Trip level analysis ----------------------------------------------------------
# 3.1 Trip feature regressions
$(PROCESSED_DATA)voyages.rds: code/0.0_data_set_up.R $(PROCESSED_DATA)voyage_data_5_v_20250521/*.parquet
		cd $(<D);Rscript $(<F)
# 3.2 Estimation
#3.2_trip_feature_regressions.R

# 3.3 Tables and figures
#3.3_main_estimation.R

# Costs and emissions ----------------------------------------------------------
#4.1_cost_regressions.R

#4.2_emissions_regressions.R

#4.5_merge_pred.R
