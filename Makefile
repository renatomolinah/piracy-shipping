# SHORTCUTS (IN ALL CAPS)
# For content
CONTENT=results/figures_and_tables/

# For data
RAW_DATA=data/raw_data/
PROCESSED_DATA=data/processed/
OUTPUT_DATA=data/output/

# Build targets
all: figures tables
figures:
tables:
panel_data:
processed_data:

# PROCESSED data
$(PROCESSED_DATA)voyages.rds: code/0.0_data_set_up.R $(PROCESSED_DATA)voyage_data_5_v_20250521/*.parquet
		cd $(<D);Rscript $(<F)

$(CONTENT)summary_statistics.tex: code/01_summary.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)

# ANALYSIS
# Gridcell level analysis

# 2.1 Download data from BigQuery
$(PROCESSED_DATA)attacks_and_activity_by_grid.rds: code/2.0_download_attacks_and_activity_by_gridcell.R
		cd $(<D);Rscript $(<F)
# 2.2 Build event study panel
$(PROCESSED_DATA)ev_panel.rds: code/2.1_build_gridcell_event_study_panel.R attacks_and_activity_by_grid.rds
		cd $(<D);Rscript $(<F)
# 2.3 Estimate models
$(OUTPUT_DATA)gridcell_models/gridcell_models.RData: 2.2_gridcell_estimation $(PROCESSED_DATA)ev_panel.rds
		cd $(<D);Rscript $(<F)
# 2.4 Build figures and tables

workflow.png: _workflow/build_dag.sh Makefile
		bash $(<D)/$(<F)

