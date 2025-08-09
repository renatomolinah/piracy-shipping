# SHORTCUTS (IN ALL CAPS)
# For content
CONTENT=results/figures_and_tables/

# For data
RAW_DATA=data/raw_data/
PROCESSED_DATA=data/processed/
OUTPUT_DATA=data/output/

all: figures tables
# Aliases
# By type of conent
figures: $(CONTENT)cell_level_event_study_2x2.png
tables: $(CONTENT)summary_statistics.tex $(CONTENT)cell_post_regression.tex
# By pipeline
gridcell_pipeline: $(CONTENT)cell_post_regression.tex $(CONTENT)cell_level_event_study_2x2.png


# PROCESSED data
$(PROCESSED_DATA)voyages.rds: code/0.0_data_set_up.R $(PROCESSED_DATA)voyage_data_5_v_20250521/*.parquet
		cd $(<D);Rscript $(<F)

$(CONTENT)summary_statistics.tex: code/1.0_summary.R $(PROCESSED_DATA)voyages.rds
		cd $(<D);Rscript $(<F)

# ANALYSIS
# Gridcell level analysis
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
$(CONTENT)cell_post_regression.tex: code/2.4_gridcell_tables_and_figures.R $(OUTPUT_DATA)gridcell_models.RData
		cd $(<D);Rscript $(<F)
$(CONTENT)cell_level_event_study_2x2.png: code/2.4_gridcell_tables_and_figures.R $(OUTPUT_DATA)gridcell_models.RData
		cd $(<D);Rscript $(<F)

workflow.png: _workflow/build_dag.sh Makefile
		bash $(<D)/$(<F)

