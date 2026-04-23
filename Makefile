# --- Paths ---
CONTENT   = results/figures_and_tables/
RAW_DATA  = data/raw_data/
PROCESSED = data/processed/
OUTPUT    = data/output/
STAMPS    = .stamps/

# --- Pipelines ---
.PHONY: all gridcell_pipeline trip_pipeline cost_pipeline clean log

all: gridcell_pipeline trip_pipeline cost_pipeline

gridcell_pipeline: $(STAMPS)2.4_gridcell_figures $(STAMPS)2.4c_DC_did_multiplegt
trip_pipeline:     $(STAMPS)3.1_trip_features $(STAMPS)3.2_main_estimation
cost_pipeline:     $(STAMPS)4.5_counterfactual_maps $(STAMPS)4.6_counterfactual_tables

# --- DAG ---
workflow.png: _workflow/build_dag.sh Makefile
	bash $(<D)/$(<F)

# --- 0. Data setup ---
$(PROCESSED)voyages.rds: code/0.0_data_set_up.R $(PROCESSED)voyage_data_5_v_20250521/*.parquet
	@echo "--- Data setup ---"
	cd $(<D);Rscript $(<F)

# --- 1. Summary statistics ---
$(CONTENT)summary_statistics.tex: code/1.0_summary.R $(PROCESSED)voyages.rds
	@echo "--- Summary statistics ---"
	cd $(<D);Rscript $(<F)

# --- 2.1 Download gridcell data ---
# Outputs: attacks_and_activity_by_grid_0_1.rds, _0_5.rds, _1.rds
$(STAMPS)2.1_gridcell_download: code/2.1_gridcell_download_attacks_and_activity.R
	@mkdir -p $(STAMPS)
	@echo "--- Downloading gridcell data ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(PROCESSED)attacks_and_activity_by_grid.rds: $(STAMPS)2.1_gridcell_download ;
$(PROCESSED)attacks_and_activity_by_grid_0_5.rds: $(STAMPS)2.1_gridcell_download ;
$(PROCESSED)attacks_and_activity_by_grid_0_1.rds: $(STAMPS)2.1_gridcell_download ;

# --- 2.2 Build event study panel ---
$(PROCESSED)ev_panel.rds: code/2.2_gridcell_build_event_study_panel.R $(PROCESSED)attacks_and_activity_by_grid.rds
	@echo "--- Building event study panel ---"
	cd $(<D);Rscript $(<F)

# --- 2.3 Gridcell estimation ---
$(OUTPUT)gridcell_models.RData: code/2.3_gridcell_estimation.R $(PROCESSED)ev_panel.rds
	@echo "--- Gridcell estimation ---"
	cd $(<D);Rscript $(<F)

# --- 2.4 Gridcell figures and tables ---
# Outputs: cell_post_regression.tex, cell_level_event_study_2x2.png
$(STAMPS)2.4_gridcell_figures: code/2.4_gridcell_tables_and_figures.R $(OUTPUT)gridcell_models.RData
	@mkdir -p $(STAMPS)
	@echo "--- Gridcell figures and tables ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(CONTENT)cell_post_regression.tex: $(STAMPS)2.4_gridcell_figures ;
$(CONTENT)cell_level_event_study_2x2.png: $(STAMPS)2.4_gridcell_figures ;

# --- 2.4c De Chaisemartin DiD ---
# Outputs: grid_level_event_study_DC_did_multiplegt_dyn_0_5.png, grid_level_did_multiplegt_dyn_summary.tex
$(STAMPS)2.4c_DC_did_multiplegt: code/2.4c_DC_gridcell_did_multiplegt_dyn.R $(PROCESSED)attacks_and_activity_by_grid_0_5.rds
	@mkdir -p $(STAMPS)
	@echo "--- De Chaisemartin DiD ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(CONTENT)grid_level_event_study_DC_did_multiplegt_dyn_0_5.png: $(STAMPS)2.4c_DC_did_multiplegt ;
$(CONTENT)grid_level_did_multiplegt_dyn_summary.tex: $(STAMPS)2.4c_DC_did_multiplegt ;

# --- 3.1 Trip feature regressions ---
# Outputs: spec_distance.tex, spec_time.tex, spec_speed.tex
$(STAMPS)3.1_trip_features: code/3.1_trip_feature_regressions.R $(PROCESSED)voyages.rds
	@mkdir -p $(STAMPS)
	@echo "--- Trip feature regressions ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(CONTENT)spec_distance.tex: $(STAMPS)3.1_trip_features ;
$(CONTENT)spec_time.tex: $(STAMPS)3.1_trip_features ;
$(CONTENT)spec_speed.tex: $(STAMPS)3.1_trip_features ;

# --- 3.2 Main estimation ---
# Outputs: all_features.png, feature_coefficients.rds, feature_coefficients_clean.rds
$(STAMPS)3.2_main_estimation: code/3.2_main_estimation.R $(PROCESSED)voyages.rds
	@mkdir -p $(STAMPS)
	@echo "--- Main estimation ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(CONTENT)all_features.png: $(STAMPS)3.2_main_estimation ;
$(OUTPUT)feature_coefficients.rds: $(STAMPS)3.2_main_estimation ;
$(OUTPUT)feature_coefficients_clean.rds: $(STAMPS)3.2_main_estimation ;

# --- 4.1 Cost regressions ---
# Outputs: cost_pred_global.rds, cost.tex, spec_fuel.tex, spec_labor.tex, spec_total.tex
$(STAMPS)4.1_cost: code/4.1_cost_regressions.R $(PROCESSED)voyages.rds
	@mkdir -p $(STAMPS)
	@echo "--- Cost regressions ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(PROCESSED)cost_pred_global.rds: $(STAMPS)4.1_cost ;
$(CONTENT)cost.tex: $(STAMPS)4.1_cost ;
$(CONTENT)spec_fuel.tex: $(STAMPS)4.1_cost ;
$(CONTENT)spec_labor.tex: $(STAMPS)4.1_cost ;
$(CONTENT)spec_total.tex: $(STAMPS)4.1_cost ;

# --- 4.2 Emissions regressions ---
# Outputs: emissions_pred_global.rds, emissions.tex, spec_co2.tex, spec_nox.tex, spec_sox.tex
$(STAMPS)4.2_emissions: code/4.2_emissions_regressions.R $(PROCESSED)voyages.rds
	@mkdir -p $(STAMPS)
	@echo "--- Emissions regressions ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(PROCESSED)emissions_pred_global.rds: $(STAMPS)4.2_emissions ;
$(CONTENT)emissions.tex: $(STAMPS)4.2_emissions ;
$(CONTENT)spec_co2.tex: $(STAMPS)4.2_emissions ;
$(CONTENT)spec_nox.tex: $(STAMPS)4.2_emissions ;
$(CONTENT)spec_sox.tex: $(STAMPS)4.2_emissions ;

# --- 4.3 Merge predictions ---
# Outputs: full_pred_global.rds, full_pred_global.csv
$(STAMPS)4.3_merge: code/4.3_merge_pred.R $(PROCESSED)cost_pred_global.rds $(PROCESSED)emissions_pred_global.rds
	@mkdir -p $(STAMPS)
	@echo "--- Merging predictions ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(PROCESSED)full_pred_global.rds: $(STAMPS)4.3_merge ;
$(PROCESSED)full_pred_global.csv: $(STAMPS)4.3_merge ;

# --- 4.4 Upload to BigQuery ---
$(STAMPS)4.4_bigquery_upload: code/4.4_upload_full_pred_global_data.sh $(PROCESSED)full_pred_global.csv
	@mkdir -p $(STAMPS)
	@echo "--- Uploading to BigQuery ---"
	cd $(<D);bash $(<F)
	@touch $@

# --- 4.5 Counterfactual maps ---
# Outputs: counterfactual_maps.png, total_map.png
$(STAMPS)4.5_counterfactual_maps: code/4.5_counterfactual_maps.R $(STAMPS)4.4_bigquery_upload
	@mkdir -p $(STAMPS)
	@echo "--- Counterfactual maps ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(CONTENT)counterfactual_maps.png: $(STAMPS)4.5_counterfactual_maps ;
$(CONTENT)total_map.png: $(STAMPS)4.5_counterfactual_maps ;

# --- 4.6 Counterfactual tables ---
# Outputs: counterfactual_costs.tex, counterfactual_emissions.tex
$(STAMPS)4.6_counterfactual_tables: code/4.6_counterfactual_tables.R $(STAMPS)4.4_bigquery_upload
	@mkdir -p $(STAMPS)
	@echo "--- Counterfactual tables ---"
	cd $(<D);Rscript $(<F)
	@touch $@

$(CONTENT)counterfactual_costs.tex: $(STAMPS)4.6_counterfactual_tables ;
$(CONTENT)counterfactual_emissions.tex: $(STAMPS)4.6_counterfactual_tables ;

# --- Log ---
log:
	$(MAKE) all 2>&1 | tee make.log

# --- Clean ---
clean:
	rm -rf $(STAMPS)
