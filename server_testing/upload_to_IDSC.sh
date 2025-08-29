# data
scp -C \
processed_data/attacks_and_activity_by_grid.rds \
processed_data/daily_attacks_and_activity_for_event_study.rds \
jxv893@pegasusdev.ccs.miami.edu:./piracy/processed_data


# scripts
scp -C \
test.R \
r/event_port_port.R \
jxv893@pegasusdev.ccs.miami.edu:./piracy/r


# Bash
scp -C \
test_gridcell_fixest.sh \
run_route_DIDmultiplegtDYN.sh \
jxv893@pegasusdev.ccs.miami.edu:./piracy/shell

scp -C jxv893@pegasusdev.ccs.miami.edu:./piracy/output_data/dist_port_to_port.rds download_dist_port_to_port.rds
