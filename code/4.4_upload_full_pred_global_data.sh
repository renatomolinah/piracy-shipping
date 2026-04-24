#!/bin/bash
# Set path
export PROJECT_PATH="/Users/jcvd/GitHub/piracy-shipping/"

# Upload all local csv files to GCS Bucket
gsutil cp "$PROJECT_PATH"data/processed/full_pred_global.csv gs://piracy-emlab-gcp

# Create a table in Big Query, specify the scheme, add a desciption, and name it
bq mk --table \
--schema trip_id:STRING,attacks_7day_num:INTEGER,wind_speed:FLOAT,wind_vector:FLOAT,wave_height:FLOAT,country_pair:STRING,vessel_type:STRING,tonnage_decile:INTEGER,hotspot:STRING,top_route:INTEGER,month:INTEGER,year:INTEGER,p_fuel:FLOAT,np_fuel:FLOAT,p_labor:FLOAT,np_labor:FLOAT,p_total:FLOAT,np_total:FLOAT,p_co2:FLOAT,np_co2:FLOAT,p_nox:FLOAT,np_nox:FLOAT,p_sox:FLOAT,np_sox:FLOAT \
--description "Model prediction of costs with and without pirate encounters" \
emlab-gcp:piracy.full_pred_global_v_20260423

# If I need to delete it, this is the command:
#bq rm -f -t emlab-gcp:piracy.full_pred_global_v_20260423

# Upload from GCS bcket to Big Query table
# We specify the format of the original data, tell it to ignore the first row (headers),
# then specify that NA = NULL, and tell it to replace existing data in the table.
bq load \
--source_format=CSV \
--skip_leading_rows=1 \
--null_marker "NA" \
--replace \
emlab-gcp:piracy.full_pred_global_v_20260423 \
gs://piracy-emlab-gcp/full_pred_global.csv
