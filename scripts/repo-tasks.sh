#!/bin/bash

PARENT_DIR=$HOME/source/orbitalinsight
REPOS="aoi_service_v2 notification sam artifact_service tile_db_utils go_database go_services go_pipelines go_data_viz frontend go_monitoring_ui streamx-controller streamy geoloc_service dsaas mars-server opis"

for R in $REPOS; do
  cd $PARENT_DIR/$R
  echo "**** $R"
  git checkout master
  gcrb chetan/no-autodeploy-staging
done
