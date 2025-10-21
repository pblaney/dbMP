#!/usr/bin/env bash
# Execution script to run the dbMP app

echo "Deplying dbMP Dashboard UI ..."
#Rscript app.R user_creds.rds dbmp.sqlite
R -q -e 'shiny::runApp(launch.browser = TRUE)'