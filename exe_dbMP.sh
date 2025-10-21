#!/usr/bin/env bash
# Execution script to run the dbMP app

echo "Deplying dbMP Dashboard UI ..."
R -q -e 'shiny::runApp(launch.browser = TRUE)'