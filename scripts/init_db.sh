#!/bin/bash
# This script is supposed to create the database things for this project

psql postgres <<EOF
\x
CREATE DATABASE minions OWNER postgres;
EOF
