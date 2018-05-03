#!/bin/bash

# Set up DB
mysql -uadmin -padmin db < /usr/sql/source.sql
