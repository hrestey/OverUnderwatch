#!/bin/bash

# Run MYSQL
/usr/bin/mysqld_safe

# Set up DB
mysql -uadmin -padmin db < /usr/sql/sources.sql
