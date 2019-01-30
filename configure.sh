#!/bin/bash

composer install --ignore-platform-reqs -d app/

docker network create owlmapnet
# build mysql container image
echo "Building mymysql image:"
docker build -t mymysql $PWD/sql/
# build php/prolog container image
echo "Building php + apache + prolog image:"
docker build -t phppl .

echo "Building container for database from mymysql image:"
docker run -d --name overunderwatchdb --network owlmapnet -p 33060:3306 mymysql
echo "Building container for web server from phppl image:"
docker run -dit --name overunderwatchphppl --network owlmapnet -p 8080:80 -v "$PWD/app":/var/www/html phppl

# use docker exec to run the run-db.sh script in the mysql container
echo "Setting up db in database container"
docker exec -d overunderwatchdb bash /usr/sql/run-db.sh

