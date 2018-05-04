#!/bin/bash

docker network create owlmapnet
# build mysql container image
echo "Building mymysql image:"
docker build -t mymysql $PWD/sql/
# build php/prolog container image
echo "Building php + apache + prolog image:"
docker build -t phppl .

echo "Building container for database from mymysql image:"
docker run -d --name owlmap-db --network owlmapnet -p 33060:3306 mymysql
echo "Building container for web server from phppl image:"
docker run -dit --name owl-phppl --network owlmapnet -p 8080:80 -v "$PWD/app":/var/www/html phppl

# use docker exec to run the run-db.sh script in the mysql container
echo "Setting up db in database container"
docker exec -d owlmap-db bash /usr/sql/run-db.sh

# install and run composer with docker exec in main container
echo "Install and run composer on main container:"
docker exec -d owl-phppl bash /usr/sbin/installcomposer.sh
docker exec -d owl-phppl composer install --ignore-platform-reqs -d /var/www/html/app/
