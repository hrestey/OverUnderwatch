#!/bin/bash

docker network create owlmapnet
# build mysql container image
docker build -t mymysql $PWD/sql/
# build php/prolog container image
docker build -t phppl

docker run -d --name owlmap-db --network owlmapnet -p 33060:3306 mymysql
docker run -dit --name owl-phppl --network owlmapnet -p 8080:80 -v "$PWD/src":/var/www/html phppl

# use docker exec to run the run-db.sh script in the mysql container
docker exec -d owlmap-db bash /usr/sql/run-db.sh
