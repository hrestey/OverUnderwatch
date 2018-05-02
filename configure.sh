#!/bin/bash

docker network create webapps
# build mysql container image
# build php/prolog container image

docker run -d --name owlmap-db --network webapps -p 33060:3306 -e MYSQL_ROOT_PASSWORD=taprootofthetree mysql
docker run -dit --name owl-phppl --network webapps -p 8080:80 -v "$PWD/src":/var/www/html swipl-php
