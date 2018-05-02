FROM php:7.0-apache

RUN apt-get install -y swi-prolog

ADD src /var/www/html
