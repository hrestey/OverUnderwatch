create database if not exists db;
use db;

drop table if exists user_standings;
drop table if exists tiebreakers;
drop table if exists standings_teams;
drop table if exists matches;
drop table if exists users;
drop table if exists teams;

create table teams
(id int not null primary key auto_increment,
    name varchar(40),
    wins int not null,
    losses int not null,
    mapdiff int not null);

create table users
(id int not null primary key auto_increment,
    uname varchar(255),
    password varchar(255),
    email varchar(255));

create table matches
(id int not null primary key auto_increment,
    teamA_id int not null references teams(id),
    teamB_id int not null references teams(id),
    winsA int not null,
    winsB int not null,
    key(teamA_id, teamB_id));

create table tiebreakers
(id int not null primary key auto_increment,
    match_id int not null references matches(id),
    winner_id int not null references teams(id));

create table user_standings
(id int not null primary key auto_increment,
    user_id int not null references users(id));

create table standings_teams
(id int not null primary key auto_increment,
    team_id int not null references teams(id),
    userstandings_id int not null references userstandings(id),
    wins int not null,
    losses int not null,
    mapdiff int not null,
    teamrank int not null);
