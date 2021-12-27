psql postgres://postgres:password@localhost/ -c 'drop database ghcnd'
psql postgres://postgres:password@localhost/ -c 'create database ghcnd;'
psql postgres://postgres:password@localhost/ghcnd -f initialize.sql
