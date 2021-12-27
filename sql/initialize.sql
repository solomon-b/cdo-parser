create database ghcnd;

CREATE TABLE daily_records (
  id SERIAL PRIMARY KEY,
  station_id CHAR(11) NOT NULL,
  date DATE NOT NULL,
  element CHAR(4) NOT NULL,
  value INTEGER NOT NULL,
  mflag CHAR(1),
  qflag CHAR(1),
  sflag CHAR(1)
);

CREATE INDEX daily_records_station_id_index ON daily_records (station_id);
CREATE INDEX daily_records_date_index ON daily_records (date);

CREATE SEQUENCE daily_records_id_sequence
  start 1
  increment 1;

CREATE TABLE stations (
  id serial PRIMARY KEY,
  station_id CHAR(11) NOT NULL,
  gps POINT NOT NULL,
  elevation INTEGER NOT NULL,
  state CHAR(2) NOT NULL,
  name VARCHAR(30) NOT NULL,
  gsn_flag CHAR(3),
  hcn_crn_flag CHAR(3),
  wmo_id CHAR(4)
);

CREATE INDEX stations_station_id_index ON stations (station_id);

SELECT
  *
FROM
  daily_records;
