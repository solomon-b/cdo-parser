CREATE TABLE stations (
  station_id CHAR(11) PRIMARY KEY,
  gps POINT NOT NULL,
  elevation INTEGER NOT NULL,
  state CHAR(2),
  name VARCHAR(30) NOT NULL,
  gsn_flag CHAR(3),
  hcn_crn_flag CHAR(3),
  wmo_id CHAR(5)
);

CREATE INDEX stations_station_id_index ON stations (station_id);

CREATE TABLE daily_records (
  station_id CHAR(11) NOT NULL,
  date DATE NOT NULL,
  element CHAR(4) NOT NULL,
  value INTEGER NOT NULL,
  mflag CHAR(1),
  qflag CHAR(1),
  sflag CHAR(1),
  PRIMARY KEY (station_id, date, element),
  FOREIGN KEY(station_id) 
    REFERENCES stations(station_id)
    ON DELETE CASCADE
);

CREATE INDEX daily_records_station_id_index ON daily_records (station_id);
CREATE INDEX daily_records_date_index ON daily_records (date);
