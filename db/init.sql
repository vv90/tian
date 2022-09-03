
CREATE TABLE IF NOT EXISTS nav_points (
  "id" int not null primary key generated always as identity,
  "name" text not null,
  "code" text not null,
  "country" text null,
  "lat" numeric not null,
  "lon" numeric not null,
  "elev" numeric not null,
  "style" text not null,
  "rwdir" int null,
  "rwlen" numeric null,
  "freq" text null,
  "descr" text not null
);

CREATE INDEX IF NOT EXISTS "nav_points_name" ON nav_points ("name");