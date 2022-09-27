
CREATE TABLE IF NOT EXISTS nav_points (
  "name" text primary key not null,
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

CREATE TABLE IF NOT EXISTS tasks (
  "id" int not null primary key generated always as identity,
  "start_point_name" text not null references nav_points ("name"),
  "start_type" text not null,
  "start_radius" numeric not null,
  "finish_point_name" text not null references nav_points ("name"),
  "finish_type" text not null,
  "finish_radius" numeric not null
);

CREATE TABLE IF NOT EXISTS task_turnpoints (
  "task_id" int not null references tasks ("id"),
  "turnpoint_name" text not null references nav_points ("name"),
  "turnpoint_type" text not null,
  "turnpoint_radius" numeric not null,
  "turnpoint_number" int not null,
  primary key ("task_id", "turnpoint_name")
);
