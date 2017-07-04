\connect "holiday_ping";

CREATE TABLE "public"."users" (
  "id" serial PRIMARY KEY,
  "email" character varying(100) UNIQUE NOT NULL,
  "name" character varying(200) NOT NULL,
  "country" character varying(20) NOT NULL,
  "password" character varying(100) NOT NULL,
);

CREATE TABLE channels (
  "id" serial PRIMARY KEY,
  "user" serial REFERENCES users,
  "name" character varying(50) NOT NULL,
  "type" character varying(20) NOT NULL,
  "configuration" jsonb NOT NULL,
  UNIQUE ("user", "name")
);
