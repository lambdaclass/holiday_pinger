\connect "holiday_ping";

CREATE TABLE "public"."users" (
  "id" serial PRIMARY KEY,
  "email" character varying(100) UNIQUE NOT NULL,
  "name" character varying(200) NOT NULL,
  "password" character varying(100),
  "auth_type" character varying(20),
  "verified" boolean NOT NULL DEFAULT FALSE,
  "verification_code" character varying(30),
  "verification_sent_at" timestamp without time zone,
  "password_reset_code" character varying(30),
  "password_reset_sent_at" timestamp without time zone
);

CREATE TABLE channels (
  "id" serial PRIMARY KEY,
  "user" integer REFERENCES users ON DELETE CASCADE,
  "name" character varying(50) NOT NULL,
  "type" character varying(20) NOT NULL,
  "configuration" jsonb NOT NULL,
  "same_day" boolean NOT NULL DEFAULT FALSE,
  "days_before" smallint CHECK ("days_before" > 0),
  UNIQUE ("user", "name")
);

CREATE TABLE holidays (
  "id" serial PRIMARY KEY,
  "country" character varying(20) NOT NULL,
  "date" date NOT NULL,
  "name" character varying(50) NOT NULL,
  UNIQUE ("country", "date")
);

CREATE TABLE channel_holidays (
  "id" serial PRIMARY KEY,
  "channel" integer REFERENCES channels ON DELETE CASCADE,
  "date" date NOT NULL,
  "name" character varying(50) NOT NULL,
  UNIQUE ("channel", "date")
);

CREATE TABLE sent_reminders (
  "id" serial PRIMARY KEY,
  "user" integer REFERENCES users ON DELETE CASCADE,
  "channel" integer REFERENCES channels ON DELETE SET NULL,
  "channel_type" character varying(20) NOT NULL,
  "target" character varying(100),
  "test" boolean NOT NULL DEFAULT FALSE,
  "timestamp" timestamp without time zone default (now() at time zone 'utc')
);

INSERT INTO "holidays" ("country", "date", "name") VALUES
('argentina', make_date(date_part('year', now())::int, 1, 1), 'New year'),
('argentina', make_date(date_part('year', now())::int, 2, 27), 'Carnival'),
('argentina', make_date(date_part('year', now())::int, 2, 28), 'Carnival'),
('argentina', make_date(date_part('year', now())::int, 3, 24), ''),
('argentina', make_date(date_part('year', now())::int, 4, 2), ''),
('argentina', make_date(date_part('year', now())::int, 4, 13), ''),
('argentina', make_date(date_part('year', now())::int, 4, 14), ''),
('argentina', make_date(date_part('year', now())::int, 5, 1), ''),
('argentina', make_date(date_part('year', now())::int, 5, 25), ''),
('argentina', make_date(date_part('year', now())::int, 6, 17), ''),
('argentina', make_date(date_part('year', now())::int, 6, 20), ''),
('argentina', make_date(date_part('year', now())::int, 7, 9), 'Independence day'),
('argentina', make_date(date_part('year', now())::int, 11, 20), ''),
('argentina', make_date(date_part('year', now())::int, 12, 8), ''),
('argentina', make_date(date_part('year', now())::int, 12, 25), 'Christmas'),

('united states', make_date(date_part('year', now())::int, 1, 1), 'New years day'),
('united states', make_date(date_part('year', now())::int, 1, 16), 'Martin Luther King Jr. Day'),
('united states', make_date(date_part('year', now())::int, 2, 20), 'Presidents'' day'),
('united states', make_date(date_part('year', now())::int, 5, 29), 'Memorial day'),
('united states', make_date(date_part('year', now())::int, 7, 4), 'Independence day'),
('united states', make_date(date_part('year', now())::int, 9, 4), 'Labor day'),
('united states', make_date(date_part('year', now())::int, 11, 10), 'Veterans day'),
('united states', make_date(date_part('year', now())::int, 11, 23), 'Thanksgiving'),
('united states', make_date(date_part('year', now())::int, 12, 25), 'Christmas')
;
