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
  "reminder_time" time NOT NULL,
  "reminder_timezone" text NOT NULL,
  "reminder_days_before" integer[],
  UNIQUE ("user", "name")
);

CREATE TABLE holidays (
  "id" serial PRIMARY KEY,
  "country" character varying(20) NOT NULL,
  "date" date NOT NULL,
  "name" text NOT NULL,
  UNIQUE ("country", "date")
);

CREATE TABLE channel_holidays (
  "id" serial PRIMARY KEY,
  "channel" integer REFERENCES channels ON DELETE CASCADE,
  "date" date NOT NULL,
  "name" text  NOT NULL,
  UNIQUE ("channel", "date")
);

CREATE TABLE scheduled_reminders (
  "id" serial PRIMARY KEY,
  "holiday" integer REFERENCES "channel_holidays" ON DELETE CASCADE,
  "send_at" timestamp with time zone
);

CREATE TABLE sent_reminders_log (
  "id" serial PRIMARY KEY,
  "user" integer REFERENCES users ON DELETE CASCADE,
  "channel" integer REFERENCES channels ON DELETE SET NULL,
  "channel_type" character varying(20) NOT NULL,
  "target" character varying(100),
  "test" boolean NOT NULL DEFAULT FALSE,
  "timestamp" timestamp without time zone default (now() at time zone 'utc')
);

INSERT INTO "holidays" ("country", "date", "name") VALUES

-- Argentina https://www.infobae.com/feriados-argentina/
('argentina', make_date(2019, 11, 18), 'Día de la Soberanía Nacional'),
('argentina', make_date(2019, 12, 8), 'Día de la Virgen'),
('argentina', make_date(2019, 12, 25), 'Navidad'),
('argentina', make_date(2020, 1, 1), 'Año Nuevo'),
('argentina', make_date(2020, 2, 24), 'Carnaval'),
('argentina', make_date(2020, 2, 25), 'Carnaval'),
('argentina', make_date(2020, 3, 24), 'Día Nacional de la Memoria por la Verdad y la Justicia'),
('argentina', make_date(2020, 4, 2), 'Día del Veterano y de los Caídos en la Guerra de Malvinas'),
('argentina', make_date(2020, 4, 10), 'Viernes Santo'),
('argentina', make_date(2020, 5, 1), 'Día del Trabajador'),
('argentina', make_date(2020, 5, 25), 'Día de la Revolución de Mayo'),
('argentina', make_date(2020, 6, 15), 'Paso a la inmortalidad del General Martín de Güemes'),
('argentina', make_date(2020, 6, 20), 'Paso a la inmortalidad del General Manuel Belgrano'),
('argentina', make_date(2020, 7, 9), 'Día de la Independencia'),
('argentina', make_date(2020, 8, 17), 'Paso a la Inmortalidad del General San Martín'),
('argentina', make_date(2020, 10, 12), 'Día del Respeto por la Diversidad Cultural.'),
('argentina', make_date(2020, 11, 23), 'Día de la Soberanía Nacional'),
('argentina', make_date(2020, 12, 8), 'Día de la Virgen'),
('argentina', make_date(2020, 12, 25), 'Navidad'),

-- United States https://www.calendarr.com/united-states/calendar-2020/
('united states', make_date(2019, 11, 11), 'Veterans day'),
('united states', make_date(2019, 11, 28), 'Thanksgiving'),
('united states', make_date(2019, 12, 25), 'Christmas'),
('united states', make_date(2020, 1, 1), 'New years day'),
('united states', make_date(2020, 1, 20), 'Martin Luther King Jr. Day'),
('united states', make_date(2020, 2, 17), 'Presidents'' day'),
('united states', make_date(2020, 4, 10), 'Good Friday'),
('united states', make_date(2020, 5, 25), 'Memorial day'),
('united states', make_date(2020, 7, 4), 'Independence day'),
('united states', make_date(2020, 9, 7), 'Labor day'),
('united states', make_date(2020, 10, 12), 'Columbus Day'),
('united states', make_date(2020, 11, 11), 'Veterans day'),
('united states', make_date(2020, 11, 26), 'Thanksgiving'),
('united states', make_date(2020, 12, 25), 'Christmas')
;
