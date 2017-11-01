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
('argentina', make_date(2017, 11, 20), 'Día de la Soberanía Nacional'),
('argentina', make_date(2017, 12, 8), 'Día de la Virgen'),
('argentina', make_date(2017, 12, 25), 'Navidad'),
('argentina', make_date(2018, 1, 1), 'Año Nuevo'),
('argentina', make_date(2018, 2, 12), 'Carnaval'),
('argentina', make_date(2018, 2, 13), 'Carnaval'),
('argentina', make_date(2018, 3, 24), 'Día Nacional de la Memoria por la Verdad y la Justicia'),
('argentina', make_date(2018, 3, 30), 'Viernes Santo'),
('argentina', make_date(2018, 4, 2), 'Día del Veterano y de los Caídos en la Guerra de Malvinas'),
('argentina', make_date(2018, 5, 1), 'Día del Trabajador'),
('argentina', make_date(2018, 5, 25), 'Día de la Revolución de Mayo'),
('argentina', make_date(2018, 6, 17), 'Paso a la inmortalidad del General Martín de Güemes'),
('argentina', make_date(2018, 6, 20), 'Paso a la inmortalidad del General Manuel Belgrano'),
('argentina', make_date(2018, 7, 9), 'Día de la Independencia'),
('argentina', make_date(2018, 8, 20), 'Paso a la Inmortalidad del General San Martín'),
('argentina', make_date(2018, 10, 15), 'Día del Respeto por la Diversidad Cultural.'),
('argentina', make_date(2018, 11, 19), 'Día de la Soberanía Nacional'),
('argentina', make_date(2018, 12, 8), 'Día de la Virgen'),
('argentina', make_date(2018, 12, 25), 'Navidad'),

-- Brazil https://www.calendarr.com/brasil/calendario-2018/
('brazil', make_date(2017, 11, 2), 'Finados'),
('brazil', make_date(2017, 11, 15), 'Proclamação da República'),
('brazil', make_date(2017, 12, 25), 'Natal'),
('brazil', make_date(2018, 1, 1), 'Ano Nuevo'),
('brazil', make_date(2018, 2, 13), 'Carnaval'),
('brazil', make_date(2018, 3, 30), 'Sexta-feira da Paixão'),
('brazil', make_date(2018, 5, 1), 'Dia do Trabalhador'),
('brazil', make_date(2018, 5, 30), 'Corpus Christi'),
('brazil', make_date(2018, 9, 7), 'Dia da Independência do Brasil'),
('brazil', make_date(2018, 10, 12), 'Nossa Senhora Aparecida'),
('brazil', make_date(2018, 11, 2), 'Finados'),
('brazil', make_date(2018, 11, 15), 'Proclamação da República'),
('brazil', make_date(2018, 12, 25), 'Natal'),

-- Canada
('canada', make_date(2017, 11, 11), 'Remembrance Day'),
('canada', make_date(2017, 12, 25), 'Christmas'),
('canada', make_date(2018, 1, 1), 'New Year''s Day'),
('canada', make_date(2018, 3, 30), 'Good Friday'),
('canada', make_date(2018, 5, 21), 'Victoria Day'),
('canada', make_date(2018, 7, 2), 'Canada Day'),
('canada', make_date(2018, 9, 3), 'Labour Day'),
('canada', make_date(2018, 10, 8), 'Thanksgiving Day'),
('canada', make_date(2018, 11, 12), 'Remembrance Day'),
('canada', make_date(2018, 12, 25), 'Christmas'),

-- India https://www.timeanddate.com/holidays/india/2018#!hol=9
('india', make_date(2017, 11, 4), 'Guru Nanak Jayanti'),
('india', make_date(2017, 12, 2), 'Milad un-Nabi/Id-e-Milad'),
('india', make_date(2017, 12, 25), 'Christmas'),
('india', make_date(2018, 1, 26), 'Republic Day'),
('india', make_date(2018, 2, 13), 'Maha Shivaratri/Shivaratri'),
('india', make_date(2018, 3, 29), 'Mahavir Jayanti'),
('india', make_date(2018, 3, 30), 'Good Friday'),
('india', make_date(2018, 4, 30), 'Buddha Purnima/Vesak'),
('india', make_date(2018, 8, 15), 'Independence Day'),
('india', make_date(2018, 8, 22), 'Bakr Id/Eid ul-Adha'),
('india', make_date(2018, 9, 3), 'Janmashtami'),
('india', make_date(2018, 9, 21), 'Muharram/Ashura'),
('india', make_date(2018, 10, 2), 'Mahatma Gandhi Jayanti'),
('india', make_date(2018, 10, 19), 'Dussehra (Maha Navami)'),
('india', make_date(2018, 11, 7), 'Diwali/Deepavali'),
('india', make_date(2018, 11, 21), 'Milad un-Nabi/Id-e-Milad'),
('india', make_date(2018, 11, 23), 'Guru Nanak Jayanti'),
('india', make_date(2018, 12, 25), 'Christmas'),

-- Mexico http://www.dias-festivos-mexico.com.mx/2018-mexico/
('mexico', make_date(2017, 11, 20), 'Revolución Mexicana'),
('mexico', make_date(2017, 12, 25), 'Navidad'),
('mexico', make_date(2018, 1, 1), 'Año Nuevo'),
('mexico', make_date(2018, 2, 5), 'Día de la Constitución Mexicana'),
('mexico', make_date(2018, 3, 19), 'Natalicio de Benito Juárez'),
('mexico', make_date(2018, 5, 1), 'Día del Trabajo'),
('mexico', make_date(2018, 9, 16), 'Día de la Independencia'),
('mexico', make_date(2018, 11, 19), 'Revolución Mexicana'),
('mexico', make_date(2018, 12, 1), 'Transmisión de Poder Ejecutivo Federal'),
('mexico', make_date(2018, 12, 25), 'Navidad'),

-- Russia https://www.timeanddate.com/holidays/russia/2018#!hol=9
('russia', make_date(2017, 11, 6), 'День народного единства'),
('russia', make_date(2018, 1, 1), 'Новый год'),
('russia', make_date(2018, 1, 2), 'Новогодние каникулы'),
('russia', make_date(2018, 1, 3), 'Новогодние каникулы'),
('russia', make_date(2018, 1, 4), 'Новогодние каникулы'),
('russia', make_date(2018, 1, 5), 'Новогодние каникулы'),
('russia', make_date(2018, 1, 8), 'Новогодние каникулы'),
('russia', make_date(2018, 2, 23), 'День защитника Отечества'),
('russia', make_date(2018, 3, 8), 'Восьмое марта'),
('russia', make_date(2018, 4, 30), 'Праздник Весны и Труда'),
('russia', make_date(2018, 5, 1), 'Праздник Весны и Труда'),
('russia', make_date(2018, 5, 9), 'День Победы'),
('russia', make_date(2018, 6, 12), 'День России'),
('russia', make_date(2017, 11, 5), 'День народного единства'),

-- United States https://www.calendarr.com/united-states/calendar-2018/
('united states', make_date(2017, 11, 10), 'Veterans day'),
('united states', make_date(2017, 11, 23), 'Thanksgiving'),
('united states', make_date(2017, 12, 25), 'Christmas'),
('united states', make_date(2018, 1, 1), 'New years day'),
('united states', make_date(2018, 1, 15), 'Martin Luther King Jr. Day'),
('united states', make_date(2018, 2, 19), 'Presidents'' day'),
('united states', make_date(2018, 4, 30), 'Good Friday'),
('united states', make_date(2018, 5, 28), 'Memorial day'),
('united states', make_date(2018, 7, 4), 'Independence day'),
('united states', make_date(2018, 9, 3), 'Labor day'),
('united states', make_date(2018, 10, 8), 'Columbus Day'),
('united states', make_date(2018, 11, 11), 'Veterans day'),
('united states', make_date(2018, 11, 22), 'Thanksgiving'),
('united states', make_date(2018, 12, 25), 'Christmas')
;
