\connect "holiday_ping";

CREATE TABLE "public"."users" (
        "id" serial PRIMARY KEY,
        "email" character varying(100) UNIQUE NOT NULL,
        "name" character varying(200) NOT NULL,
        "country" character varying(20) NOT NULL,
        "password" character varying(100) NOT NULL,
);
