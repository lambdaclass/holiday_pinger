\connect "holiday_ping";

DROP TABLE IF EXISTS "users";
CREATE SEQUENCE users_id_seq INCREMENT 1 MINVALUE 1 MAXVALUE 9223372036854775807 START 1 CACHE 1;

CREATE TABLE "public"."users" (
        "id" integer DEFAULT nextval('users_id_seq') NOT NULL,
        "email" character varying(100) NOT NULL,
        "name" character varying(200) NOT NULL,
        "country" character varying(20) NOT NULL,
        "password" character varying(100) NOT NULL,
        CONSTRAINT "users_email" UNIQUE ("email"),
        CONSTRAINT "users_id" PRIMARY KEY ("id")
) WITH (oids = false);
