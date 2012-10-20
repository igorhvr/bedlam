--
-- PostgreSQL database dump
--

SET client_encoding = 'SQL_ASCII';
SET check_function_bodies = false;

--
-- TOC entry 4 (OID 2200)
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


SET search_path = public, pg_catalog;

--
-- TOC entry 5 (OID 20817)
-- Name: knowledge; Type: TABLE; Schema: public; Owner: scgmille
--

CREATE TABLE knowledge (
    "type" character varying(10) NOT NULL,
    "key" character varying(80) NOT NULL,
    data text NOT NULL
);


--
-- TOC entry 6 (OID 20817)
-- Name: knowledge; Type: ACL; Schema: public; Owner: scgmille
--

REVOKE ALL ON TABLE knowledge FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE ON TABLE knowledge TO sarahadmin;


--
-- TOC entry 7 (OID 20822)
-- Name: timezones; Type: TABLE; Schema: public; Owner: scgmille
--

CREATE TABLE timezones (
    place character varying(80) NOT NULL,
    utfoffset integer NOT NULL
);


--
-- TOC entry 8 (OID 20824)
-- Name: tell; Type: TABLE; Schema: public; Owner: scgmille
--

CREATE TABLE tell (
    recipient character varying(20) NOT NULL,
    sender character varying(20) NOT NULL,
    message text NOT NULL,
    id character varying(20) NOT NULL
);


--
-- TOC entry 9 (OID 20829)
-- Name: seen; Type: TABLE; Schema: public; Owner: scgmille
--

CREATE TABLE seen (
    nick character varying(20) NOT NULL,
    seenon timestamp with time zone NOT NULL,
    message text NOT NULL,
    id character varying(20) NOT NULL
);


--
-- TOC entry 10 (OID 20834)
-- Name: aka; Type: TABLE; Schema: public; Owner: scgmille
--

CREATE TABLE aka (
    "key" character varying(255),
    data character varying(255)
);


--
-- TOC entry 11 (OID 22084)
-- Name: knowledge_type_key; Type: INDEX; Schema: public; Owner: scgmille
--

CREATE UNIQUE INDEX knowledge_type_key ON knowledge USING btree ("type", "key", data);


--
-- TOC entry 12 (OID 22085)
-- Name: type_index; Type: INDEX; Schema: public; Owner: scgmille
--

CREATE INDEX type_index ON knowledge USING btree ("type");


--
-- TOC entry 13 (OID 22086)
-- Name: timezones_pkey; Type: CONSTRAINT; Schema: public; Owner: scgmille
--

ALTER TABLE ONLY timezones
    ADD CONSTRAINT timezones_pkey PRIMARY KEY (place);


--
-- TOC entry 14 (OID 22088)
-- Name: seen_pkey; Type: CONSTRAINT; Schema: public; Owner: scgmille
--

ALTER TABLE ONLY seen
    ADD CONSTRAINT seen_pkey PRIMARY KEY (nick);


--
-- TOC entry 3 (OID 2200)
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public namespace';

CREATE TABLE airport (
  code char(3) not null,
  city varchar(60) not null,
  state char(2)
);
