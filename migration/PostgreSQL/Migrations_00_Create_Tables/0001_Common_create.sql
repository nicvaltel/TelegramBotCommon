BEGIN transaction;

create extension IF NOT EXISTS  citext;
create extension IF NOT EXISTS pgcrypto;

CREATE SCHEMA common;


create table common.users (
  user_id bigint primary key not null,
  username text not null,
  created timestamp with time zone default (now() at time zone 'utc')
);

create table common.messages (
  id bigserial primary key not null,
  user_id bigint not null,
  text text not null,
  sent timestamp with time zone default (now() at time zone 'utc'),
  FOREIGN KEY (user_id) REFERENCES rusrom.users(user_id)
);

END TRANSACTION;