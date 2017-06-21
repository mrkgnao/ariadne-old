drop owned by mrkgnao;

create extension "uuid-ossp";

create table knots
  ( knot_id         uuid
                    primary key
                    default uuid_generate_v1mc()
  , knot_created_at timestamp with time zone
                    not null
                    default now()
  , knot_extra_data jsonb
                    not null
                    default '{}'
  );

create table paths
  ( path_id     uuid
                primary key
                constraint path__knot
                references knots
  , path_source uuid
                constraint path__source_fk
                references knots
                not null
  , path_target uuid
                constraint path__target_fk
                references knots
                not null
  );

create table links
  ( link_id     uuid
                primary key
                constraint link__knot
                references knots
  , link_url    text
                not null
  , link_title  text
                not null
  );

create table fulltexts
  ( fulltext_id uuid
                primary key
                constraint link__knot
                references knots
  , fulltext_contents
                text
                not null
  );


create table persons
  ( person_id   uuid
                primary key
                constraint person__knot
                references knots
  , person_name text
                not null
  );

create table quotes
  ( quote_id    uuid
                primary key
                constraint quote__knot
                references knots (knot_id)
  , speaker_id  uuid
                constraint quote__person
                references persons (person_id)
                not null
  , quote_text  text
                not null
  );

create extension pg_trgm;

insert into knots(knot_extra_data)
values
  (default),
  (default),
  ('{"a":"b"}'),
  (default),
  (default);

with rows as
(insert into knots (knot_extra_data) values (default) returning knot_id)
insert into paths(path_id, path_source, path_target)
select knot_id, (select knot_id from knots limit 1), (select knot_id from knots limit 1)
from rows;

-- insert into persons (person_id, person_name)
-- values
--   (1, 'Nietzsche'),
--   (2, 'Camus');

-- insert into quotes (quote_id, speaker_id, quote_text)
-- values
--   (3, 1, 'What does not kill me makes me stronger'),
--   (4, 2, 'I leave Sisyphus at the foot of the mountain.');

-- create table quotes
--   ( quote_id    uuid
--                 primary key
--                 references knots(knot_id)
--   , person_id   uuid
--                 references persons(person_id)
--                 not null
--   , quote_text  text
--                 not null
--   );

-- create table quotes
--   ( quote_id    uuid
--                 primary key
--                 references knots(knot_id)
--   , person_id   uuid
--                 references persons(person_id)
--                 not null
--   , quote_text  text
--                 not null
--   );

-- create function insert_person(name_of_person text)
-- returns void as $$
-- declare new_person_id uuid;
-- begin

-- insert into knots values (default) returning knot_id into new_person_id;
-- insert into persons(person_id, person_name) values (new_person_id, name_of_person);

-- end;
-- $$ language plpgsql;

-- select insert_person('Dude');
