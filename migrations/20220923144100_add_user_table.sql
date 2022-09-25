CREATE TABLE IF NOT EXISTS user_account (
  id uuid NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  github_username text NOT NULL UNIQUE,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null
);

drop trigger if exists user_account_insert on user_account;
  create trigger user_account_insert before insert on user_account for each row execute procedure create_timestamps();
drop trigger if exists user_account_update on user_account;
  create trigger user_account_update before update on user_account for each row execute procedure update_timestamps();

ALTER TABLE plan
  ADD CONSTRAINT plan_pkey PRIMARY KEY (id),
  ADD COLUMN user_id uuid NULL REFERENCES user_account(id);

CREATE INDEX IF NOT EXISTS user_id_idx ON plan(user_id);
