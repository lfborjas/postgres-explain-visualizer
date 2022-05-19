CREATE TABLE IF NOT EXISTS plan (
  id uuid NOT NULL DEFAULT uuid_generate_v4(),
  source text NOT NULL,
  query text,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null
);

drop trigger if exists plan_insert on plan;
  create trigger plan_insert before insert on plan for each row execute procedure create_timestamps();
drop trigger if exists plan_update on plan;
  create trigger plan_update before update on plan for each row execute procedure update_timestamps();
