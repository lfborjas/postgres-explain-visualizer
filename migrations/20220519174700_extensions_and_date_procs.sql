CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE OR REPLACE FUNCTION create_timestamps()
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.created_at = now();
            NEW.updated_at = now();
            RETURN NEW;
        END;
        $$ language 'plpgsql';

CREATE OR REPLACE FUNCTION update_timestamps()
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.updated_at = now();
            RETURN NEW;
        END;
        $$ language 'plpgsql';
