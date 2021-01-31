CREATE TABLE calculation
  ( id BIGSERIAL UNIQUE PRIMARY KEY
  , calculation_text TEXT NOT NULL
  , calculation_result DOUBLE PRECISION NOT NULL
  , username TEXT NOT NULL
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  );

----------

DROP TABLE IF EXISTS calculation;
