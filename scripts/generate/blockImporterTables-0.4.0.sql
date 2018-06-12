-- Tables
CREATE TABLE pending_txs 	( hash		          text      PRIMARY KEY
                          , inputs_address 		text[]
                          , inputs_amount 		bigint[]
        					        , outputs_address   text[]
                          , outputs_amount    bigint[]
                          , created_time      timestamp with time zone
                          );

CREATE TABLE ptx_addresses  ( tx_hash  text     REFERENCES pending_txs ON DELETE CASCADE
											      , address  text
											      );

-- Indexes
CREATE INDEX ON pending_txs (hash);
CREATE INDEX ON ptx_addresses (tx_hash);
CREATE INDEX ON ptx_addresses (address);