-- Auxiliary datatypes
CREATE DOMAIN hash AS text;
CREATE DOMAIN address AS text;

-- Tables
CREATE TABLE utxos  ( utxo_id   text      PRIMARY KEY
                    , tx_hash   hash
          					, tx_index	integer
          					, receiver	address
          					, amount 	  bigint
                    );

-- FIXME: Not used, delete?
CREATE TABLE bestBlock ( best_block_num bigint);

CREATE TABLE txs 	( hash		          hash      PRIMARY KEY
                  , inputs_address 		text[]
                  , inputs_amount 		bigint[]
        					, outputs_address   text[]
                  , outputs_amount    bigint[]
        					, block_num         bigint    NULL
        					, time              timestamp with time zone NULL
                  );

CREATE TABLE tx_addresses ( tx_hash  hash     REFERENCES txs ON DELETE CASCADE
											    , address  address
											    );

-- Indexes
CREATE INDEX ON utxos (receiver);
CREATE INDEX ON txs (hash);
CREATE INDEX ON txs (time);
CREATE INDEX ON tx_addresses (tx_hash);
CREATE INDEX ON tx_addresses (address);
