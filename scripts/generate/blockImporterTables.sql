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

CREATE TABLE bestblock ( best_block_num bigint );

CREATE TABLE txs 	( hash		  hash PRIMARY KEY
        					, block_num bigint NULL
        					, time      timestamp with time zone NULL
                  );

CREATE TABLE tx_details  ( hash     hash REFERENCES txs ON DELETE CASCADE
												 , is_input boolean
												 , address  address
												 , amount   bigint
                         ); 

-- Indexes
CREATE INDEX ON "utxos" (receiver);
CREATE INDEX address_idx ON tx_details (address);
