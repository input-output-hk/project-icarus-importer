-- Auxiliary datatypes
CREATE DOMAIN hash AS text;
CREATE DOMAIN address AS text;
CREATE TYPE output AS ( out_address address
						          , out_amount 	bigint
                      );

-- Tables
CREATE TABLE utxos  ( utxo_id   text      PRIMARY KEY
                    , tx_hash   hash
          					, tx_index	integer
          					, receiver	address
          					, amount 	  bigint
                    );

-- FIXME: Define primary keys
CREATE TABLE bestBlock ( best_block_num bigint);

-- FIXME: Temporarily not used
-- FIXME: Define primary keys
CREATE TABLE txs 	( tx_hash		hash
        					, inputs 		output[]
        					, outputs 	output[]
        					, block_num bigint NULL
        					, tx_time   numeric NULL
                  );

-- FIXME: Delete, will be replaced by txs
CREATE table temp_txs (hash text);

-- FIXME: Add table of txs addr (tx_hash, addr, amount)


-- Indexes
CREATE INDEX ON "utxos" (receiver);