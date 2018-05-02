CREATE DOMAIN hash AS character(32);
CREATE DOMAIN address AS character(30);
CREATE TYPE output AS 	( out_address 	address
						, out_amount 	bigint
);

CREATE TABLE utxos  ( tx_hash			hash
					, tx_index			integer
					, receiver_address	address
					, amount 			bigint
);
CREATE TABLE bestBlock ( best_block_num bigint);
CREATE TABLE txs 	( tx_hash		hash
					, inputs 		output[]
					, outputs 		output[]
					, block_num 	bigint NULL
					, timestamp 	numeric NULL
);