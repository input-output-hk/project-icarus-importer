CREATE DOMAIN hash AS text;
CREATE DOMAIN address AS text;
CREATE TYPE output AS 	( out_address 	address
						, out_amount 	bigint
);

CREATE TABLE utxos  ( tx_hash	hash
					, tx_index	integer
					, receiver	address
					, amount 	bigint
);
CREATE TABLE bestBlock ( best_block_num bigint);
CREATE TABLE txs 	( tx_hash		hash
					, inputs 		output[]
					, outputs 		output[]
					, block_num 	bigint NULL
					, timestamp 	numeric NULL
);

-- FIXME: Agregar tabla txs addr (tx_hash, addr, amount)