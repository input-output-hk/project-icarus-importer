ALTER TABLE txs ADD COLUMN succeeded boolean DEFAULT true;
ALTER TABLE txs ALTER COLUMN block_num bigint NULL;
