import { join } from 'path';
import * as debug from 'debug';
import { readdirSync, readFileSync } from 'fs';
import { GetItemsInfoResponse as Item } from 'pr0gramm-api';

const log = debug('analyser');

require('debug').enable('analyser');

const path = 'data/item';
const items: Item[] = readdirSync(path)
  .map((item) => JSON.parse(readFileSync(join(path, item), 'utf8')));