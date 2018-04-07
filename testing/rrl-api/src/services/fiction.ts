import { get } from 'got';
import * as date from 'date.js'; // TODO: DANGEROUS, needs declaration.
import * as cheerio from 'cheerio';
import { getBaseAddress } from '../constants';

import { Fiction } from '../common-types';

export class FictionService {
  public async getFiction(id: number): Promise<Fiction> {
    const url = `${getBaseAddress()}/fiction/${id.toString()}`;

    const { body } = await get(url);

    return FictionParser.parseFiction(body);
  }

  public async getRandom(): Promise<Fiction> {
    const url = `${getBaseAddress()}/fiction/random`;

    const { body } = await get(url);

    return FictionParser.parseFiction(body);
  }
}

class FictionParser {
  public static parseFiction(html: string): Fiction {
    const $ = cheerio.load('html');

    const title = $('div.fic-title').find('h1').text();
    const type = $('span.bg-blue-hoki').eq(1).text();
    const status = $('span.bg-blue-hoki').eq(2).text();
    const image = $('cover-' + title).attr('src');

    const tags = $('span.tags').find('label')
      .map((i, el) => $(el).text()).get();

    const warnings = $('ul.list-inline').find('li')
      .map((i, el) => $(el).text()).get();

    const description = $('div.hidden-content').find('p')
      .map((i, el) => $(el).text()).get().join('');
    
    const authorEl = $('.portlet.light').eq(0);

    const author = {
      name: $(authorEl).find('.mt-card-name').find('a').text(),
      title: $(authorEl).find('.mt-card-desc').text(),
      avatar: $(authorEl).find('.mt-card-avatar').find('img').attr('src'),
      id: 2
    };

    return { type, tags, title, image, status, 
      author, warnings, description };
  }
}