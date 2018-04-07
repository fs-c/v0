const BASE_ADDRESS = 'https://royalroadl.com'

import { get } from 'got';
import * as date from 'date.js'; // TODO: DANGEROUS, needs declaration.
import * as cheerio from 'cheerio';
import { URLSearchParams } from 'url';

interface FictionBlurb {
  id: number,
  type: string,
  title: string,
  image: string,
  tags: string[],
}

interface LatestBlurb extends FictionBlurb {
  latest: {
    name: string,
    link: string,
    created: number,
  }[],
}

interface PopularBlurb extends FictionBlurb {
  description: string,
  stats: {
    pages: number,
    latest: number,
    rating: number,
    chapters: number,
    followers: number,
  },
}

export class RoyalRoadAPI {
  constructor() {}

  public async getLatest(page: number = 1): Promise<LatestBlurb[]> {
    const params = new URLSearchParams({ page: page.toString() });
    const url = `${BASE_ADDRESS}/fictions/latest-updates?${params}`;

    const response = await get(url);

    return Parser.parseLatest(response.body);
  }

  public async getPopular(page: number = 1): Promise<PopularBlurb[]> {
    const params = new URLSearchParams({ page: page.toString() });
    const url = `${BASE_ADDRESS}/fictions/active-popular?${params}`;

    const response = await get(url);

    return Parser.parsePopular(response.body);
  }
}

class Parser {
  private static parseBlurb(
    $: CheerioStatic, el: CheerioElement
  ): FictionBlurb {
    const titleEl = $(el).find('.fiction-title').children('a');

    const title = $(titleEl).text();
    const image = $(el).find('img').attr('src');
    const type = $(el).find('span.label.bg-blue-hoki').text();
    const id = parseInt($(titleEl).attr('href').split('/')[2], 10);

    const tags = $(el).find('span.label.bg-blue-dark')
      .map((i, el) => $(el).text()).get();

    return { id, type, tags, title, image };
  }

  static parseLatest(html: string): LatestBlurb[] {
    const $ = cheerio.load(html);

    let fictions: LatestBlurb[] = [];

    // Using .each instead of the more concise .map because the typings are 
    // suboptimal. (TODO, maybe)
    $('.fiction-list-item').each((i, el) => {
      let latest: {
        name: string,
        link: string,
        created: number,
      }[] = [];

      $(el).find('li.list-item').each((i, el) => {
        latest.push({
          link: $(el).find('a').attr('href'),
          name: $(el).find('span.col-xs-8').text(),
          created: (date($(el).find('time').text()) as Date).getTime(),          
        });
      });

      fictions.push(Object.assign(
        Parser.parseBlurb($, el),
        { latest }
      ));
    });

    return fictions;
  }

  static parsePopular(html: string): PopularBlurb[] {
    const $ = cheerio.load(html);

    let fictions: PopularBlurb[] = [];

    // Using .each instead of the more concise .map because the typings are 
    // suboptimal. (TODO, maybe)
    $('.fiction-list-item').each((i, el) => {
      let description = '';

      // Sigh...
      $(el).find('.margin-top-10.col-xs-12').find('p').each((i, el) => {
        description += $(el).text() + '\n';
      });

      // DANGEROUS. But due to RRL site design there's few ways around this.
      let stats: any = {};

      stats.latest = date($(el).find('time').attr('datetime')).getTime();
      stats.rating = parseFloat($(el).find('.star').attr('title'));

      $(el).find('span').each((i, el) => {
        const text = $(el).text().toLowerCase();
        const key = text.split(' ')[1];
        const value = parseInt(text.split(' ')[0].replace(/,/gi, ''), 10);

        if (!key || !value) { return; }

        stats[key] = value;
      });

      fictions.push(Object.assign(
        Parser.parseBlurb($, el),
        { description },
        { stats }
      ));
    });

    return fictions;
  }
}