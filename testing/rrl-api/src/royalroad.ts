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

interface ActiveRankingBlurb extends FictionBlurb {
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

      fictions.push(
        Object.assign(Parser.parseBlurb($, el),
        { latest })
      );
    });

    return fictions;
  }
}