export interface FictionBlurb {
  id: number,
  type: string,
  title: string,
  image: string,
  tags: string[],
}

export interface LatestBlurb extends FictionBlurb {
  latest: {
    name: string,
    link: string,
    created: number,
  }[],
}

export interface PopularBlurb extends FictionBlurb {
  description: string,
  stats: {
    pages: number,
    latest: number,
    rating: number,
    chapters: number,
    followers: number,
  },
}

export interface BestBlurb extends PopularBlurb {}

export interface SearchBlurb {
  id: number,
  pages: number,
  title: string,
  image: string,
  author: string,
  description: string,
}