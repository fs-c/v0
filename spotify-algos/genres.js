const albums = require('./albums');

// Set of all unique genres of all artists.
const genresSet = new Set(
  albums.map((album) => album.genres)
        .reduce((acc, cur) => acc.concat(cur), [])
);

// Object of genres; key is name, value is number of occurences.
const genresObj = albums.reduce((acc, cur) => {
  cur.genres.forEach((genre) => 
    acc[genre] = acc[genre] ? acc[genre] + 1 : 1
  )

  return acc;
}, {});
