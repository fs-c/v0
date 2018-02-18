function getHashParams() {
  const hashParams = {};
  const r = /([^&;=]+)=?([^&;]*)/g
  const q = window.location.hash.substring(1);

  let e;
  while (e = r.exec(q)) {
     hashParams[e[1]] = decodeURIComponent(e[2]);
  }

  return hashParams;
}

function generateRandomString(length) {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  let text = '';
  for (var i = 0; i < length; i++) {
    text += chars.charAt(Math.floor(Math.random() * chars.length));
  }

  return text;
}

// The key in localStorage.
const stateKey = 'spotify_auth_state'

const spotify = new SpotifyWebApi();

const app = new Vue({
  el: '#app',
  data: {
    shortTerm: [],
    mediumTerm: [],
    longTerm: [],
  },
  methods: {
    avgFollowers(items) {
      const total = items
        .reduce((acc, cur) => acc + cur.followers.total, 0);

      return total / items.length
    },
    avgPopularity(items) {
      const total = items
        .reduce((acc, cur) => acc + cur.popularity, 0);

      return total / items.length
    },
    topGenres(items) {
      let genres = {};
      const all = items.map((e) => e.genres)
        .reduce((acc, cur) => {
          return acc.concat(cur);
        }, []);

      for (const genre of all) {
        if (genres[genre]) {
          genres[genre]++
        } else genres[genre] = 1
      }

      genres = Object.keys(genres).map((e) => `${e} (${genres[e]})`);
      
      return genres.sort((a, b) => {
          a.slice(a.lastIndexOf('('), a.lastIndexOf(')'))
          - b.slice(b.lastIndexOf('('), b.lastIndexOf(')'))
        }).slice(0, 5);
    },
  }
});

const params = getHashParams();

const state = params.state;
const token = params.access_token;
const expires = params.expires_id;
const storedState = localStorage.getItem(stateKey);

if (!token && (!state || state !== storedState)) {
  // We don't have a token, or there's a state mismatch.
  // Try to get a token.
  const scope = 'user-top-read';  
  const state = generateRandomString(8);
  const clientID = '2b6ec851a3484d53b10ff10a0ca3191d';

  localStorage.setItem(stateKey, state);

  const url = 'https://accounts.spotify.com/authorize'
    + `?response_type=token`
    + `&client_id=${encodeURIComponent(clientID)}`
    + `&scope=${encodeURIComponent(scope)}`
    + `&redirect_uri=${encodeURIComponent(window.location.origin + '/')}`
    + `&state=${encodeURIComponent(state)}`

  // Redirect to spotify auth page.
  window.location = url;
} else {
  // State match, and we have a token.
  // Get top artists and add them to the app.
  localStorage.removeItem(stateKey);

  spotify.setAccessToken(token);

  spotify.getMyTopArtists({
    limit: 50,
    time_range: 'short_term'
  }).then((data) => {
    console.log('short term: ');
    console.log(data);
    app.shortTerm = data.items}
  ).catch(console.error)

  spotify.getMyTopArtists({
    limit: 50,
    time_range: 'medium_term'
  }).then((data) => {
    console.log('medium term: ');
    console.log(data);
    app.mediumTerm = data.items
  }).catch(console.error)

  spotify.getMyTopArtists({
    limit: 50,
    time_range: 'long_term'
  }).then((data) => {
    console.log('long term: ');
    console.log(data);
    app.longTerm = data.items
  }).catch(console.error)
}
