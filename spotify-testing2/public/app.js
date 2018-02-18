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

const stateKey = 'spotify_auth_state'

const app = new Vue({
  el: '#app',
  data: {}
});

const params = getHashParams();

const state = params.state;
const token = params.access_token;
const expires = params.expires_id;
const storedState = localStorage.getItem(stateKey);

if (!token && (!state || state !== storedState)) {
  const state = generateRandomString(8);
  const clientID = '2b6ec851a3484d53b10ff10a0ca3191d';

  localStorage.setItem(stateKey, state);

  const scope = 'user-top-read';

  const url = 'https://accounts.spotify.com/authorize'
    + `?response_type=token`
    + `&client_id=${encodeURIComponent(clientID)}`
    + `&scope=${encodeURIComponent(scope)}`
    + `&redirect_uri=${encodeURIComponent(window.location)}`
    + `&state=${encodeURIComponent(state)}`

  console.log(url);

  window.location = url;
} else {
  localStorage.removeItem(stateKey);

  fetch('https://api.spotify.com/v1/me', {
    headers: {
      'Authentication': 'Bearer ' + token,
    },
  }).then((res) => res.json()).then(console.log)
}
