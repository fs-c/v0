const bcrypt = require('bcrypt');

bcrypt.hash('password', 8, (err, hash) =>
  console.log(err, hash));
