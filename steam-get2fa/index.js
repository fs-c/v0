const fs = require('fs');
const Steam = require('steam-user');
const rls = require('readline-sync');

const account = {
  accountName: rls.question('Name: '),
  password: rls.question('Pass: '),
}

const user = new Steam();

user.logOn(account);

user.on('loggedOn', () => {
  console.log('logged on');

  user.enableTwoFactor((response) => {
    console.log('enabled two factor, response: ');
    console.log(response);

    fs.writeFileSync('response.json', JSON.stringify(response), 'utf8');

    const code = rls.question('SMS Code: ');

    user.finalizeTwoFactor(response.shared_secret, code, (err) => {
      if (err) throw err;

      console.log('finalized two factor');
    });
  });
});

// Just report that something went wrong.
// No need to throw, this might just be a one-off steam server issue. 
user.on('error', (err) => console.log(err));
