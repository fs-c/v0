const fs = require('fs');
const rls = require('readline-sync');
const Steam = require('steamcommunity');

const account = {
  accountName: rls.question('Name: '),
  password: rls.questionNewPassword('Pass: '),
}

const user = new Steam();

user.login(account, (err) => {
  if (err) throw err;
  console.log('logged on');

  user.enableTwoFactor((err, response) => {
    if (err) throw err;    
    console.log(response);

    fs.writeFileSync(`response-${Date.now()}.json`, JSON.stringify(response), 'utf8');

    const code = rls.question('SMS Code: ');

    user.finalizeTwoFactor(response.shared_secret, code, (err) => {
      if (err) throw err;

      console.log('finalized two factor');
    });
  });
});
