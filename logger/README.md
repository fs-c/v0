### logger

Super simple ultra-slim logger for small personal projects.

No external dependencies, and less than 50 LOC.

```
const { log } = require('@sturmwalzer/logger');

log('%o just ate %o cookies!', 'lilly', 3);

log({
  cookieJar: {
    owner: 'sturmwalzer',
    cookies: [
      {
        crunchyness: 100,
        sweetness: 89,
      },
      {
        crunchyness: 93,
        sweetness: 90,
      },
    ]
  },
});
```

![terminal](https://i.imgur.com/qmzMJoB.png)