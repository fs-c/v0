export default {
    oidc: {
        clientId: '0oam89tv5H5LBTiw0356',
        issuer: 'https://dev-789978.okta.com/oauth2/default',
        redirectUri: 'http://localhost:8080/implicit/callback',
        scope: 'openid profile email',
    },
    resourceServer: {
        messagesUrl: 'http://localhost:8000/api/messages',
    },
};
