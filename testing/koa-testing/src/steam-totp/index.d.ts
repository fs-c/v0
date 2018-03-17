declare module 'steam-totp' {
  function getAuthCode(secret: string, offset?: number): string;
  function getAuthCode(
    secret: string,
    callback: (err: Error, code: string, offset: number, latency: number) => void,
  ): void;
}
