// JSON.stringify all objects because the WS expects strings or buffers.
// Executed in the context of the Client class.
const payloads = exports.payloads = function() { return {
  heartbeat: JSON.stringify({
    op: 1,
    d: this.connection.sequence,
  }),
  identify: JSON.stringify({
    op: 2,
    d: {
      token: this.token,
      v: 6,
      properties: {
        $os: require('os').platform,
        $browser: '',
        $device: '',
        $referrer: '',
        $referring_domain: '',   
      }
    }
  }),
  resume: JSON.stringify({
    op: 6,
    d: {
      seq: this.sequence,
      token: this.token,
      session_id: this.connection.sessionID,
    }
  }),
}};
