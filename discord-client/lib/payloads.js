/**
 * Payloads for messages to Discord, executed in the contect of the Client class.
 * All objects are JSON.stringified because WS expects strings or buffers.
 * 
 * @returns {Object} An object with populated payload objects.
 * @private
 */
const payloads = function() { return {
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
        $os: require('os').platform(),
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
      seq: this.connection.sequence,
      token: this.token,
      session_id: this.connection.sessionID,
    }
  }),
};};

exports = { payloads };
