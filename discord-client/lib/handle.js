const debug = require('debug')('discord');

const handle = module.exports = function(message) {
  try {
    message = JSON.parse(message);
  } catch(e) { // Really unlikely.
    debug('error parsing message (%o)', e.message);
    return;
  }

  const data = message.d;

  switch (message.op) {
    // Event dispatched.
    case 0:
      this.connection.sequence = message.s;
      break;

    // Invalid session.
    case 9:
      this.connection.sequence = 0;
      this.connection.sessionID = undefined;

      debug('invalid session');

      this.socket.close();
      this.state = 'disconnected';

      this.emit('error', new Error('Invalid session.'));
      break;

    // Socket hello.
    case 10:
      if (this.connection.sequence && this.connection.sessionID) {
        // If we had a connection before, resume it.
        this.send(this.payloads.resume);
      } else {
        // Else, send initial identification.
        this.send(this.payloads.identify);
      }

      // Clear heartbeat timer, if it exists.
      const heartbeat = this.heartbeat.timer
      if (heartbeat) {
        clearInterval(heartbeat);
      }

      // Get heartbeat interval information, will fall back to default if not 
      // provided. 
      const interval = data.heartbeat_interval;
      if (interval) {
        this.heartbeat.interval = interval;
      }

      debug('sending heartbeats with %oms interval',
        this.heartbeat.interval);

      this.heartbeat.timer = setTimeout(() => {
        this.heartbeat.lastSent = Date.now();

        this.send(this.payloads.heartbeat);

        this.heartbeat.timeoutTimer = setTimeout(() => {
          // If this gets called we have not received an heartbeat for too long.
          this.socket.close();
          this.state = 'disconnected';
        }, this.heartbeat.maxDelay);

        debug('sent heartbeat');
      }, this.heartbeat.interval);
      break;
    
    // Heartbeat acknowledgement.
    case 11:
      // Clear the timeout catch.
      clearInterval(this.heartbeat.timeoutTimer);

      // Calculate our ping.
      this.connection.ping = Date.now() - this.heartbeat.lastSent;

      debug('heartbeat acknowledged with %oms delay', this.connection.ping);
      break;
  }
}