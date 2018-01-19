import * as mongoose from 'mongoose';

export const User = mongoose.model('User', new mongoose.Schema({
  username: String,
  password: String,
  accessLevel: { type: Number, default: 3 } // Equivalent: -1 = root, 3 = guest.
}))