import { Document, Schema, Model, model } from 'mongoose';

export interface IUserDocument extends Document {
  nickname: string;
  password: string;
  accessLevel: number;
  created: number;
}

const UserSchema = new Schema({
  nickname: String,
  password: String,
  accessLevel: { // Equivalents: -1 = root, 3 = guest.
    type: Number,
    default: 3,
  },
  created: { type: Date, default: Date.now() },
});

export const User: Model<IUserDocument> = model<IUserDocument>(
  'User',
  UserSchema,
);
