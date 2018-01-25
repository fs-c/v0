import base64url from 'base64url';
import { randomBytes } from 'crypto';
import { Document, Schema, Model, model } from 'mongoose';

export interface IApiKeyDocument extends Document {
  key: string;
  level: number;
  owner: string;
}

const ApiKeySchema = new Schema({
  key: {
    type: String,
    default: base64url(randomBytes(20)),
  },
  level: {
    type: Number,
    default: 3,
  },
  owner: {
    type: String,
    default: 'anon',
  },
});

export const ApiKey: Model<IApiKeyDocument> = model<IApiKeyDocument>(
  'ApiKey',
  ApiKeySchema,
);