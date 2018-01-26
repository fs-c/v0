import { Context } from 'koa';
import { ApiKey, IApiKeyDocument } from '../models/ApiKey';

export async function generateKey(ctx: Context) {
  const user = ctx.state.user;
  const level = ctx.request.body.level;
  const description = ctx.request.body.description || '';

  if (!level || level < user.accessLevel) {
    return await ctx.render('status', {
      message: 'Invalid level.',
    });
  }

  try {
    const apiKey = await new ApiKey({
      level,
      description,
      owner: user.nickname,
    }).save();

    await ctx.render('status', {
      status: 'success',
      message: 'Created API key.',
      data: {
        Key: apiKey.key,
        Level: apiKey.level,
        Description: apiKey.description,
        Owner: apiKey.owner,
      },
    });
  } catch(err) {
    await ctx.render('status', {
      status: 'error',
      message: err.message,
    })
  }
}
