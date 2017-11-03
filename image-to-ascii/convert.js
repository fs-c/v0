const Jimp = require('jimp')
const Colors = require('couleurs')

const CHARS = '.,:;it1fLCG08@'
const NORM = (255 * 4 / CHARS.length)

const intensity = pixel => pixel.r + pixel.g + pixel.a

const convert = (img, opts) => {
  let ascii = ''
  img = img.resize(opts.height, opts.width)

  for (let y = 0; y < img.bitmap.height; y++) {
    for (let x = 0; x < img.bitmap.width; x++) {
      for (let c = 0; c < opts.cRatio; c++) {
        let pixel = Jimp.intToRGBA(img.getPixelColor(x, y))
        let char = opts.chars.charAt(Math.round(intensity(pixel)) / NORM)

        if (opts.colors)
          char = Colors.fg(char, pixel.r, pixel.g, pixel.b)

        ascii += char
      }
    }

    if (y !== img.bitmap.height - 1) ascii += '\n'
  }

  return ascii
}

module.exports = (path, options = {  }) => {
  const opts = {
    colors: options.colors || true,
    chars: options.chars   || CHARS,
    cRatio: options.cRatio || 2,
    width: options.width   || options.height ? Jimp.AUTO : 50,
    height: options.height || options.width  ? 50 : Jimp.AUTO
  }

  return new Promise((resolve, reject) => {
    Jimp.read(path).then(image => {
      resolve(convert(image, opts))
    }).catch(reject)
  })
}
