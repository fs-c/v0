const Jimp = require('jimp')
const Colors = require('couleurs')

const chars = '.,:;it1fLCG08@'
const norm = (255 * 4 / chars.length)

const intensity = pixel => pixel.r + pixel.g + pixel.a

const convert_core = (img) => {
  let ascii = ''

  for (let y = 0; y < img.bitmap.height; y++) {
    for (let x = 0; x < img.bitmap.width; x++) {
      for (let c = 0; c < 2; c++) {
        let pixel = Jimp.intToRGBA(img.getPixelColor(x, y))
        let char = chars.charAt(Math.round(intensity(pixel)) / norm)

        char = Colors.fg(char, pixel.r, pixel.g, pixel.b)

        ascii += char
      }
    }

    if (y !== img.bitmap.height - 1) ascii += '\n'
  }

  return ascii
}

const convert = (path) => {
  return new Promise((resolve, reject) => {
    Jimp.read(path).then(image => {
      resolve(convert_core(image.resize(Jimp.AUTO, 75)))
    }).catch(reject)
  })
}

module.exports = convert
