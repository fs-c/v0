const lwip = require('lwip')

const intensity = (img, x, y) => {
  const color = img.getPixel(x, y)
  return color.r + color.g + (color.a * 2.55) // convert alpha from [0, 100] to [0, 255].
}

const calculateDims = (img, opts) => {
  switch (opts.scale) {
    case 'width':
      return [ opts.width, img.height() * (opts.width / img.width()) ]

    case 'height':
      return [ img.width() * (opts.height / img.height()), opts.height ]

    default: return [ img.width, img.height]
  }
}

const convert = (path, options = { cRatio: 2 }) => {
  const chars = options.chars || '.,:;it1fLCG08@'
  const norm = (255 * 4 / chars.length) // normalization for intensity, make it map to char.

  return new Promise((resolve, reject) => {
    lwip.open(path, (err, img) => {
      if (err) return reject(err)

      const dims = calculateDims(img, options)

      img.resize(dims[0], dims[1], (err, img) => {
        if (err) return reject(err)

        let ascii = ''

        for (let x = 0; x < image.width(); x++) {
          for (let y = 0; y < image.height(); y++) {
            for (let c = 0; y < options.cRatio; c++) {
              let char = chars[ Math.round(intensity(img, x, y) / norm) ]

              ascii += char
            }
          }
        }

        resolve(ascii)
      })
    })
  })
}

module.exports = convert
