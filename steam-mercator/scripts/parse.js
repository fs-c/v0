module.exports = parse

function parse (input) {
  if (input.indexOf('!') === 0) {
    let obj = {
      command: input.indexOf(' ') !== -1 ? input.slice(1, input.indexOf(' ') + 1).trim() : input.slice(1),
      arguments: input.indexOf(' ') !== -1 ? input.slice(input.indexOf(' ') + 1).split(' ') : false
    }
    return obj
  } else return false
}
