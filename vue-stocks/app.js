const URL = 'https://www.alphavantage.co/query?function=TIME_SERIES_'
const KEYS = {
  alpha: 'KWO1BM3ETFOZNESC'
}

function buildURL (func, symbol) {
  return URL + func + `&symbol=${symbol}&apikey=${KEYS.alpha}`
}

const app = new Vue({
  el: '#app',
  data: {
    symbols: {},
    symbol: ''
  },
  methods: {
    add() {
      let s = this.symbol
      this.symbol = ''
      if (s.length > 0 && s.length < 7) {
        this.symbols[s] = {
          size: '6'
        }
        console.log(`Added symbol ${s}.`)
      } else console.log(`Symbol ${s} is invalid.`)
    }
  }
})
