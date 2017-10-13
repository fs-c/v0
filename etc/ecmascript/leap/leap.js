class Year {
  constructor (year) {
    this._year = year
  }

  isLeap () {
    return !(this._year % 4) && !!(this._year % 100) ? true : !(this._year % 400) ? true : false
  }
}
