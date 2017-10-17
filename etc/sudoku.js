let raw = [ 0, 4, 0, 0, 8, 3, 0, 6, 0, 6, 0, 7, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 6, 0, 2, 9, 0, 0, 0, 4, 0, 1, 0, 9, 2, 6, 9, 0, 0, 0, 3, 0, 0, 0, 8, 8, 2, 6, 0, 7, 0, 5, 0, 0, 0, 7, 9, 0, 5, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 3, 0, 9, 0, 6, 0, 1, 9, 0, 0, 7, 0 ]

class Sudoku {
  constructor (raw) {
    this._raw = raw
    this._dimensional = Sudoku.rawToDim(this._raw)
  }

  get raw () { return this._raw }
  get dimensional () { return this._dimensional }

  static rawToDim (raw) {
    let a = [ ]

    for (let i = 0; i < 9; i++) {
      if (!a[i]) a[i] = []

      let s = raw.splice(0, 9)
      for (let ii in s) a[i].push(s[ii])
    }

    return a
  }

  static dimToRaw (dim) {
    let a = [ ]

    for (let row of dim) a = a.concat(row)

    return a
  }

  getCubes () {
    let a = [ ]

    for (let i = 0; i < 9; i++) {
      for (let ii = 0; ii < 3; ii++) {
        a.push([])
      }
    }

    return a
  }
}

let sudoku = new Sudoku(raw)

console.log(sudoku.getCubes())
