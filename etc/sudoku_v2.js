let raw = [ 0, 4, 0, 0, 8, 3, 0, 6, 0, 6, 0, 7, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 6, 0, 2, 9, 0, 0, 0, 4, 0, 1, 0, 9, 2, 6, 9, 0, 0, 0, 3, 0, 0, 0, 8, 8, 2, 6, 0, 7, 0, 5, 0, 0, 0, 7, 9, 0, 5, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 3, 0, 9, 0, 6, 0, 1, 9, 0, 0, 7, 0 ]

class Sudoku {
  constructor (raw) {
    this.raw = raw
  }

  getRows () {
    let o = 0
    let a = [ ]
    let r = this.raw

    for (let i = 0; i < 9; i++) {
      a.push([  ])

      this.raw.slice(o, o + 9).map(e => a[i].push(e))

      o += 9
    }

    return a
  }

  getColumns () {
    let a  = [  ]

    for (let i = 0; i < 9; ++i) {
      a.push([  ])
      for (let ii = 0; ii < 9; ++ii) {
        a[i].push(this.getRows()[ii][i])
      }
    }

    return a
  }

  getCubes () {
    let a = [  ]
    let o = 0

    for (let i = 0; i < 9; i++) {
      a.push([  ])

      for (let ii = 0; ii < 3; ii++) {
        this.raw.slice(o + (ii * 9), o + (ii * 9) + 3).map(e => a[i].push(e))
      }

      o += 3
    }
    return a
  }
}

let s = new Sudoku(raw)

console.log(s.getRows())
