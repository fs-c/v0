let raw = [ 0, 4, 0, 0, 8, 3, 0, 6, 0, 6, 0, 7, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 6, 0, 2, 9, 0, 0, 0, 4, 0, 1, 0, 9, 2, 6, 9, 0, 0, 0, 3, 0, 0, 0, 8, 8, 2, 6, 0, 7, 0, 5, 0, 0, 0, 7, 9, 0, 5, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 3, 0, 9, 0, 6, 0, 1, 9, 0, 0, 7, 0 ]
let alt = [ 2, 9, 5, 7, 4, 3, 8, 6, 1, 4, 3, 1, 8, 6, 5, 9, 2, 7, 8, 7, 6, 1, 9, 2, 5, 4, 3, 3, 8, 7, 4, 5, 9, 2, 1, 6, 6, 1, 2, 3, 8, 7, 4, 9, 5, 5, 4, 9, 2, 1, 6, 7, 3, 8, 7, 6, 3, 5, 3, 4, 1, 8, 9, 9, 2, 8, 6, 7, 1, 3, 5, 4, 1, 5, 4, 9, 3, 8, 6, 7, 2 ]

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
  
  isSolved () {
    if (this.raw.reduce((a, b) => a + b) !== 1215) return { status: false, reason: 'total' }

    for (let i in this.getRows())
      if (this.getRows()[i].reduce((a, b) => a + b) !== 45) return { status: false, reason: `row ${i} / ${this.getRows()[i]}` }

    for (let i in this.getColumns())
      if (this.getColumns()[i].reduce((a, b) => a + b) !== 45) return { status: false, reason: `column ${i} / ${this.getColumns()[i]}` }

    for (let i in this.getCubes())
      if (this.getCubes()[i].reduce((a, b) => a+ b) !== 45) return { status: false, reason: `cube ${i} / ${this.getCubes()[i]}` }

    return true
  }

  fillRandomly () {
    let a = [  ]

    for (let n of this.raw) a.push(!n ? Math.floor((Math.random() * 8) + 1) : n)

    return new Sudoku(a)
  }

  solve () {
    let c = new Sudoku(this.raw)

    while (!c.isSolved().status) {
      c = new Sudoku(this.raw).fillRandomly()
    }

    return c
  }
}

let s = new Sudoku(raw)

console.log(s.solve().getRows())
