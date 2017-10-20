/**
* Please note that _this does not work_.
*/

let raw = [ 0, 4, 0, 0, 8, 3, 0, 6, 0, 6, 0, 7, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 6, 0, 2, 9, 0, 0, 0, 4, 0, 1, 0, 9, 2, 6, 9, 0, 0, 0, 3, 0, 0, 0, 8, 8, 2, 6, 0, 7, 0, 5, 0, 0, 0, 7, 9, 0, 5, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 3, 0, 9, 0, 6, 0, 1, 9, 0, 0, 7, 0 ]

class Sudoku {
  constructor (raw) {
    this.raw = raw

    this._original = raw.map((e, i) => !e ? i : undefined)
  }

  /**
  * @return an array of rows.
  */
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

  /**
  * @return an array of columns.
  */
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

  /**
  * @return an array containing the contents of the nine 3x3 fields.
  */
  getCubes () {
    let o = 0
    let a = [  ]

    for (let i = 0; i < 9; i++) {
      a.push([  ])

      for (let ii = 0; ii < 3; ii++) {
        this.raw.slice(o + (ii * 9), o + (ii * 9) + 3).map(e => a[i].push(e))
      }

      o += 3
    }
    return a
  }

  /**
  * @return an object with the status and reason (for error) properties.
  */
  isSolved () {
    // if (this.raw.reduce((a, b) => a + b) !== 1215) return { status: false, reason: 'total' }

    for (let i in this.getRows())
      if (this.getRows()[i].reduce((a, b) => a + b) !== 45) return { status: false, reason: `row ${i} / ${this.getRows()[i]}` }

    for (let i in this.getColumns())
      if (this.getColumns()[i].reduce((a, b) => a + b) !== 45) return { status: false, reason: `column ${i} / ${this.getColumns()[i]}` }

    for (let i in this.getCubes())
      if (this.getCubes()[i].reduce((a, b) => a+ b) !== 45) return { status: false, reason: `cube ${i} / ${this.getCubes()[i]}` }

    return { status: true, reason: '' }
  }

  /**
  * Randomly fill all fields that are set to 0.
  */
  fillRandomly () {
    for (let i of this._original) {
      if (i !== undefined) this.raw[i] = Math.ceil((Math.random() * 8) + 1)
    }

    return this // For convenience.
  }

  /**
  * Keep creating new sudokus and randomly fill them until a valid one is found.
  * @return a valid sudoku object.
  */
  solve () {
    return new Promise((resolve, reject) => {
      let i = 0
      let c = new Sudoku(this.raw)

      while (!c.isSolved().status) {
        i++
        c.fillRandomly()

        if (process.env.NODE_ENV === 'dev') {
          let s = c.isSolved()
          console.log(`iteration ${i} = status: ${s.status} / res: ${s.reason}`)
        }
      }

      resolve(c)
    })
  }
}

let s = new Sudoku(raw)

s.solve().then(e => console.log(e))
