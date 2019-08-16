class Sudoku {
    constructor(field = null) {
        this.field = field;
    }

    row(index) {
        return this.field[index];
    }

    col(index) {
        return this.field.map((row) => row[index]);
    }

    group(index) {
        const imod = (index % 3) * 3;
        const idiv = Math.floor(index / 3) * 3;

        const colRange = [ imod, imod + 2 ];
        const rowRange = [ idiv, idiv + 2 ];

        const group = [];

        for (let y = rowRange[0]; y <= rowRange[1]; y++)
            for (let x = colRange[0]; x <= colRange[1]; x++)
                group.push(this.field[y][x]);

        return  { colRange, rowRange, group };
    }

    solve() {
        while (true) {
            const cur = this.emptyLocation;

            console.log(this.field, cur);

            if (!cur)
                break;

            const { x, y } = cur;

            for (let i = 1; i <= 9; i++) {
                if (!this.safe(x, y, i))
                    continue;

                this.field[y][x] = i;
            }

            this.field[y][x] = 0;
        }
    }

    safeInRow(index, number) {
        console.log(this.row(index));

        return this.row(index).indexOf(number) == -1;
    }

    safeInCol(index, number) {
        console.log(this.col(index));

        return this.col(index).indexOf(number) == -1;
    }

    safeInGroup(row, col, number) {
        for (let x = 0; x < 3; x++)
            for (let y = 0; y < 3; y++)
                if (this.field[y + col - col % 3][x + row - row % 3] == number)
                    return false;

        return true;
    }

    safe(row, col, number) {
        return this.safeInRow(row, number) && this.safeInCol(col, number)
            && this.safeInGroup(row, col, number);
    }

    get emptyLocation() {
        for (let y = 0; y < 9; y++)
            for (let x = 0; x < 9; x++)
                if (this.field[y][x] != 0)
                    return { x, y };

        return false;
    }
};

const sudoku = new Sudoku([
    [ 4, 2, 7, 1, 0, 0, 5, 6, 8 ],
    [ 9, 1, 5, 8, 7, 6, 3, 4, 2 ],
    [ 6, 8, 3, 5, 4, 2, 1, 9, 7 ],
    [ 1, 0, 0, 0, 0, 0, 6, 0, 0 ],
    [ 5, 0, 0, 0, 2, 1, 0, 0, 0 ],
    [ 7, 0, 0, 0, 8, 5, 0, 0, 0 ],
    [ 0, 7, 1, 4, 5, 0, 0, 0, 6 ],
    [ 0, 4, 0, 2, 6, 0, 0, 0, 0 ],
    [ 0, 5, 6, 0, 1, 0, 4, 0, 3 ],
]);

console.log(sudoku.field);

console.log(sudoku.row(3), sudoku.col(3));

console.log(sudoku.solve());