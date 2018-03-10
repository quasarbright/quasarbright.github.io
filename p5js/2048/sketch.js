let numRows = 4
let numCols = 4
let board
let w, h
$(document).ready(function() {
    $(document).keydown(function(event) {
        function addNum() {
            zerocoords = []
            for (let r = 0; r < board.length; r++) {
                for (let c = 0; c < board[0].length; c++) {
                    if (board[r][c] === 0) {
                        zerocoords.push([r, c])
                    }
                }
            }
            if (!zerocoords) return undefined
            let choiceCoord = zerocoords[Math.floor(Math.random() * zerocoords.length)]
            let newNum = 2
            if (Math.random() < .1) {
                newNum = 4
            }
            if (choiceCoord)
                board[choiceCoord[0]][choiceCoord[1]] = newNum
        }
        switch (event.key) {
            case 'ArrowUp':
                board = moveUp(board)
                break
            case 'ArrowDown':
                board = moveDown(board)
                break
            case 'ArrowLeft':
                board = moveLeft(board)
                break
            case 'ArrowRight':
                board = moveRight(board)
                break
            case ' ':
                addNum()
                break
        }
        if (Math.random() < .25) addNum()
    })
})
let makeBoard

function setup() {
    makeBoard = function(numRows, numCols) {
        let flat = [2, 2, 2, 2]
        while (flat.length < numRows * numCols) flat.push(0)
        flat = shuffle(flat)
        let board = []
        for (let i = 0; i < numRows; i++) {
            row = []
            for (let j = 0; j < numCols; j++) {
                row.push(flat[i * numCols + j])
            }
            board.push(row)
        }
        return board
    }
    board = makeBoard(4, 4)
    createCanvas(400, 400);
    w = width / board[0].length
    h = height / board.length
    textAlign(CENTER, CENTER)
    textSize(32)
}

function draw() {
    background(255);
    //show board
    for (let r = 0; r < board.length; r++) {
        for (let c = 0; c < board[0].length; c++) {
            switch (board[r][c]) {
                case 0:
                    fill(200)
                    break
                case 2:
                    fill(180,180,0)
                    break
                case 4:
                    fill(128,23,0)
                    break
                case 8:
                    fill(180,0,0)
                    break
                case 16:
                    fill(220,0,0)
                    break
                case 32:
                    fill(255,0,0)
                    break
                case 64:
                    fill(255,255,0)
                    break
                case 128:
                    fill(128,255,0)
                    break
                case 256:
                    fill(0,255,255)
                    break
                case 512:
                    fill(0,0,255)
                    break
                case 1024:
                    fill(255,0,255)
                    break
                case 2048:
                    fill(0,128,128)
                    break
            }
            text(board[r][c], c * w + w / 2, r * h + h / 2)
        }
    }
}

function processReduced(arr_) {
    //will shift, process, and refill rows of arr (left justified)
    let arr = arr_.slice() //copy to not tamper with input array
    //filter 0s
    arr = arr.map(row => row.filter(e => e !== 0))


    //combine common pairs
    for (let row of arr) {
        for (let i = 0; i < row.length - 1; i++) {
            if (row[i] === row[i + 1]) {
                row[i] = row[i] + row[i]
                row[i + 1] = 0
            }
        }
    }
    //filter 0s again
    arr = arr.map(row => row.filter(e => e !== 0))
    // console.log('combined',arr)

    //pad with 0s
    for (let row of arr) {
        while (row.length < arr.length) {
            row.push(0)
        }
    }
    return arr
}

function moveUp(arr) {
    //transpose array to make it columns
    let cols = [] //an array of the columns of arr (transpose)
    for (let i = 0; i < arr[0].length; i++) { //assume arr rectangular
        let col = []
        for (let row of arr) {
            col.push(row[i])
        }
        cols.push(col)
    }
    // console.log('transposed',cols)
    //process
    cols = processReduced(cols)
    //retranspose
    let rows = []
    for (let i = 0; i < cols[0].length; i++) {
        let row = []
        for (let col of cols) {
            row.push(col[i])
        }
        rows.push(row)
    }
    // console.log('retransposed',rows)
    return rows
}

function moveDown(arr_) {
    let arr = arr_.slice()
    return moveUp(arr.reverse()).reverse()
}

function moveLeft(arr) {
    return processReduced(arr)
}

function moveRight(arr) {
    let rowsReversed = arr.map(row => row.reverse())
    return processReduced(rowsReversed).map(row => row.reverse())
}

// console.log(moveRight([
//   [0, 0, 8, 16],
//   [8, 0, 8, 2],
//   [0, 4, 8, 0],
//   [8, 0, 8, 2],
// ]))
// moveUp(arr)
