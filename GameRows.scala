import scala.collection.mutable.ArrayBuffer

object  GameBoard extends App {
    
    val minRows = 6
    val minCols = 7
    val defaultRows = 6
    val defaultCols = 7
    var rows = defaultRows
    var cols = defaultCols
    var board = ArrayBuffer.fill(rows, cols)("◯")
    var player1:String = ""
    var player2:String = ""
    var history: ArrayBuffer[(String, Int)] = ArrayBuffer.empty[(String, Int)]

    def chooseDimensions(): Unit = {
        println("Enter the number of rows (minimum " + minRows + "):")
        val userRows = scala.io.StdIn.readInt()
        println("Enter the number of columns (minimum " + minCols + "):")
        val userCols = scala.io.StdIn.readInt()

        if (userRows < minRows || userCols < minCols) {
            println("The minimum allowed dimensions are " + minRows + " rows and " + minCols + " columns. Using default dimensions.")
        } else if (math.abs(userRows - userCols) > 2) {
            println("The number of rows and columns must not differ by more than 2. Using default dimensions.")
        } else {
            rows = userRows
            cols = userCols
            board = ArrayBuffer.fill(rows, cols)("◯")
        }
    }

    def drawBoard(): Unit = {
        for (i <- 0 until rows) {
            for (j <- 0 until cols) {
                print(board(i)(j) + " ")
            }
            println()
        }
    }

    def checkForWin(player: String): Boolean = {
        for (i <- 0 until rows) {
            for (j <- 0 until cols) {
                if (board(i)(j) == "⬤") {
                    if (checkVerticalWin(i, j) || checkHorizontalWin(i, j) || checkDiagonalWin(i, j) || checkDiagonalWin2(i,j)) {
                        return true
                    }
                } else if (board(i)(j) == "X") {
                    if (checkVerticalWin(i, j) || checkHorizontalWin(i, j) || checkDiagonalWin(i, j) || checkDiagonalWin(i,j)) {
                        return true
                    }
                }
            }
        }
        false
    }

    def checkVerticalWin(row: Int, col: Int): Boolean = {
        var count1 = 0
        var count2 = 0
        for (i <- row until rows) {
            if (board(i)(col) == "⬤") {
                count1 += 1
                count2 = 0
                if (count1 == 4) {
                    return true
                }
            } else if (board(i)(col) == "X") {
                count1 = 0
                count2 += 1
                if (count2 == 4) {
                    return true
                }
            } else{
                count1 = 0
                count2 = 0
            }
        }
        false
    }


    def checkHorizontalWin(row: Int, col: Int): Boolean = {
        var count1 = 0
        var count2 = 0
        for (j <- col until cols) {
            if (board(row)(j) == "⬤") {
                count2 = 0
                count1 += 1
                if (count1 == 4) {
                    return true
                }
            } else if (board(row)(j) == "X"){
                count1 = 0
                count2 += 1
                if (count2 == 4) {
                    return true
                }
            }else{
                count1 = 0
                count2 = 0
            }
        }
        false
    }
    // bottom left - top right
     def checkDiagonalWin2(row: Int, col: Int): Boolean = {
        var count1 = 0
        var count2 = 0
        var i = row
        var j = col
        while (i < rows && j >= 0) {
            if (board(i)(j) == "⬤") {
                count1 += 1
                count2 = 0
                if (count1 == 4) {
                    return true
                }
            } else if (board(i)(j) == "X") {
                count2 += 1
                count1 = 0
                if (count2 == 4) {
                    return true
                }
            } else{
                count1 = 0
                count2 = 0
            }
            i += 1
            j -= 1
        }
        count1 = 0
        count2 = 0
        i = row
        j = col
        while (i >= 0 && j < cols) {
            if (board(i)(j) == "⬤") {
                count1 += 1
                count2 = 0
                if (count1 == 4) {
                    return true
                }
            } else if (board(i)(j) == "X") {
                count2 += 1
                count1 = 0
                if (count2 == 4) {
                    return true
                }
            } else{
                count1 = 0
                count2 = 0
            }
            i -= 1
            j += 1
        }
        false
    }

    //top right - bottom left
    def checkDiagonalWin(row: Int, col: Int): Boolean = {
        var count1 = 0
        var count2 = 0
        var i = row
        var j = col
        while (i < rows && j < cols) {
            if (board(i)(j) == "⬤") {
                count1 += 1
                count2 = 0
                if (count1 == 4) {
                    return true
                }
            } else if (board(i)(j) == "X") {
                count2 += 1
                count1 = 0
                if (count2 == 4) {
                    return true
                }
            } else{
                count1 = 0
                count2 = 0
            }
            i += 1
            j += 1
        }
        count1 = 0
        count2 = 0
        i = row
        j = col
        while (i >= 0 && j >= 0) {
            if (board(i)(j) == "⬤") {
                count1 += 1
                count2 = 0
                if (count1 == 4) {
                    return true
                }
            } else if (board(i)(j) == "X") {
                count2 += 1
                count1 = 0
                if (count2 == 4) {
                    return true
                }
            } else{
                count1 = 0
                count2 = 0
            }
            i -= 1
            j -= 1
        }
        false
    }

    def start(): Boolean = {
        println("Enter name of player 1:")
        player1 = scala.io.StdIn.readLine()
        println("Enter name of player 2:")
        player2 = scala.io.StdIn.readLine()
        var currentPlayer = player1
        var gameOver = false
        var column: Int = 0
        while (!gameOver) {
            var invalidInput = true
              println("Moves history:")
                for (i <- history.indices) {
                    println("Player: " + history(i)._1 + ", Column: " + history(i)._2)
                }
            while(invalidInput){
                println(currentPlayer + ", select a column (1-" + cols + ") to drop your piece:")
                column = scala.io.StdIn.readInt()
                if(column > cols){
                    println("Invalid Column, please select column between 1 and "+cols)
                }else{
                    invalidInput = false
                }
            }
            var placed = false
            for (i <- rows - 1 to 0 by -1) {
                if (board(i)(column - 1) == "◯" && !placed) {
                    if(currentPlayer == player1){
                        board(i)(column - 1) = "⬤"
                    }else{
                        board(i)(column - 1) = "X"
                    }
                    history += Tuple2(currentPlayer, column)
                    placed = true
                }
            }
            if (!placed) {
                println("This column is full, please select another column")
            }else{
                drawBoard()
                if (checkForWin(currentPlayer)) {
                    println(currentPlayer + " wins!")
                    gameOver = true
                    return true
                } else {
                    if (currentPlayer == player1) {
                        currentPlayer = player2
                    } else {
                        currentPlayer = player1
                    }
                }
            }
        }
        return false
    } 

    chooseDimensions()
    start()
}