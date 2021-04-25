if (interactive()) {
      con <- stdin()
} else {
      con <- "stdin"
}

prompt_player <- function(prompt, check) {
    cat(prompt, '')
    input <- readLines(con = con, n = 1)
    while (check(input)) {
        cat(input, "is not a valid choice.\n")
        cat(prompt, '')
        input <- readLines(con = con, n = 1)
    }
    input
}

is_valid_move <- function(raw_input, board) {
    raw_move <- strsplit(raw_input, ',')[[1]]
    raw_move <- sapply(raw_move, trimws)
    if (length(raw_move) != 2) {
        return(FALSE)
    }
    move <- sapply(raw_move, as.numeric)
    if (anyNA(move)) {
        return(FALSE)
    }
    if (!all(sapply(move, function(x) 1 <= x & x <= 3))) {
        return(FALSE)
    }
    return(is.na(board[move[1], move[2]]))
}

generate_computer_move <- function(board) {
    found_move <- FALSE
    row <- 0
    col <- 0
    while (!found_move) {
        encoded_move <- sample(9, 1) - 1
        row <- (encoded_move %% 3) + 1
        col <- (encoded_move %/% 3) + 1
        found_move <- is.na(board[row, col])
    }
    c(row, col)
}

is_game_over <- function(board) {
    full_rows = apply(board, 1, function(x) !anyNA(x))
    for (row in 1:nrow(board)) {
        if (full_rows[row] & !is.na(row) & length(unique(board[row,])) == 1) {
            cat(board[row,1], "won!")
            return(TRUE)
        }
    }

    full_cols = apply(board, 2, function(x) !anyNA(x))
    for (col in 1:ncol(board)) {
        if (full_cols[col] & !is.na(col) & length(unique(board[,col])) == 1) {
            cat(board[1,col], "won!")
            return(TRUE)
        }
    }

    neg_diag = diag(board)
    if (!anyNA(neg_diag) & length(unique(neg_diag)) == 1) {
        cat(neg_diag[1], "won!")
        return(TRUE)
    }

    pos_diag = diag(apply(board, 2, rev))
    if (!anyNA(pos_diag) & length(unique(pos_diag)) == 1) {
        cat(pos_diag[1], "won!")
        return(TRUE)
    }

    if (!anyNA(board)) {
        cat("Tie.")
        return(TRUE)
    }
    return(FALSE)
}

player_symbol <- prompt_player("x or o?", function(x) x != 'x' & x != 'o')
computer_symbol = if (player_symbol == 'o') 'x' else 'o'
player_turn = if (player_symbol == 'o') FALSE else TRUE
board = matrix(nrow=3, ncol=3, byrow=TRUE)

while (!is_game_over(board)) {
    if (player_turn) {
        raw_input <- prompt_player(
            "Choose a move. Type row,col (e.g. 1,1):",
             function(x) !is_valid_move(x, board))
        move <- sapply(
            strsplit(raw_input, ',')[[1]],
            function(x) as.numeric(trimws(x)))
        board[move[1], move[2]] <- player_symbol
    } else {
        pos <- generate_computer_move(board)
        board[pos[1], pos[2]] <- computer_symbol
        cat("Computer chooses:", pos[1], ",", pos[2], "\n")
    }
    player_turn = !player_turn
    print(board)
}
