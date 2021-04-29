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
    raw_move <- sapply(strsplit(raw_input, ',')[[1]], trimws)
    if (length(raw_move) != 2) {
        return(FALSE)
    }
    are_valid_integers <- all(grepl("^[1-3]$", raw_move))
    if (!are_valid_integers) {
        return(FALSE)
    }
    move <- as.numeric(raw_move)
    is.na(board[move[1], move[2]])
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

is_winning_line <- function(x) {
    !anyNA(x) && length(unique(x)) == 1
}

is_game_over <- function(board) {
    neg_diag <- diag(board)
    pos_diag <- diag(apply(board, 2, rev))
    all_lines <- cbind(board, t(board), neg_diag, pos_diag)
    line_wins <- apply(all_lines, 2, is_winning_line)
    if (any(line_wins)) {
        i <- which(line_wins)[1]
        cat(all_lines[1, i], "won!")
        return(TRUE)
    }

    if (!anyNA(board)) {
        cat("Tie.")
        return(TRUE)
    }
    return(FALSE)
}

player_symbol <- prompt_player("x or o?", function(x) x != 'x' & x != 'o')
computer_symbol <- if (player_symbol == 'o') 'x' else 'o'
player_turn <- if (player_symbol == 'o') FALSE else TRUE
board <- matrix(nrow=3, ncol=3, byrow=TRUE)

while (!is_game_over(board)) {
    if (player_turn) {
        raw_input <- prompt_player(
            "Choose a move. Type row,col (e.g. 1,1):",
             function(x) !is_valid_move(x, board))
        move <- as.numeric(sapply(strsplit(raw_input, ',')[[1]], trimws))
        board[move[1], move[2]] <- player_symbol
    } else {
        pos <- generate_computer_move(board)
        board[pos[1], pos[2]] <- computer_symbol
        cat("Computer chooses:", pos[1], ",", pos[2], "\n")
    }
    player_turn <- !player_turn
    print(board)
}
