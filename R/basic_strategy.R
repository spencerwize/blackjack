# Basic strategy for 6-deck, dealer Stands on Soft 17 (S17), Double After Split
# allowed (DAS), no surrender, no insurance (handled separately by the count).
#
# Dealer upcard index: 1..10 (1 = Ace, 10 = T/J/Q/K).
#
# Actions:
#   "H"  hit
#   "S"  stand
#   "D"  double if first two cards (else hit)
#   "Ds" double if allowed (else stand)
#   "P"  split (only valid for pairs)

# --- Hard totals 5..21 vs dealer upcard 1..10 ----------------------------
.hard_strategy <- function() {
  m <- matrix("H", nrow = 21 - 5 + 1, ncol = 10,
              dimnames = list(as.character(5:21),
                              c("A", as.character(2:10))))
  set <- function(total, ups, action) {
    m[as.character(total), ups] <<- action
  }
  # 5-8 always hit (default).
  # 9
  set(9, c("3","4","5","6"), "D")
  # 10
  set(10, c("2","3","4","5","6","7","8","9"), "D")
  # 11
  set(11, c("2","3","4","5","6","7","8","9","10"), "D")
  # 12: stand vs 4-6
  m["12", ] <- "H"; set(12, c("4","5","6"), "S")
  # 13-16: stand vs 2-6
  for (t in 13:16) {
    m[as.character(t), ] <- "H"
    set(t, c("2","3","4","5","6"), "S")
  }
  # 17-21: always stand
  for (t in 17:21) m[as.character(t), ] <- "S"
  m
}

# --- Soft totals 13..21 (A+2 .. A+10) ------------------------------------
.soft_strategy <- function() {
  m <- matrix("H", nrow = 21 - 13 + 1, ncol = 10,
              dimnames = list(as.character(13:21),
                              c("A", as.character(2:10))))
  set <- function(total, ups, action) {
    m[as.character(total), ups] <<- action
  }
  # A,2 (13): D vs 5-6
  set(13, c("5","6"), "D")
  # A,3 (14): D vs 5-6
  set(14, c("5","6"), "D")
  # A,4 (15): D vs 4-6
  set(15, c("4","5","6"), "D")
  # A,5 (16): D vs 4-6
  set(16, c("4","5","6"), "D")
  # A,6 (17): D vs 3-6
  set(17, c("3","4","5","6"), "D")
  # A,7 (18): S vs 2,7,8; Ds vs 3-6; H vs 9,10,A
  m["18", ] <- "H"
  set(18, c("2","7","8"), "S")
  set(18, c("3","4","5","6"), "Ds")
  # A,8 (19): S, but Ds vs 6 in some charts; standard S17 = stand.
  m["19", ] <- "S"
  # A,9 (20), A,10 (21): stand
  m["20", ] <- "S"; m["21", ] <- "S"
  m
}

# --- Pair splits: rows are pair card 1..10 -------------------------------
.pair_strategy <- function() {
  m <- matrix("N", nrow = 10, ncol = 10,
              dimnames = list(c("A", as.character(2:10)),
                              c("A", as.character(2:10))))
  setr <- function(rowname, ups, action) m[rowname, ups] <<- action
  # A,A: always split
  m["A", ] <- "P"
  # 2,2 and 3,3: P vs 2-7
  for (r in c("2","3")) { m[r, ] <- "N"; setr(r, c("2","3","4","5","6","7"), "P") }
  # 4,4: P vs 5,6 (DAS)
  m["4", ] <- "N"; setr("4", c("5","6"), "P")
  # 5,5: never split (treat as hard 10 elsewhere)
  m["5", ] <- "N"
  # 6,6: P vs 2-6
  m["6", ] <- "N"; setr("6", c("2","3","4","5","6"), "P")
  # 7,7: P vs 2-7
  m["7", ] <- "N"; setr("7", c("2","3","4","5","6","7"), "P")
  # 8,8: always split
  m["8", ] <- "P"
  # 9,9: P vs 2-6, 8, 9; stand vs 7, 10, A
  m["9", ] <- "N"; setr("9", c("2","3","4","5","6","8","9"), "P")
  # 10,10: never split
  m["10", ] <- "N"
  m
}

HARD_STRAT <- .hard_strategy()
SOFT_STRAT <- .soft_strategy()
PAIR_STRAT <- .pair_strategy()

.up_col <- function(up) if (up == 1) "A" else as.character(up)

# Decide action given a hand and dealer upcard.
# can_double, can_split: whether those actions are currently legal.
basic_action <- function(cards, up, can_double = TRUE, can_split = TRUE) {
  col <- .up_col(up)
  # Pair?
  if (can_split && hand_is_pair(cards)) {
    rn <- if (cards[1] == 1) "A" else as.character(cards[1])
    a <- PAIR_STRAT[rn, col]
    if (a == "P") return("P")
  }
  total <- hand_total(cards)
  if (hand_is_soft(cards)) {
    if (total >= 13 && total <= 21) {
      a <- SOFT_STRAT[as.character(total), col]
    } else {
      a <- "H"
    }
  } else {
    if (total < 5) total <- 5
    if (total > 21) return("S")  # already busted; shouldn't be queried
    a <- HARD_STRAT[as.character(total), col]
  }
  # Translate D / Ds based on legality.
  if (a == "D")  return(if (can_double) "D" else "H")
  if (a == "Ds") return(if (can_double) "D" else "S")
  a
}
