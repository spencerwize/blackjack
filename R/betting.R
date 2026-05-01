# Betting strategies. Each is a function(true_count, running_count, min_bet, max_bet)
# returning the bet size (>= 0; 0 means sit out the hand).

flat_bet <- function(min_bet = 1, max_bet = NULL) {
  force(min_bet)
  function(tc, rc, ...) min_bet
}

# Classic 1-N spread keyed on true count.
# Bet = unit * max(1, floor(tc) - threshold), capped at max_units.
spread_bet <- function(unit = 1, threshold = 1, max_units = 12,
                       sit_below = -Inf) {
  force(unit); force(threshold); force(max_units); force(sit_below)
  function(tc, rc, ...) {
    if (tc < sit_below) return(0)
    units <- floor(tc) - threshold
    if (units < 1) units <- 1
    if (units > max_units) units <- max_units
    unit * units
  }
}

# Wong-style: sit out hands when count is unfavorable, ramp up otherwise.
wong_bet <- function(unit = 1, enter_tc = 1, max_units = 12) {
  force(unit); force(enter_tc); force(max_units)
  function(tc, rc, ...) {
    if (tc < enter_tc) return(0)
    units <- min(max_units, floor(tc))
    unit * max(1, units)
  }
}

# KO-style on running count (for unbalanced systems).
ko_running_bet <- function(unit = 1, pivot = 0, max_units = 10) {
  force(unit); force(pivot); force(max_units)
  function(tc, rc, ...) {
    units <- rc - pivot
    if (units < 1) units <- 1
    if (units > max_units) units <- max_units
    unit * units
  }
}
