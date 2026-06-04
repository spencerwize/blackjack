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

# Tiered ramp:
#   TC < 1        -> unit * 0.5
#   1 <= TC < 2   -> unit
#   2 <= TC < 3   -> unit * 2
#   3 <= TC < 4   -> unit * 3
#   TC >= 4       -> unit * 4
tiered_ramp_bet <- function(unit = 10) {
  force(unit)
  function(tc, rc, ...) {
    if (tc < 1) return(unit * 0.5)
    if (tc < 2) return(unit)
    if (tc < 3) return(unit * 2)
    if (tc < 4) return(unit * 3)
    unit * 4
  }
}
