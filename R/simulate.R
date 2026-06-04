# Top-level simulator. Runs `n_rounds` rounds, reshuffling whenever the cut
# card is reached at the end of a hand. Tracks bankroll, true count at bet
# time, hand outcomes, and produces a summary.

simulate_blackjack <- function(n_rounds       = 10000,
                               n_decks        = 6,
                               counting       = "Hi-Lo",
                               betting        = spread_bet(),
                               rules          = default_rules(),
                               min_bet        = 1,
                               max_bet        = 500,
                               cut_range      = c(0.80, 1.00),
                               start_bankroll = 0,
                               seed           = NULL,
                               verbose        = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  system <- if (is.character(counting)) get_system(counting, n_decks) else counting

  shoe <- new_shoe(n_decks, system, cut_range)
  if (!system$balanced) shoe$running_count <- system$ihc

  net_per_round    <- numeric(n_rounds)
  bet_per_round    <- numeric(n_rounds)
  tc_per_round     <- numeric(n_rounds)
  rc_per_round     <- numeric(n_rounds)
  hands_per_round  <- integer(n_rounds)
  shoe_id_per_round <- integer(n_rounds)
  current_shoe_id  <- 1L

  bankroll <- start_bankroll

  for (i in seq_len(n_rounds)) {
    # Need enough cards to start a hand; reshuffle if cut card already passed.
    if (shoe_exhausted(shoe) || cards_remaining(shoe) < 15) {
      shoe <- new_shoe(n_decks, system, cut_range)
      if (!system$balanced) shoe$running_count <- system$ihc
      current_shoe_id <- current_shoe_id + 1L
    }
    res <- play_round(shoe, betting, rules, min_bet, max_bet)
    shoe <- res$shoe
    net_per_round[i]      <- res$net
    bet_per_round[i]      <- res$bet
    tc_per_round[i]       <- res$tc_at_bet
    rc_per_round[i]       <- res$rc_at_bet
    hands_per_round[i]    <- res$n_hands
    shoe_id_per_round[i]  <- current_shoe_id
    bankroll <- bankroll + res$net
    if (verbose && i %% 1000 == 0) {
      message(sprintf("Round %d: bankroll=%.2f", i, bankroll))
    }
  }

  total_action <- sum(bet_per_round)
  total_net    <- sum(net_per_round)
  list(
    summary = list(
      counting_system  = system$name,
      rounds           = n_rounds,
      shoes_played     = current_shoe_id,
      avg_rounds_per_shoe = n_rounds / current_shoe_id,
      total_wagered    = total_action,
      total_net        = total_net,
      ev_per_round     = mean(net_per_round),
      ev_per_unit      = if (total_action > 0) total_net / total_action else NA_real_,
      win_rate_hands   = mean(net_per_round > 0),
      avg_bet          = mean(bet_per_round[bet_per_round > 0]),
      bankroll         = bankroll
    ),
    rounds = data.frame(
      round          = seq_len(n_rounds),
      shoe_id        = shoe_id_per_round,
      true_count     = tc_per_round,
      running_cnt    = rc_per_round,
      bet            = bet_per_round,
      net            = net_per_round,
      n_player_hands = hands_per_round
    )
  )
}

# Run the tiered ramp over n_rounds and return cumulative bet + net totals.
# Use to size a bankroll: how much money goes across the felt over N hands
# at a given unit, and what the EV looks like.
theoretical_total <- function(unit       = 10,
                              n_rounds   = 10000,
                              n_decks    = 6,
                              counting   = "Hi-Lo",
                              rules      = default_rules(),
                              max_units  = 10,
                              min_bet    = unit * 0.5,
                              max_bet    = unit * max_units,
                              seed       = NULL) {
  res <- simulate_blackjack(
    n_rounds = n_rounds, n_decks = n_decks, counting = counting,
    betting  = tiered_ramp_bet(unit, max_units = max_units),
    rules    = rules, min_bet = min_bet, max_bet = max_bet, seed = seed
  )
  r <- res$rounds
  r$cum_wagered <- cumsum(r$bet)
  r$cum_net     <- cumsum(r$net)
  list(
    unit             = unit,
    rounds           = n_rounds,
    total_wagered    = sum(r$bet),
    total_net        = sum(r$net),
    avg_bet          = mean(r$bet),
    bets_by_tier     = c(
      "TC<1"    = sum(r$bet[r$true_count <  1]),
      "TC 1-2"  = sum(r$bet[r$true_count >= 1 & r$true_count < 2]),
      "TC 2-3"  = sum(r$bet[r$true_count >= 2 & r$true_count < 3]),
      "TC 3-4"  = sum(r$bet[r$true_count >= 3 & r$true_count < 4]),
      "TC>=4"   = sum(r$bet[r$true_count >= 4])
    ),
    hands_by_tier    = c(
      "TC<1"    = sum(r$true_count <  1),
      "TC 1-2"  = sum(r$true_count >= 1 & r$true_count < 2),
      "TC 2-3"  = sum(r$true_count >= 2 & r$true_count < 3),
      "TC 3-4"  = sum(r$true_count >= 3 & r$true_count < 4),
      "TC>=4"   = sum(r$true_count >= 4)
    ),
    cum_wagered      = cumsum(r$bet),
    cum_net          = cumsum(r$net),
    rounds_df        = r
  )
}

# Breakdown of outcomes by category. Helps localize bugs.
diagnose <- function(n_rounds = 100000, seed = 1) {
  if (!is.null(seed)) set.seed(seed)
  system <- hi_lo_system()
  shoe <- new_shoe(6, system)
  rules <- default_rules()
  fb <- flat_bet(1)
  player_bj <- 0L; dealer_bj <- 0L; both_bj <- 0L
  net_by_outcome <- list(win = 0, lose = 0, push = 0)
  hands_total <- 0L
  net_total <- 0
  for (i in seq_len(n_rounds)) {
    if (shoe_exhausted(shoe) || cards_remaining(shoe) < 15) {
      shoe <- new_shoe(6, system)
    }
    # Peek before play to detect naturals.
    pos0 <- shoe$pos
    p1 <- shoe$cards[pos0]; up <- shoe$cards[pos0 + 1L]
    p2 <- shoe$cards[pos0 + 2L]; hole <- shoe$cards[pos0 + 3L]
    pbj <- hand_is_blackjack(c(p1, p2))
    dbj <- (up == 1 || up == 10) && hand_is_blackjack(c(up, hole))
    if (pbj) player_bj <- player_bj + 1L
    if (dbj) dealer_bj <- dealer_bj + 1L
    if (pbj && dbj) both_bj <- both_bj + 1L
    res <- play_round(shoe, fb, rules, 1, 1)
    shoe <- res$shoe
    net_total <- net_total + res$net
    hands_total <- hands_total + res$n_hands
    if (res$net > 0) net_by_outcome$win  <- net_by_outcome$win  + res$net
    else if (res$net < 0) net_by_outcome$lose <- net_by_outcome$lose + res$net
    else net_by_outcome$push <- net_by_outcome$push + 1
  }
  cat(sprintf(
    "Rounds=%d hands=%d net=%.1f edge=%.3f%%\n",
    n_rounds, hands_total, net_total, 100 * net_total / n_rounds))
  cat(sprintf(
    "Player BJ: %d (%.2f%%) — expected ~4.74%%\n",
    player_bj, 100 * player_bj / n_rounds))
  cat(sprintf(
    "Dealer BJ: %d (%.2f%%) — expected ~4.74%%\n",
    dealer_bj, 100 * dealer_bj / n_rounds))
  cat(sprintf(
    "Both BJ:   %d (%.2f%%)\n", both_bj, 100 * both_bj / n_rounds))
  cat(sprintf("Sum of winning net:  %.1f\n", net_by_outcome$win))
  cat(sprintf("Sum of losing  net:  %.1f\n", net_by_outcome$lose))
  invisible(list(rounds = n_rounds, hands = hands_total,
                 net = net_total, player_bj = player_bj,
                 dealer_bj = dealer_bj, both_bj = both_bj))
}

# Flat-bet sanity check. Returns empirical house edge as a % of action.
# At 6 decks S17 DAS no-surrender, expectation is around -0.45% +/- a few
# bps of sim noise. Anything far from that points to a bug.
sanity_check <- function(n_rounds = 200000, n_decks = 6, seed = 1) {
  res <- simulate_blackjack(
    n_rounds = n_rounds, n_decks = n_decks, counting = "Hi-Lo",
    betting  = flat_bet(min_bet = 1),
    min_bet = 1, max_bet = 1, seed = seed
  )
  edge_pct <- 100 * res$summary$total_net / res$summary$total_wagered
  cat(sprintf(
    "Flat-bet sim: %d rounds, %d shoes\n  wagered=%.0f  net=%.1f  edge=%.3f%%\n  expected ~ -0.45%% at 6-deck S17 DAS\n",
    res$summary$rounds, res$summary$shoes_played,
    res$summary$total_wagered, res$summary$total_net, edge_pct))
  invisible(res$summary)
}

# Source all source files in one go.
source_all <- function(dir = "R") {
  files <- c("counting_systems.R", "cards.R", "hand.R", "basic_strategy.R",
             "betting.R", "play.R", "simulate.R")
  for (f in files) source(file.path(dir, f))
  invisible(TRUE)
}
