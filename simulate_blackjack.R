# Example driver. Run with: Rscript simulate_blackjack.R
# Sources all modules in R/ and compares a few counting + betting strategies.

source("R/counting_systems.R")
source("R/cards.R")
source("R/hand.R")
source("R/basic_strategy.R")
source("R/betting.R")
source("R/play.R")
source("R/simulate.R")

run_one <- function(label, counting, betting, n = 50000, seed = 1) {
  cat(sprintf("\n=== %s ===\n", label))
  res <- simulate_blackjack(
    n_rounds = n,
    counting = counting,
    betting  = betting,
    rules    = modifyList(default_rules(), list(insurance_tc = 3)),
    seed     = seed
  )
  s <- res$summary
  cat(sprintf("rounds=%d shoes=%d wagered=%.0f net=%.1f ev/round=%.4f ev/unit=%.4f%%\n",
              s$rounds, s$shoes_played, s$total_wagered, s$total_net,
              s$ev_per_round, 100 * s$ev_per_unit))
  invisible(res)
}

run_one("Hi-Lo, flat $1",          "Hi-Lo",   flat_bet(1))
run_one("Hi-Lo, 1-12 spread",      "Hi-Lo",   spread_bet(unit = 1, threshold = 1, max_units = 12))
run_one("Hi-Lo, Wong out",         "Hi-Lo",   wong_bet(unit = 1, enter_tc = 1, max_units = 12))
run_one("KO, running-count bet",   "KO",      ko_running_bet(unit = 1, pivot = 4, max_units = 10))
run_one("Hi-Opt I, 1-12 spread",   "Hi-Opt I",spread_bet(unit = 1, threshold = 1, max_units = 12))
run_one("Omega II, 1-12 spread",   "Omega II",spread_bet(unit = 1, threshold = 2, max_units = 12))
