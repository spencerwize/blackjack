# Each system: a vector `values` indexed by card rank 1..10.
# `balanced` = TRUE/FALSE. `name` for reporting.
# True-count divisor: balanced -> running / decks_remaining.
# Unbalanced (KO): use running count directly, with system-specific pivot.

make_system <- function(name, values, balanced = TRUE, ihc = 0L) {
  stopifnot(length(values) == 10)
  list(name = name, values = as.integer(values),
       balanced = balanced, ihc = ihc)
}

hi_lo_system <- function() {
  # A,2,3,4,5,6,7,8,9,10
  make_system("Hi-Lo",
              c(-1, +1, +1, +1, +1, +1, 0, 0, 0, -1),
              balanced = TRUE)
}

ko_system <- function(n_decks = 6) {
  # KO is unbalanced. Initial Running Count = 4 - 4*n_decks.
  make_system("KO",
              c(-1, +1, +1, +1, +1, +1, +1, 0, 0, -1),
              balanced = FALSE,
              ihc = 4L - 4L * n_decks)
}

hi_opt_i_system <- function() {
  make_system("Hi-Opt I",
              c(0, 0, +1, +1, +1, +1, 0, 0, 0, -1),
              balanced = TRUE)
}

omega_ii_system <- function() {
  make_system("Omega II",
              c(0, +1, +1, +2, +2, +2, +1, 0, -1, -2),
              balanced = TRUE)
}

# Convenience: count name -> system (need n_decks for KO).
get_system <- function(name, n_decks = 6) {
  switch(tolower(name),
         "hi-lo"    = hi_lo_system(),
         "ko"       = ko_system(n_decks),
         "hi-opt i" = hi_opt_i_system(),
         "omega ii" = omega_ii_system(),
         stop("Unknown counting system: ", name))
}
