# Hand utilities. A hand is an integer vector of card ranks (1=Ace, 10=T/J/Q/K).

hand_total <- function(cards) {
  # Treat aces as 11 when possible without busting.
  base <- sum(ifelse(cards == 1, 1, cards))
  n_aces <- sum(cards == 1)
  total <- base
  # Promote one ace at a time from 1->11 (i.e. add 10) while it fits.
  for (i in seq_len(n_aces)) {
    if (total + 10 <= 21) total <- total + 10 else break
  }
  total
}

hand_is_soft <- function(cards) {
  base <- sum(ifelse(cards == 1, 1, cards))
  n_aces <- sum(cards == 1)
  if (n_aces == 0) return(FALSE)
  # Soft if at least one ace is being counted as 11.
  base + 10 <= 21
}

hand_is_blackjack <- function(cards) {
  length(cards) == 2 && hand_total(cards) == 21
}

hand_is_pair <- function(cards) {
  length(cards) == 2 && cards[1] == cards[2]
}

hand_busted <- function(cards) hand_total(cards) > 21
