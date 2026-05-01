# Cards are integers 1..10. 1 = Ace; 10 = T/J/Q/K (4x weight).
# A shoe is a list with: cards (int vector), pos (next index to deal),
# cut_pos (index of cut card), running_count, system, n_decks.

new_shoe <- function(n_decks = 6, system = hi_lo_system(),
                     cut_fraction_range = c(0.80, 1.00)) {
  # 4 of each rank 1..9, 16 tens per deck
  one_deck <- c(rep(1:9, each = 4), rep(10, 16))
  cards <- sample(rep(one_deck, n_decks))
  n <- length(cards)
  cut_lo <- floor(cut_fraction_range[1] * n)
  cut_hi <- floor(cut_fraction_range[2] * n)
  cut_pos <- sample(cut_lo:cut_hi, 1)
  list(
    cards = cards,
    pos = 1L,
    cut_pos = cut_pos,
    running_count = 0L,
    system = system,
    n_decks = n_decks
  )
}

# Deal one card, updating position and running count.
deal_card <- function(shoe) {
  card <- shoe$cards[shoe$pos]
  shoe$pos <- shoe$pos + 1L
  shoe$running_count <- shoe$running_count +
    shoe$system$values[card]
  list(card = card, shoe = shoe)
}

# Deal a card without updating the running count (used for the dealer's
# hole card; count is updated when the card is revealed).
deal_card_hidden <- function(shoe) {
  card <- shoe$cards[shoe$pos]
  shoe$pos <- shoe$pos + 1L
  list(card = card, shoe = shoe)
}

reveal_card <- function(shoe, card) {
  shoe$running_count <- shoe$running_count + shoe$system$values[card]
  shoe
}

deal_n <- function(shoe, n) {
  out <- integer(n)
  for (i in seq_len(n)) {
    d <- deal_card(shoe)
    out[i] <- d$card
    shoe <- d$shoe
  }
  list(cards = out, shoe = shoe)
}

# Cards remaining and decks remaining (for true count).
cards_remaining <- function(shoe) length(shoe$cards) - shoe$pos + 1L
decks_remaining <- function(shoe) cards_remaining(shoe) / 52

# True count = running count / decks remaining (Hi-Lo style, balanced systems).
true_count <- function(shoe) {
  dr <- decks_remaining(shoe)
  if (dr <= 0) return(0)
  shoe$running_count / dr
}

# Has the cut card been reached?
shoe_exhausted <- function(shoe) shoe$pos > shoe$cut_pos
