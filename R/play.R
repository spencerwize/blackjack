# Play a single round of blackjack and return per-hand outcomes.
# Returns: list(net = numeric net win/loss, shoe = updated shoe,
#               bet = base bet, n_hands = number of player hands resolved).

# Rules (configurable via opts):
#   * 6 decks, S17, DAS, blackjack pays 3:2.
#   * Double on any two cards.
#   * Split up to 3 times (4 hands). Split aces get one card each, no resplit.
#   * No surrender. Insurance optional, controlled by `insurance_tc`.

default_rules <- function() {
  list(
    bj_payout       = 1.5,
    max_splits      = 3,
    das             = TRUE,
    s17             = TRUE,
    insurance_tc    = Inf,   # take insurance only if true count >= this
    resplit_aces    = FALSE
  )
}

# Play out the dealer's hand. Returns updated dealer cards and shoe.
.play_dealer <- function(dealer_cards, hole_card, shoe, rules) {
  shoe <- reveal_card(shoe, hole_card)
  dealer_cards <- c(dealer_cards, hole_card)
  repeat {
    total <- hand_total(dealer_cards)
    soft  <- hand_is_soft(dealer_cards)
    if (total > 21) break
    if (total >= 18) break
    if (total == 17) {
      if (rules$s17 || !soft) break
      # H17: hit soft 17
    }
    d <- deal_card(shoe)
    dealer_cards <- c(dealer_cards, d$card)
    shoe <- d$shoe
  }
  list(cards = dealer_cards, shoe = shoe)
}

# Play out a single player hand. Returns final cards, final bet, and shoe.
# `from_split_ace` => one card only, no further actions.
.play_player_hand <- function(cards, bet, up, shoe, rules,
                              from_split_ace = FALSE,
                              can_split_more = TRUE) {
  if (from_split_ace) {
    # Take exactly one card if not already 2 cards.
    if (length(cards) < 2) {
      d <- deal_card(shoe); cards <- c(cards, d$card); shoe <- d$shoe
    }
    return(list(cards = cards, bet = bet, shoe = shoe, doubled = FALSE,
                split_request = NULL))
  }
  doubled <- FALSE
  repeat {
    if (hand_busted(cards)) break
    can_double <- length(cards) == 2
    can_split  <- can_split_more && hand_is_pair(cards)
    a <- basic_action(cards, up, can_double = can_double, can_split = can_split)
    if (a == "S") break
    if (a == "P") {
      # Signal to caller to handle the split.
      return(list(cards = cards, bet = bet, shoe = shoe, doubled = FALSE,
                  split_request = TRUE))
    }
    if (a == "D") {
      d <- deal_card(shoe); cards <- c(cards, d$card); shoe <- d$shoe
      bet <- bet * 2; doubled <- TRUE
      break
    }
    # Hit
    d <- deal_card(shoe); cards <- c(cards, d$card); shoe <- d$shoe
  }
  list(cards = cards, bet = bet, shoe = shoe, doubled = doubled,
       split_request = NULL)
}

# Settle one player hand against the dealer total. Returns net for the hand.
.settle <- function(player_cards, bet, dealer_cards, rules,
                    is_blackjack = FALSE) {
  pt <- hand_total(player_cards)
  dt <- hand_total(dealer_cards)
  if (is_blackjack) {
    # Caller handles BJ vs BJ push before reaching here.
    return(bet * rules$bj_payout)
  }
  if (pt > 21) return(-bet)
  if (dt > 21) return(+bet)
  if (pt > dt)  return(+bet)
  if (pt < dt)  return(-bet)
  0  # push
}

play_round <- function(shoe, bet_fn, rules = default_rules(),
                       min_bet = 1, max_bet = 1e6) {
  # Determine bet size from current count BEFORE any cards leave the shoe.
  tc <- true_count(shoe); rc <- shoe$running_count
  bet <- bet_fn(tc = tc, rc = rc, min_bet = min_bet, max_bet = max_bet)
  if (bet <= 0) {
    # Sit out: still need to burn no cards. Just return.
    return(list(net = 0, shoe = shoe, bet = 0, n_hands = 0,
                tc_at_bet = tc, rc_at_bet = rc))
  }
  bet <- max(min_bet, min(bet, max_bet))

  # Deal: player, dealer up, player, dealer hole (hole uncounted until reveal).
  d1 <- deal_card(shoe);        shoe <- d1$shoe
  d2 <- deal_card(shoe);        shoe <- d2$shoe   # dealer up
  d3 <- deal_card(shoe);        shoe <- d3$shoe
  d4 <- deal_card_hidden(shoe); shoe <- d4$shoe   # dealer hole
  player <- c(d1$card, d3$card)
  up     <- d2$card
  hole   <- d4$card

  player_bj <- hand_is_blackjack(player)
  dealer_bj <- (up == 1 || up == 10) &&
               hand_is_blackjack(c(up, hole))

  # Insurance decision (only when dealer up is Ace).
  insurance_net <- 0
  if (up == 1 && tc >= rules$insurance_tc) {
    ins_bet <- bet / 2
    if (dealer_bj) insurance_net <- +2 * ins_bet else insurance_net <- -ins_bet
  }

  # Resolve naturals.
  if (dealer_bj) {
    shoe <- reveal_card(shoe, hole)
    if (player_bj) {
      return(list(net = 0 + insurance_net, shoe = shoe, bet = bet,
                  n_hands = 1, tc_at_bet = tc, rc_at_bet = rc))
    }
    return(list(net = -bet + insurance_net, shoe = shoe, bet = bet,
                n_hands = 1, tc_at_bet = tc, rc_at_bet = rc))
  }
  if (player_bj) {
    shoe <- reveal_card(shoe, hole)
    return(list(net = bet * rules$bj_payout + insurance_net, shoe = shoe,
                bet = bet, n_hands = 1, tc_at_bet = tc, rc_at_bet = rc))
  }

  # Iterative split handling. Maintain a queue of pending hands.
  hands <- list(list(cards = player, bet = bet, splits_used = 0L,
                     from_split_ace = FALSE))
  finished <- list()
  while (length(hands) > 0) {
    h <- hands[[1]]; hands <- hands[-1]
    can_split_more <- h$splits_used < rules$max_splits &&
                      !h$from_split_ace
    res <- .play_player_hand(h$cards, h$bet, up, shoe, rules,
                             from_split_ace = h$from_split_ace,
                             can_split_more = can_split_more)
    shoe <- res$shoe
    if (isTRUE(res$split_request)) {
      # Split: each card becomes the start of a new hand, dealt one more card.
      c1 <- h$cards[1]; c2 <- h$cards[2]
      is_ace_split <- (c1 == 1)
      d_a <- deal_card(shoe); shoe <- d_a$shoe
      d_b <- deal_card(shoe); shoe <- d_b$shoe
      new_a <- list(cards = c(c1, d_a$card), bet = h$bet,
                    splits_used = h$splits_used + 1L,
                    from_split_ace = is_ace_split && !rules$resplit_aces)
      new_b <- list(cards = c(c2, d_b$card), bet = h$bet,
                    splits_used = h$splits_used + 1L,
                    from_split_ace = is_ace_split && !rules$resplit_aces)
      hands <- c(list(new_a, new_b), hands)
    } else {
      finished <- c(finished, list(list(cards = res$cards, bet = res$bet)))
    }
  }

  # Dealer plays only if at least one hand isn't busted.
  any_alive <- any(vapply(finished, function(f) !hand_busted(f$cards),
                          logical(1)))
  if (any_alive) {
    dr <- .play_dealer(c(up), hole, shoe, rules)
    dealer_cards <- dr$cards; shoe <- dr$shoe
  } else {
    shoe <- reveal_card(shoe, hole)
    dealer_cards <- c(up, hole)
  }

  net <- 0
  for (f in finished) {
    net <- net + .settle(f$cards, f$bet, dealer_cards, rules)
  }
  list(net = net + insurance_net, shoe = shoe, bet = bet,
       n_hands = length(finished), tc_at_bet = tc, rc_at_bet = rc)
}
