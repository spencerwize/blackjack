# Blackjack practice app (Shiny).
#
# Run with:
#   shiny::runApp("app.R")
# or
#   Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
#
# What it does:
#   * Deals real 6-deck shoe blackjack with an unseen running count.
#   * You bet, then play Hit / Stand / Double / Split.
#   * Plays are graded silently against basic strategy; mistakes only
#     show up in the shoe-end report.
#   * Bets are graded against a Hi-Lo target (from the hidden count).
#   * When the shoe reshuffles, you're quizzed on the final running and
#     true count.

suppressPackageStartupMessages(library(shiny))

source("R/cards.R")
source("R/counting_systems.R")
source("R/hand.R")
source("R/basic_strategy.R")
source("R/betting.R")

# ---------- helpers ------------------------------------------------------

SUITS <- c("♠", "♥", "♦", "♣")  # spade heart diamond club

rank_label <- function(r) {
  if (r == 1) "A" else if (r == 10) "10" else as.character(r)
}

card_html <- function(rank, suit, hidden = FALSE) {
  if (isTRUE(hidden)) {
    return('<div class="card back"></div>')
  }
  red <- suit %in% c("♥", "♦")
  cls <- paste0("card", if (red) " red" else "")
  sprintf(
    '<div class="%s"><span class="r">%s</span><span class="s">%s</span></div>',
    cls, rank_label(rank), suit
  )
}

cards_html <- function(cards, suits, hide_index = NULL) {
  if (length(cards) == 0) return("")
  pieces <- vapply(seq_along(cards), function(i) {
    card_html(cards[i], suits[i], hidden = !is.null(hide_index) && i == hide_index)
  }, character(1))
  paste0('<div class="hand">', paste(pieces, collapse = ""), "</div>")
}

# Hi-Lo target bet: 1 unit at TC<=1, then floor(TC)*unit up to max.
target_bet <- function(tc, unit = 5, max_units = 12) {
  if (tc < 1) return(unit)
  unit * min(max_units, floor(tc))
}

# Empty stats tally.
empty_stats <- function() {
  list(
    decisions_total = 0L,
    decisions_correct = 0L,
    decisions_log = list(),       # list of (action, expected, hand, up, ok)
    bets_total = 0L,
    bets_correct = 0L,            # within +/-1 unit of target
    bet_log = list(),
    count_quizzes = 0L,
    count_quiz_ok = 0L,
    rounds = 0L,
    bankroll = 0,
    rc_actual_at_quiz = NA_integer_
  )
}

# ---------- UI -----------------------------------------------------------

css <- "
body { font-family: system-ui, -apple-system, Segoe UI, sans-serif;
       background: #0b3d2e; color: #f5f5f5; }
.card { display:inline-block; width: 70px; height: 100px;
        background: white; color: black; border-radius: 8px;
        margin: 4px; padding: 6px; vertical-align: top;
        box-shadow: 2px 2px 4px rgba(0,0,0,0.4); position: relative; }
.card.red { color: #c00; }
.card.back { background: repeating-linear-gradient(
        45deg, #225, #225 6px, #336 6px, #336 12px); }
.card .r { display:block; font-size: 20px; font-weight: 700; }
.card .s { display:block; font-size: 28px; text-align: right; }
.hand { margin: 8px 0; }
.hand-active { outline: 3px solid gold; padding: 4px; border-radius: 8px; }
.panel { background: rgba(0,0,0,0.3); padding: 12px; border-radius: 8px;
         margin-bottom: 12px; }
.btn { margin: 4px; padding: 8px 14px; font-size: 14px; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css)), tags$title("Blackjack Practice")),
  titlePanel("Blackjack Practice"),
  fluidRow(
    column(8,
      div(class = "panel",
          h4("Dealer"),
          uiOutput("dealer_cards"),
          textOutput("dealer_total")
      ),
      div(class = "panel",
          h4("You"),
          uiOutput("player_hands"),
          textOutput("player_msg")
      ),
      div(class = "panel",
          uiOutput("round_review")
      ),
      div(class = "panel",
          uiOutput("controls")
      )
    ),
    column(4,
      div(class = "panel",
          h4("Stats (this session)"),
          tableOutput("stats_tbl")
      ),
      div(class = "panel",
          h4("Last shoe"),
          verbatimTextOutput("shoe_report")
      ),
      div(class = "panel",
          actionButton("toggle_count", "Check running count", class = "btn"),
          uiOutput("count_display"),
          actionButton("reset_session", "Reset session", class = "btn"),
          checkboxInput("hint_mode", "Show hint on mistakes (off = silent)", value = FALSE)
      )
    )
  )
)

# ---------- server -------------------------------------------------------

server <- function(input, output, session) {

  S <- reactiveValues(
    system = hi_lo_system(),
    n_decks = 6,
    shoe = NULL,
    suits_dealt = integer(0),     # parallel to shoe$cards positions consumed
    phase = "betting",            # betting | playing | settle | shoe_quiz | shoe_report
    bet_input = 5,
    base_unit = 5,
    player_hands = list(),
    active = 1L,
    dealer = list(cards = integer(0), suits = character(0), hole = NA_integer_, hole_suit = NA_character_),
    msg = "",
    round_log = list(),
    stats = empty_stats(),
    last_shoe_report = "(no shoe finished yet)",
    show_count = FALSE,
    pending_quiz_resume = NULL    # function to call after quiz answered
  )

  # Initialize first shoe.
  observe({
    if (is.null(S$shoe)) {
      S$shoe <- new_shoe(S$n_decks, S$system)
    }
  })

  # Suit picker: pure cosmetics; doesn't affect logic.
  pick_suit <- function() sample(SUITS, 1)

  # Wrappers around the engine that also produce a parallel suit per card.
  draw <- function(hidden_count = FALSE) {
    if (hidden_count) {
      d <- deal_card_hidden(S$shoe); S$shoe <- d$shoe
    } else {
      d <- deal_card(S$shoe);        S$shoe <- d$shoe
    }
    list(card = d$card, suit = pick_suit())
  }

  # ---- Render ----------------------------------------------------------
  output$dealer_cards <- renderUI({
    d <- S$dealer
    if (length(d$cards) == 0) return(HTML(""))
    if (S$phase %in% c("playing")) {
      # Show only upcard, hole face down.
      HTML(cards_html(c(d$cards[1], NA), c(d$suits[1], "?"), hide_index = 2))
    } else {
      cards <- c(d$cards, if (!is.na(d$hole)) d$hole else integer(0))
      suits <- c(d$suits, if (!is.na(d$hole_suit)) d$hole_suit else character(0))
      HTML(cards_html(cards, suits))
    }
  })

  output$dealer_total <- renderText({
    d <- S$dealer
    if (length(d$cards) == 0) return("")
    if (S$phase == "playing") {
      paste("Showing:", rank_label(d$cards[1]))
    } else {
      full <- c(d$cards, if (!is.na(d$hole)) d$hole else integer(0))
      paste("Dealer total:", hand_total(full))
    }
  })

  output$player_hands <- renderUI({
    if (length(S$player_hands) == 0) return(HTML(""))
    pieces <- lapply(seq_along(S$player_hands), function(i) {
      h <- S$player_hands[[i]]
      tot <- hand_total(h$cards)
      soft <- hand_is_soft(h$cards)
      cls <- if (S$phase == "playing" && i == S$active) "hand-active" else ""
      label <- sprintf("Hand %d  bet $%g  total %s%s%s",
                       i, h$bet, tot,
                       if (soft) " (soft)" else "",
                       if (isTRUE(h$result_text != "")) paste0(" — ", h$result_text) else "")
      HTML(sprintf('<div class="%s">%s%s</div>', cls,
                   cards_html(h$cards, h$suits),
                   paste0("<div>", label, "</div>")))
    })
    do.call(tagList, pieces)
  })

  output$player_msg <- renderText(S$msg)

  output$stats_tbl <- renderTable({
    s <- S$stats
    dec_pct <- if (s$decisions_total) 100 * s$decisions_correct / s$decisions_total else NA
    bet_pct <- if (s$bets_total) 100 * s$bets_correct / s$bets_total else NA
    cnt_pct <- if (s$count_quizzes) 100 * s$count_quiz_ok / s$count_quizzes else NA
    data.frame(
      Stat = c("Rounds", "Decisions", "Decision accuracy",
               "Bets graded", "Bet accuracy (±1 unit)",
               "Count quizzes", "Count quiz accuracy", "Bankroll"),
      Value = c(s$rounds,
                s$decisions_total,
                if (is.na(dec_pct)) "-" else sprintf("%.1f%%", dec_pct),
                s$bets_total,
                if (is.na(bet_pct)) "-" else sprintf("%.1f%%", bet_pct),
                s$count_quizzes,
                if (is.na(cnt_pct)) "-" else sprintf("%.1f%%", cnt_pct),
                sprintf("$%g", s$bankroll))
    )
  }, striped = TRUE)

  output$shoe_report <- renderText(S$last_shoe_report)

  output$count_display <- renderUI({
    if (!isTRUE(S$show_count) || is.null(S$shoe)) return(NULL)
    rc <- S$shoe$running_count
    dr <- decks_remaining(S$shoe)
    tc <- if (dr > 0) rc / dr else 0
    HTML(sprintf(
      '<div style="margin:6px 0;padding:6px;background:#000;border-radius:6px;">
         RC: <b>%d</b> &nbsp; TC: <b>%.2f</b> &nbsp; Decks left: %.2f
       </div>', rc, tc, dr))
  })

  observeEvent(input$toggle_count, { S$show_count <- !isTRUE(S$show_count) })

  action_name <- function(a) {
    switch(a, H = "Hit", S = "Stand", D = "Double", P = "Split", a)
  }

  output$round_review <- renderUI({
    if (S$phase != "settle" || length(S$round_log) == 0) return(NULL)
    rows <- lapply(S$round_log, function(e) {
      took <- action_name(e$took); want <- action_name(e$expected)
      if (e$ok) {
        sprintf('<li style="color:#9f9;">Hand %d (%s vs %s): %s — correct</li>',
                e$hand_idx, e$hand, e$up, took)
      } else {
        sprintf('<li style="color:#f99;">Hand %d (%s vs %s): you %s — basic strategy says <b>%s</b></li>',
                e$hand_idx, e$hand, e$up, took, want)
      }
    })
    HTML(paste0("<h4>Round review</h4><ul>",
                paste(unlist(rows), collapse = ""), "</ul>"))
  })

  # ---- Controls --------------------------------------------------------
  output$controls <- renderUI({
    switch(S$phase,
      "betting" = tagList(
        numericInput("bet_input", "Bet ($):", value = S$bet_input,
                     min = 1, step = 1, width = "120px"),
        actionButton("deal", "Deal", class = "btn btn-primary"),
        helpText("Tip: bet bigger when the deck is hot.")
      ),
      "playing" = {
        h <- S$player_hands[[S$active]]
        can_double <- length(h$cards) == 2 && !isTRUE(h$from_split_ace)
        can_split  <- length(h$cards) == 2 && h$cards[1] == h$cards[2] &&
                      length(S$player_hands) < 4 && !isTRUE(h$from_split_ace)
        tagList(
          actionButton("act_hit",   "Hit",    class = "btn"),
          actionButton("act_stand", "Stand",  class = "btn"),
          if (can_double) actionButton("act_double", "Double", class = "btn"),
          if (can_split)  actionButton("act_split",  "Split",  class = "btn")
        )
      },
      "settle" = tagList(
        actionButton("next_round", "Next round", class = "btn btn-primary")
      ),
      "shoe_report" = tagList(
        actionButton("ack_report", "Start new shoe", class = "btn btn-primary")
      ),
      NULL
    )
  })

  # ---- Phase transitions ----------------------------------------------

  start_round <- function() {
    # If shoe needs to reshuffle, run quiz first.
    if (shoe_exhausted(S$shoe) || cards_remaining(S$shoe) < 15) {
      run_count_quiz()
      return(invisible())
    }
    # Grade the bet against current TC.
    bet <- max(1, as.numeric(S$bet_input %||% S$base_unit))
    tc  <- true_count(S$shoe)
    tgt <- target_bet(tc, unit = S$base_unit)
    S$stats$bets_total   <- S$stats$bets_total + 1L
    if (abs(bet - tgt) <= S$base_unit) {
      S$stats$bets_correct <- S$stats$bets_correct + 1L
    }
    S$stats$bet_log <- c(S$stats$bet_log,
                        list(list(tc = tc, target = tgt, bet = bet)))
    # Deal.
    p1 <- draw(); d1 <- draw(); p2 <- draw()
    d2 <- draw(hidden_count = TRUE)
    player <- c(p1$card, p2$card); psuits <- c(p1$suit, p2$suit)
    S$dealer <- list(cards = d1$card, suits = d1$suit,
                     hole  = d2$card, hole_suit = d2$suit)
    S$player_hands <- list(list(
      cards = player, suits = psuits, bet = bet,
      doubled = FALSE, from_split_ace = FALSE,
      finished = FALSE, result_text = "")
    )
    S$active <- 1L
    S$msg <- ""
    S$round_log <- list()
    # Naturals.
    p_bj <- hand_is_blackjack(player)
    d_bj <- (d1$card == 1 || d1$card == 10) &&
            hand_is_blackjack(c(d1$card, d2$card))
    if (p_bj || d_bj) {
      # Reveal hole now (and add to count).
      S$shoe <- reveal_card(S$shoe, S$dealer$hole)
      go_to_settle(natural = TRUE, p_bj = p_bj, d_bj = d_bj)
      return(invisible())
    }
    S$phase <- "playing"
  }

  go_to_settle <- function(natural = FALSE, p_bj = FALSE, d_bj = FALSE) {
    # If natural, no dealer play needed.
    if (!natural) {
      # Reveal hole (if not already revealed).
      if (!is.na(S$dealer$hole)) {
        S$shoe <- reveal_card(S$shoe, S$dealer$hole)
        S$dealer$cards <- c(S$dealer$cards, S$dealer$hole)
        S$dealer$suits <- c(S$dealer$suits, S$dealer$hole_suit)
        S$dealer$hole <- NA_integer_; S$dealer$hole_suit <- NA_character_
      }
      # Dealer plays only if at least one player hand is alive.
      alive <- any(vapply(S$player_hands, function(h) !hand_busted(h$cards), logical(1)))
      if (alive) {
        repeat {
          tot <- hand_total(S$dealer$cards)
          if (tot >= 17) break    # S17
          d <- draw()
          S$dealer$cards <- c(S$dealer$cards, d$card)
          S$dealer$suits <- c(S$dealer$suits, d$suit)
        }
      }
    } else {
      # Natural: ensure hole shown.
      S$dealer$cards <- c(S$dealer$cards, S$dealer$hole)
      S$dealer$suits <- c(S$dealer$suits, S$dealer$hole_suit)
      S$dealer$hole <- NA_integer_; S$dealer$hole_suit <- NA_character_
    }
    # Settle.
    dt <- hand_total(S$dealer$cards)
    msg_pieces <- character(0)
    for (i in seq_along(S$player_hands)) {
      h <- S$player_hands[[i]]
      pt <- hand_total(h$cards)
      is_bj_hand <- length(S$player_hands) == 1 && hand_is_blackjack(h$cards)
      if (natural && p_bj && d_bj) { net <- 0;       text <- "Push (BJ vs BJ)" }
      else if (natural && p_bj)    { net <- 1.5*h$bet; text <- "Blackjack! +1.5x" }
      else if (natural && d_bj)    { net <- -h$bet;  text <- "Dealer BJ" }
      else if (pt > 21)            { net <- -h$bet;  text <- "Bust" }
      else if (dt > 21)            { net <- +h$bet;  text <- "Dealer bust, win" }
      else if (pt > dt)            { net <- +h$bet;  text <- "Win" }
      else if (pt < dt)            { net <- -h$bet;  text <- "Lose" }
      else                          { net <- 0;       text <- "Push" }
      h$result_text <- sprintf("%s (%s$%g)", text, ifelse(net >= 0, "+", "-"), abs(net))
      h$net <- net
      S$player_hands[[i]] <- h
      S$stats$bankroll <- S$stats$bankroll + net
      msg_pieces <- c(msg_pieces, sprintf("Hand %d: %s", i, h$result_text))
    }
    S$msg <- paste(msg_pieces, collapse = " | ")
    S$stats$rounds <- S$stats$rounds + 1L
    S$phase <- "settle"
  }

  # ---- Decision handler -----------------------------------------------

  record_decision <- function(action_taken) {
    h <- S$player_hands[[S$active]]
    can_double <- length(h$cards) == 2 && !isTRUE(h$from_split_ace)
    can_split  <- length(h$cards) == 2 && h$cards[1] == h$cards[2] &&
                  length(S$player_hands) < 4 && !isTRUE(h$from_split_ace)
    expected <- basic_action(h$cards, S$dealer$cards[1],
                             can_double = can_double, can_split = can_split)
    ok <- (action_taken == expected)
    S$stats$decisions_total <- S$stats$decisions_total + 1L
    if (ok) S$stats$decisions_correct <- S$stats$decisions_correct + 1L
    entry <- list(
      hand_idx = S$active,
      hand = paste(vapply(h$cards, rank_label, character(1)), collapse = ""),
      up   = rank_label(S$dealer$cards[1]),
      took = action_taken, expected = expected, ok = ok
    )
    S$stats$decisions_log <- c(S$stats$decisions_log, list(entry))
    S$round_log <- c(S$round_log, list(entry))
    if (!ok && isTRUE(input$hint_mode)) {
      showNotification(sprintf("Basic strategy says: %s (you did %s)",
                               expected, action_taken),
                       type = "warning", duration = 3)
    }
  }

  do_hit <- function() {
    h <- S$player_hands[[S$active]]
    d <- draw()
    h$cards <- c(h$cards, d$card); h$suits <- c(h$suits, d$suit)
    S$player_hands[[S$active]] <- h
    if (hand_busted(h$cards) || hand_total(h$cards) == 21) advance_hand()
  }
  do_stand <- function() advance_hand()
  do_double <- function() {
    h <- S$player_hands[[S$active]]
    h$bet <- h$bet * 2; h$doubled <- TRUE
    d <- draw()
    h$cards <- c(h$cards, d$card); h$suits <- c(h$suits, d$suit)
    S$player_hands[[S$active]] <- h
    advance_hand()
  }
  do_split <- function() {
    h <- S$player_hands[[S$active]]
    is_ace <- h$cards[1] == 1
    a <- draw(); b <- draw()
    h1 <- list(cards = c(h$cards[1], a$card), suits = c(h$suits[1], a$suit),
               bet = h$bet, doubled = FALSE,
               from_split_ace = is_ace, finished = FALSE, result_text = "")
    h2 <- list(cards = c(h$cards[2], b$card), suits = c(h$suits[2], b$suit),
               bet = h$bet, doubled = FALSE,
               from_split_ace = is_ace, finished = FALSE, result_text = "")
    new_hands <- S$player_hands
    new_hands[[S$active]] <- h1
    new_hands <- append(new_hands, list(h2), after = S$active)
    S$player_hands <- new_hands
    if (is_ace) {
      # Split aces: one card each, no further play.
      advance_hand()  # h1 done
      # h2 also done; advance_hand will keep moving.
    }
  }

  advance_hand <- function() {
    repeat {
      S$active <- S$active + 1L
      if (S$active > length(S$player_hands)) {
        go_to_settle()
        return(invisible())
      }
      h <- S$player_hands[[S$active]]
      # Skip done hands (split aces).
      if (isTRUE(h$from_split_ace) && length(h$cards) >= 2) next
      break
    }
  }

  # ---- Count quiz (modal) ---------------------------------------------

  run_count_quiz <- function() {
    # Burn no more cards; we use the current shoe state.
    rc_actual <- S$shoe$running_count
    dr_actual <- decks_remaining(S$shoe)
    tc_actual <- if (dr_actual > 0) rc_actual / dr_actual else 0
    S$stats$rc_actual_at_quiz <- rc_actual
    S$stats$tc_actual_at_quiz <- tc_actual
    showModal(modalDialog(
      title = "End of shoe — count check",
      p("Before we reshuffle, what was the count?"),
      numericInput("quiz_rc", "Final running count:", value = 0, step = 1),
      numericInput("quiz_tc", "Final true count:", value = 0, step = 0.5),
      footer = tagList(actionButton("submit_quiz", "Submit", class = "btn-primary"))
    ))
  }

  observeEvent(input$submit_quiz, {
    rc_user <- as.numeric(input$quiz_rc); tc_user <- as.numeric(input$quiz_tc)
    rc_actual <- S$stats$rc_actual_at_quiz
    tc_actual <- S$stats$tc_actual_at_quiz
    rc_ok <- isTRUE(rc_user == rc_actual)
    tc_ok <- !is.na(tc_user) && abs(tc_user - tc_actual) <= 0.5
    S$stats$count_quizzes <- S$stats$count_quizzes + 1L
    if (rc_ok && tc_ok) S$stats$count_quiz_ok <- S$stats$count_quiz_ok + 1L
    removeModal()
    # Build shoe report.
    decs <- S$stats$decisions_log
    n_dec <- length(decs); n_wrong <- sum(!vapply(decs, `[[`, logical(1), "ok"))
    wrong_lines <- vapply(decs[!vapply(decs, `[[`, logical(1), "ok")], function(x) {
      sprintf("  %s vs %s: you=%s, basic=%s",
              x$hand, x$up, x$took, x$expected)
    }, character(1))
    rep <- c(
      sprintf("Shoe done. RC actual=%d, you=%g (%s).",
              rc_actual, rc_user, ifelse(rc_ok, "correct", "off")),
      sprintf("TC actual=%.2f, you=%g (%s, tol +/-0.5).",
              tc_actual, tc_user, ifelse(tc_ok, "correct", "off")),
      sprintf("Decisions this shoe: %d (%d wrong).", n_dec, n_wrong),
      if (length(wrong_lines)) c("Mistakes:", wrong_lines) else NULL
    )
    S$last_shoe_report <- paste(rep, collapse = "\n")
    # Reset shoe and per-shoe decision log (cumulative stats stay).
    S$shoe <- new_shoe(S$n_decks, S$system)
    S$stats$decisions_log <- list()
    S$phase <- "shoe_report"
  })

  observeEvent(input$ack_report, {
    S$phase <- "betting"
    S$player_hands <- list()
    S$dealer <- list(cards = integer(0), suits = character(0),
                     hole = NA_integer_, hole_suit = NA_character_)
    S$msg <- ""
  })

  # ---- Event wiring ----------------------------------------------------
  observeEvent(input$deal,        { S$bet_input <- input$bet_input; start_round() })
  observeEvent(input$act_hit,     { record_decision("H"); do_hit() })
  observeEvent(input$act_stand,   { record_decision("S"); do_stand() })
  observeEvent(input$act_double,  { record_decision("D"); do_double() })
  observeEvent(input$act_split,   { record_decision("P"); do_split() })
  observeEvent(input$next_round,  {
    S$player_hands <- list()
    S$dealer <- list(cards = integer(0), suits = character(0),
                     hole = NA_integer_, hole_suit = NA_character_)
    S$msg <- ""
    S$phase <- "betting"
  })
  observeEvent(input$reset_session, {
    S$stats <- empty_stats()
    S$shoe <- new_shoe(S$n_decks, S$system)
    S$player_hands <- list()
    S$dealer <- list(cards = integer(0), suits = character(0),
                     hole = NA_integer_, hole_suit = NA_character_)
    S$phase <- "betting"
    S$last_shoe_report <- "(reset)"
  })
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

shinyApp(ui, server)
