# =============================================================================
# CSU DEFENSE 2026: WHERE IT BREAKS, AND WHY
#
# Part A - Where:  national rank, down & distance, game by game, field zone
# Part B - Why:    six tests, each looking for the fingerprint a cause leaves
#   T1 Opponents just good?     T2 Which unit (front / LB / secondary)?
#   T3 Coverage style fit?      T4 Scheme vs opponent style / conference?
#   T5 Depth / conditioning?    T6 Play-calling change?
# Part C - Scorecard
#
# Public play-by-play cannot see coverage calls, box counts, alignments or
# missed tackles. Every "why" is an inference from fingerprints - confirm on film.
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
install.packages(c("cfbfastR", "tidyverse"))

# CFBD API key (free): https://collegefootballdata.com/key
# Session fallback below - REMOVE before sharing this script with anyone.
# Run usethis::edit_r_environ(), add CFBD_API_KEY=YOUR-KEY, restart R.
if (Sys.getenv("CFBD_API_KEY") == "") {
  stop("Please set your CFBD_API_KEY in your .Renviron file. See instructions above.")
}

library(cfbfastR)
library(tidyverse)
stopifnot(has_cfbd_key())

TEAM       <- "Colorado State"
SEASON     <- 2026
CONF       <- "Pac-12"
CHANGE_OPP <- "Oregon State"   # play-calling changed hands during this game
OUT        <- "csu_output"
dir.create(OUT, showWarnings = FALSE)

# ---- 1. Load data (cached after the first run) ------------------------------
cache <- file.path(OUT, "pbp_2026.rds")
if (file.exists(cache)) {
  pbp <- readRDS(cache)
} else {
  pbp <- tryCatch(load_cfb_pbp(seasons = SEASON), error = function(e) NULL)
  if (is.null(pbp) || nrow(pbp) == 0) {
    message("Prebuilt season not available - pulling week by week (slow)...")
    pbp <- map_dfr(1:16, function(wk) {
      tryCatch(cfbd_pbp_data(year = SEASON, week = wk, epa_wpa = TRUE),
               error = function(e) NULL)
    })
  }
  saveRDS(pbp, cache)
}
message("Plays loaded: ", nrow(pbp), " | through week ", max(pbp$week, na.rm = TRUE))

try_get <- function(x) tryCatch(x, error = function(e) { message("Skipped: ", conditionMessage(e)); NULL })
fbs_info <- cfbd_team_info(only_fbs = TRUE, year = SEASON)
FBS      <- fbs_info$school
CONF_TEAMS <- fbs_info$school[fbs_info$conference == CONF]
adv      <- try_get(cfbd_stats_season_advanced(year = SEASON, excl_garbage_time = TRUE))
sp       <- try_get(cfbd_ratings_sp(year = SEASON))
talent   <- try_get(cfbd_team_talent(year = SEASON))
roster   <- try_get(cfbd_team_roster(year = SEASON, team = TEAM))
portal   <- try_get(cfbd_recruiting_transfer_portal(year = SEASON))

# Column check. cfbfastR names shift between versions; this stops early and
# tells you what is missing instead of producing wrong numbers.
required <- c("game_id", "week", "period", "pos_team", "def_pos_team", "down",
              "distance", "yards_to_goal", "yards_gained", "play_text", "pass",
              "rush", "EPA", "success", "pos_score_diff_start", "drive_id")
missing_cols <- setdiff(required, names(pbp))   # (not named "missing": that's a base R function)
if (length(missing_cols) > 0) {
  stop("Missing columns: ", paste(missing_cols, collapse = ", "),
       " - run names(pbp) and send me the output")
}

# Optional columns: create as NA if absent so the code below still runs
optional <- c("sack_vec", "int", "fumble_vec", "completion", "penalty_1st_conv",
              "drive_play_number", "pass_breakup_player_name",
              "interception_player_name", "sack_player_name", "fumble_forced_player_name")
for (col in setdiff(optional, names(pbp))) pbp[[col]] <- NA

# ---- 2. Clean plays ----------------------------------------------------------
# Garbage time: Connelly-style score margins by quarter
pbp <- pbp %>%
  filter(!is.na(pos_team), !is.na(def_pos_team)) %>%
  mutate(
    text    = str_to_lower(coalesce(play_text, "")),
    garbage = case_when(
      period == 2 & abs(pos_score_diff_start) > 38 ~ TRUE,
      period == 3 & abs(pos_score_diff_start) > 28 ~ TRUE,
      period == 4 & abs(pos_score_diff_start) > 22 ~ TRUE,
      TRUE ~ FALSE
    )
  )

plays <- pbp %>%
  filter(pass == 1 | rush == 1, !is.na(EPA), !is.na(down), !garbage,
         !str_detect(text, "kneel|spike")) %>%
  mutate(
    sack     = coalesce(as.numeric(sack_vec), as.numeric(pass == 1 & str_detect(text, "sacked"))),
    intc     = coalesce(as.numeric(int), as.numeric(str_detect(text, "intercepted"))),
    fumble   = coalesce(as.numeric(fumble_vec), 0),
    pbu      = as.numeric(coalesce(pass_breakup_player_name, "") != "" |
                            str_detect(text, "broken up")),
    attempt  = as.numeric(pass == 1 & sack == 0),
    complete = as.numeric(attempt == 1 &
                            coalesce(as.numeric(completion),
                                     as.numeric(str_detect(text, "complete") &
                                                  !str_detect(text, "incomplete"))) == 1),
    tfl      = as.numeric(rush == 1 & yards_gained < 0),
    havoc    = as.numeric(sack + intc + pbu + fumble + tfl > 0),
    explosive = as.numeric((rush == 1 & yards_gained >= 12) | (pass == 1 & yards_gained >= 16)),
    # Where a run ends = which level of the defense gave it up
    rush_level = case_when(
      rush != 1 ~ NA_character_,
      yards_gained <= 0  ~ "1 Stuffed (<=0)",
      yards_gained <= 4  ~ "2 Line (1-4)",
      yards_gained <= 10 ~ "3 Second level (5-10)",
      TRUE               ~ "4 Open field (11+)"
    ),
    down_grp = case_when(
      down == 1                 ~ "1st",
      down == 2 & distance <= 3 ~ "2nd & short",
      down == 2 & distance <= 7 ~ "2nd & medium",
      down == 2                 ~ "2nd & long",
      distance <= 3             ~ "3rd/4th & short",
      distance <= 7             ~ "3rd/4th & medium",
      TRUE                      ~ "3rd/4th & long"
    ),
    zone = case_when(
      yards_to_goal <= 20 ~ "Red zone",
      yards_to_goal <= 50 ~ "Opp. territory",
      TRUE                ~ "Own territory"
    ),
    is_csu  = def_pos_team == TEAM,
    def_fbs = def_pos_team %in% FBS
  ) %>%
  group_by(game_id, def_pos_team) %>%
  mutate(def_snap = row_number()) %>%               # defensive snap # in the game
  group_by(game_id, drive_id) %>%
  mutate(drive_play = coalesce(as.numeric(drive_play_number), as.numeric(row_number()))) %>%
  ungroup()

# ---- 3. Opponent adjustment (the engine behind every "why") ------------------
# Each game's average = league mean + offense strength + defense strength,
# solved back and forth until stable (same idea as a simple rating system).
# All teams are included so FCS opponents are rated fairly; ranks are FBS only.
adjust <- function(df, y, iters = 150) {
  g <- df %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(game_id, pos_team, def_pos_team) %>%
    summarise(v = mean(.data[[y]]), n = n(), .groups = "drop")
  mu  <- weighted.mean(g$v, g$n)
  off <- tibble(pos_team = unique(g$pos_team), o = 0)
  for (i in 1:iters) {
    def <- g %>% left_join(off, by = "pos_team") %>%
      group_by(def_pos_team) %>% summarise(d = weighted.mean(v - mu - o, n))
    off <- g %>% left_join(def, by = "def_pos_team") %>%
      group_by(pos_team) %>% summarise(o = weighted.mean(v - mu - d, n))
  }
  list(mu = mu, off = off, def = def)
}

# Where CSU ranks among FBS defenses. pct_worst: 100 = worst. rank: 1 = best.
# Set good_high = TRUE when a bigger number is good for a defense (sacks, havoc).
csu_rank <- function(def_tbl, good_high = FALSE) {
  t <- def_tbl %>% filter(def_pos_team %in% FBS)
  badness <- t$d
  if (good_high) badness <- -badness
  i <- which(t$def_pos_team == TEAM)
  tibble(value     = round(t$d[i], 3),
         pct_worst = round(100 * rank(badness)[i] / nrow(t)),
         rank      = paste(rank(badness, ties.method = "min")[i], "of", nrow(t)))
}

m_all  <- adjust(plays, "EPA")
m_pass <- adjust(filter(plays, pass == 1), "EPA")
m_rush <- adjust(filter(plays, rush == 1), "EPA")
m_sr   <- adjust(plays, "success")

# adj_epa = EPA allowed beyond what that offense normally gains.
# Average it over any slice to get the defense's true contribution there.
plays <- plays %>%
  left_join(m_all$off, by = "pos_team") %>%
  mutate(adj_epa = EPA - m_all$mu - o) %>%
  select(-o)
csu <- filter(plays, is_csu)
fbs_other <- filter(plays, def_fbs, !is_csu)   # FBS baseline (excludes CSU)

ci95 <- function(x) round(mean(x) + c(-1.96, 1.96) * sd(x) / sqrt(length(x)), 3)

# =============================================================================
# PART A - WHERE IT BREAKS
# =============================================================================

# A1. National table (FBS defenses only, raw and adjusted)
raw_def <- plays %>% filter(def_fbs) %>% group_by(def_pos_team) %>% summarise(d = mean(EPA))
a1 <- bind_rows(
  "EPA/play (raw)"           = csu_rank(raw_def),
  "EPA/play (opp-adjusted)"  = csu_rank(m_all$def),
  "Pass EPA (opp-adjusted)"  = csu_rank(m_pass$def),
  "Rush EPA (opp-adjusted)"  = csu_rank(m_rush$def),
  "Success rate (opp-adj.)"  = csu_rank(m_sr$def),
  .id = "metric")
cat("\n===== A1. CSU vs FBS defenses =====\n"); print(a1)

# A2. Down & distance: CSU vs rest of FBS (adjusted EPA, raw success)
a2 <- bind_rows(CSU = csu, FBS = fbs_other, .id = "grp") %>%
  group_by(down_grp, grp) %>%
  summarise(adj_epa = mean(adj_epa), success = mean(success, na.rm = TRUE), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = grp, values_from = c(adj_epa, success, n)) %>%
  mutate(gap = adj_epa_CSU - adj_epa_FBS)
cat("\n===== A2. Down & distance (gap > 0 = CSU worse) =====\n"); print(a2)

# How often opponents even reach 3rd down
cat("Share of plays on 3rd/4th down - CSU:", round(mean(csu$down >= 3), 3),
    "| FBS:", round(mean(fbs_other$down >= 3), 3), "\n")

# A3. Game by game, raw and adjusted
a3 <- csu %>% group_by(week, pos_team) %>%
  summarise(n = n(), raw_epa = mean(EPA), adj_epa = mean(adj_epa),
            success = mean(success, na.rm = TRUE), .groups = "drop") %>%
  arrange(week)
cat("\n===== A3. By game (adj_epa > 0 = allowed more than that offense usually gets) =====\n"); print(a3)

# A4. Field zone vs FBS
a4 <- bind_rows(CSU = csu, FBS = fbs_other, .id = "grp") %>%
  group_by(zone, grp) %>%
  summarise(adj_epa = mean(adj_epa), success = mean(success, na.rm = TRUE), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = grp, values_from = c(adj_epa, success, n))
cat("\n===== A4. Field zone =====\n"); print(a4)

# =============================================================================
# PART B - WHY
# =============================================================================

# ---- T1. Are opponents just good? -------------------------------------------
# Fingerprint if true: adjusted rank much better than raw rank.
cat("\n===== T1. Raw vs opponent-adjusted (see A1) =====\n")
print(a1 %>% filter(metric %in% c("EPA/play (raw)", "EPA/play (opp-adjusted)")))
if (!is.null(sp)) {
  cat("SP+ cross-check:\n")
  print(sp %>% filter(team == TEAM) %>% select(any_of(c("team", "rating", "ranking", "defense_rating", "defense_ranking"))))
}

# ---- T2. Which unit is failing? ---------------------------------------------
# Fingerprint: the level where yards pile up. Line yards/stuff rate = DL,
# second-level yards = LB fits, open-field yards = safeties/pursuit/tackling.
t2_levels <- NULL
if (!is.null(adv)) {
  level_metrics <- tribble(
    ~metric,                  ~good_high,
    "def_line_yds",           FALSE,
    "def_second_lvl_yds",     FALSE,
    "def_open_field_yds",     FALSE,
    "def_stuff_rate",         TRUE,
    "def_power_success",      FALSE,
    "def_havoc_front_seven",  TRUE,
    "def_havoc_db",           TRUE
  ) %>% filter(metric %in% names(adv))
  # (fixed: plain loop instead of map2 inside mutate, which lost the column name)
  t2_levels <- map2_dfr(level_metrics$metric, level_metrics$good_high, function(col, gh) {
    tibble(def_pos_team = adv$team, d = as.numeric(adv[[col]])) %>%
      filter(def_pos_team %in% FBS, !is.na(d)) %>%
      csu_rank(good_high = gh) %>%
      mutate(metric = col, fbs_median = median(as.numeric(adv[[col]][adv$team %in% FBS]), na.rm = TRUE),
             .before = 1)
  })
  cat("\n===== T2a. Defense by level (CFBD, raw; pct_worst 100 = worst) =====\n"); print(t2_levels)
}

t2_runs <- bind_rows(CSU = csu, FBS = fbs_other, .id = "grp") %>%
  filter(rush == 1) %>% count(grp, rush_level) %>%
  group_by(grp) %>% mutate(share = round(n / sum(n), 3)) %>% select(-n) %>%
  pivot_wider(names_from = grp, values_from = share) %>% mutate(diff = CSU - FBS)
cat("\n===== T2b. Where runs end (share of rushes) =====\n"); print(t2_runs)

# Who makes CSU's disruptive plays, by position group
pos_group <- function(p) case_when(
  p %in% c("DL", "DE", "DT", "NT", "EDGE")        ~ "DL",
  p %in% c("LB", "ILB", "OLB", "MLB")             ~ "LB",
  p %in% c("DB", "CB", "S", "FS", "SS", "NB")     ~ "DB",
  TRUE                                            ~ "Other")
if (!is.null(roster)) {
  # Play text names players as jersey + initial.last, e.g. "#13 E.Mc-Cantos".
  #   "broken up by #13 E.Mc-Cantos"   -> pass breakup
  #   "intercepted by #27 J.King"      -> interception
  #   "hurried by #10 B.Kelley and #8 A.Turner" -> QB pressure
  #   last "(#9 J.Holman)"             -> tackler (or the sacker on a sack)
  # Players are matched to the roster on jersey number + last name.
  # Drop suffixes so "Hagans Jr." on the roster matches "Hagans" in play text
  norm <- function(x) str_remove_all(str_remove(str_to_lower(x), "\\s+(jr|sr|ii|iii|iv)\\.?$"), "[^a-z]")
  ros <- roster %>%
    transmute(jersey = as.character(jersey), key_last = norm(last_name),
              player = str_squish(paste(first_name, last_name)), position, group = pos_group(position)) %>%
    distinct(jersey, key_last, .keep_all = TRUE)
  player_re <- "#(\\d+) [A-Za-z]+\\.([A-Za-z'\\-]+)"
  
  # Last "( )" group that contains a jersey number, or NA if there isn't one
  last_or_na <- function(x) {
    if (length(x) > 0) {
      return(x[length(x)])
    }
    NA_character_
  }
  
  events <- csu %>%
    mutate(last_paren = map_chr(str_extract_all(play_text, "\\([^)]*#[^)]*\\)"), last_or_na),
           seg_pass_breakup = str_extract(play_text, paste0("broken up by ", "#\\d+ [A-Za-z]+\\.[A-Za-z'\\-]+")),
           seg_interception = str_extract(play_text, paste0("intercepted by ", "#\\d+ [A-Za-z]+\\.[A-Za-z'\\-]+")),
           # cut off "#27 J.King return 5 yards..." so an interceptor isn't also credited with a hurry
           seg_hurry        = str_remove(str_extract(play_text, "hurried by [^(]*"),
                                         "(#\\d+ [A-Za-z]+\\.[A-Za-z'\\-]+ )?return.*"),
           seg_sack         = if_else(sack == 1, last_paren, NA_character_),
           # No tackle on a touchdown - the last "( )" there is the extra-point
           # holder/snapper from the other team, not a CSU tackler
           seg_tackle       = if_else(sack == 0 & intc == 0 & fumble == 0 & !str_detect(text, "touchdown"),
                                      last_paren, NA_character_)) %>%
    select(game_id, yards_gained, explosive, pass, rush, complete, starts_with("seg_")) %>%
    pivot_longer(starts_with("seg_"), names_to = "event", names_prefix = "seg_", values_to = "seg") %>%
    filter(!is.na(seg)) %>%
    mutate(m = map(seg, ~ str_match_all(.x, player_re)[[1]]),
           jersey = map(m, ~ .x[, 2]), key_last = map(m, ~ norm(.x[, 3]))) %>%
    select(-m, -seg) %>%
    unnest(c(jersey, key_last)) %>%
    left_join(ros, by = c("jersey", "key_last")) %>%
    mutate(group = coalesce(group, "Unmatched"))
  
  cat("\n===== T2c. CSU defenders: who is making (and missing) plays =====\n")
  cat("Credits parsed:", nrow(events), "| matched to roster:",
      round(mean(events$group != "Unmatched"), 3), "\n")
  
  t2_players <- events %>%
    mutate(player = coalesce(player, paste0("#", jersey, " ", key_last))) %>%
    group_by(player, position, group) %>%
    summarise(tackles          = sum(event == "tackle"),
              tackles_explosive = sum(event == "tackle" & explosive == 1),
              avg_gain_when_tackling = round(mean(yards_gained[event == "tackle"]), 1),
              pressures        = sum(event %in% c("hurry", "sack")),
              sacks            = sum(event == "sack"),
              pass_breakups    = sum(event == "pass_breakup"),
              interceptions    = sum(event == "interception"),
              .groups = "drop") %>%
    arrange(desc(tackles))
  print(t2_players, n = Inf, width = Inf)
  
  # Who finishes plays tells you where plays end up. A high DB share of run
  # tackles = runs are getting to the third level. On completions the tackler
  # is usually the nearest defender, a rough guide to who was in coverage.
  t2_groups <- events %>% filter(event == "tackle", group != "Unmatched") %>%
    mutate(play = case_when(rush == 1 ~ "run", complete == 1 ~ "completion", TRUE ~ "other")) %>%
    filter(play != "other") %>%
    group_by(play, group) %>%
    summarise(tackles = n(), on_explosives = sum(explosive), avg_gain = round(mean(yards_gained), 1),
              .groups = "drop_last") %>%
    mutate(share_of_tackles = round(tackles / sum(tackles), 3),
           share_of_explosive_tackles = round(on_explosives / sum(on_explosives), 3)) %>%
    ungroup()
  cat("\nTackles by position group (run vs completion):\n"); print(t2_groups, width = Inf)
  
  cat("\nPressure credits (hurries + sacks) by group:\n")
  print(events %>% filter(event %in% c("hurry", "sack")) %>% count(group))
  
  cat("\nDefensive roster size by group:\n")
  print(ros %>% filter(group != "Other") %>% count(group))
}
if (!is.null(portal)) {
  cat("\nIncoming defensive transfers:\n")
  print(portal %>% filter(destination == TEAM) %>% mutate(group = pos_group(position)) %>%
          filter(group != "Other") %>% count(group))
}
if (!is.null(talent)) {
  tal <- talent %>% filter(school %in% FBS) %>% mutate(talent = as.numeric(talent))
  cat("\nTalent composite - CSU rank:", rank(-tal$talent)[tal$school == TEAM], "of", nrow(tal),
      "| CSU:", tal$talent[tal$school == TEAM],
      "| avg opponent:", round(mean(tal$talent[tal$school %in% csu$pos_team]), 1), "\n")
}

# ---- T3. Coverage style vs personnel (man vs zone proxy) --------------------
# Fingerprints (opponent-adjusted FBS percentiles, 100 = worst):
#   MAN BEING BEATEN: completion % normal/low, yards per catch HIGH, explosives HIGH
#   ZONE TOO SOFT:    completion % HIGH, yards per catch normal/low, long drives
#   EVERYTHING OPEN:  completion % HIGH and yards per catch HIGH (busts/talent)
#   DPI/holding HIGH supports man-coverage stress
dropbacks <- filter(plays, pass == 1)
penalties <- pbp %>%
  filter(!garbage) %>%
  mutate(dpi = as.numeric(str_detect(text, "pass interference|defensive holding") &
                            !str_detect(text, "offensive pass interference") &
                            coalesce(as.numeric(penalty_1st_conv), 1) == 1)) %>%
  filter(pass == 1 | dpi == 1)

t3 <- bind_rows(
  completion_pct  = csu_rank(adjust(filter(dropbacks, attempt == 1), "complete")$def),
  yards_per_catch = csu_rank(adjust(filter(dropbacks, complete == 1), "yards_gained")$def),
  explosive_pass  = csu_rank(adjust(dropbacks, "explosive")$def),
  pass_success    = csu_rank(adjust(dropbacks, "success")$def),
  sack_rate       = csu_rank(adjust(dropbacks, "sack")$def, good_high = TRUE),
  # Pressure = sack or "QB hurried" in the play text. Stat crews chart hurries
  # unevenly, so read this as a rough check alongside sack rate.
  pressure_rate   = csu_rank(adjust(mutate(dropbacks, pressure = as.numeric(sack == 1 | str_detect(text, "hurried"))),
                                    "pressure")$def, good_high = TRUE),
  pbu_int_rate    = csu_rank(adjust(filter(dropbacks, attempt == 1) %>% mutate(x = pmax(pbu, intc)), "x")$def, good_high = TRUE),
  dpi_hold_rate   = csu_rank(adjust(penalties, "dpi")$def),
  .id = "metric")
cat("\n===== T3. Pass defense fingerprint (opp-adjusted) =====\n"); print(t3)

p <- function(m) t3$pct_worst[t3$metric == m]
coverage_read <- case_when(
  p("completion_pct") >= 70 & p("yards_per_catch") >= 70 ~ "EVERYTHING OPEN - completions AND chunk yards (busts, communication, talent gap)",
  p("completion_pct") <  60 & p("yards_per_catch") >= 70 & p("explosive_pass") >= 70 ~ "MAN BEING BEATEN - fewer completions but big ones (losing 1-on-1s)",
  p("completion_pct") >= 70 & p("yards_per_catch") <  60 ~ "ZONE TOO SOFT - easy underneath completions (spacing, leverage, tackling after catch)",
  TRUE ~ "MIXED - no clean coverage fingerprint")
if (p("dpi_hold_rate") >= 75) coverage_read <- paste(coverage_read, "+ high DPI/holding")
cat("Coverage read:", coverage_read, "\n")

drive_len <- plays %>% filter(def_fbs) %>% count(def_pos_team, game_id, drive_id) %>%
  group_by(def_pos_team) %>% summarise(d = mean(n))
cat("Plays per opponent drive:"); print(csu_rank(drive_len))

# ---- T4. Scheme vs opponent style, and conference fit -----------------------
# A 3-3-5 swaps a down lineman for a DB: built for spread passing, can be
# exposed by downhill runs. Fingerprint if mismatched: adjusted run D far worse
# than pass D, poor short-yardage D, worse vs run-leaning offenses.
rush_pct <- a1$pct_worst[a1$metric == "Rush EPA (opp-adjusted)"]
pass_pct <- a1$pct_worst[a1$metric == "Pass EPA (opp-adjusted)"]
cat("\n===== T4a. Adjusted run D pct_worst:", rush_pct, "| pass D pct_worst:", pass_pct, "=====\n")

t4_short <- bind_rows(CSU = csu, FBS = fbs_other, .id = "grp") %>%
  filter(rush == 1, distance <= 2, down >= 3 | yards_to_goal <= 2) %>%
  group_by(grp) %>% summarise(converted = mean(yards_gained >= distance), n = n())
cat("Short-yardage runs converted:\n"); print(t4_short)

# Offensive identity: early-down pass rate in neutral game states
off_style <- plays %>%
  filter(down <= 2, abs(pos_score_diff_start) <= 10) %>%
  group_by(pos_team) %>% summarise(early_pass_rate = mean(pass == 1), n = n()) %>%
  left_join(rename(m_rush$off, rush_offense = o), by = "pos_team") %>%
  left_join(rename(m_pass$off, pass_offense = o), by = "pos_team")
median_pass_rate <- median(off_style$early_pass_rate[off_style$pos_team %in% FBS])

t4_opp <- csu %>%
  group_by(pos_team) %>%
  summarise(adj_vs_run = mean(adj_epa[rush == 1]), adj_vs_pass = mean(adj_epa[pass == 1]), n = n()) %>%
  left_join(off_style %>% select(pos_team, early_pass_rate), by = "pos_team") %>%
  mutate(style = if_else(early_pass_rate >= median_pass_rate, "pass-leaning", "run-leaning")) %>%
  arrange(early_pass_rate)
cat("\n===== T4b. CSU vs each opponent's style =====\n"); print(t4_opp)
print(csu %>% left_join(select(t4_opp, pos_team, style), by = "pos_team") %>%
        group_by(style) %>%
        summarise(ci_low = ci95(adj_epa)[1], ci_high = ci95(adj_epa)[2], adj_epa = mean(adj_epa), n = n()))

t4_conf <- off_style %>% filter(pos_team %in% FBS) %>%
  mutate(grp = if_else(pos_team %in% CONF_TEAMS, CONF, "Rest of FBS")) %>%
  group_by(grp) %>%
  summarise(early_pass_rate = mean(early_pass_rate), rush_offense = mean(rush_offense, na.rm = TRUE),
            pass_offense = mean(pass_offense, na.rm = TRUE), teams = n())
cat("\n===== T4c. What conference offenses look like (+ = stronger offense) =====\n"); print(t4_conf)

# ---- T5. Depth / conditioning -----------------------------------------------
# Fingerprint: CSU gets worse late by MORE than a typical FBS defense does.
fade <- function(df, label) {
  bind_rows(CSU = filter(df, is_csu), FBS = filter(df, def_fbs, !is_csu), .id = "grp") %>%
    group_by(bucket, grp) %>% summarise(adj = mean(adj_epa), n = n(), .groups = "drop") %>%
    pivot_wider(names_from = grp, values_from = c(adj, n)) %>%
    mutate(csu_minus_fbs = adj_CSU - adj_FBS, split = label)
}
# Three different stories live here, so they are read separately:
#   late-GAME fade (Q4, high snap counts)  -> conditioning / depth
#   in-DRIVE fade (fine on plays 1-3, bad after) -> adjustments, tempo, communication
#   bad FIRST quarter                      -> opponents' scripted openers / preparation
# Buckets are kept to 40+ CSU snaps where possible (61+ snaps had only ~20).
t5 <- bind_rows(
  fade(mutate(plays, bucket = paste0("Q", pmin(period, 4))), "Quarter"),
  fade(mutate(plays, bucket = as.character(cut(drive_play, c(0, 3, 6, Inf), c("a 1-3", "b 4-6", "c 7+")))), "Play # in drive"),
  fade(mutate(plays, bucket = as.character(cut(def_snap, c(0, 20, 40, Inf), c("a 1-20", "b 21-40", "c 41+")))), "Snap # in game"),
  # Tempo straight from the play text: stat crews mark hurry-up snaps "No Huddle".
  # Charting varies by crew, but the FBS baseline uses the same crews.
  fade(mutate(plays, bucket = if_else(str_detect(text, "no huddle"), "a No huddle", "b Huddle")), "Tempo (no huddle)")
)
cat("\n===== T5. When it breaks (csu_minus_fbs = how much worse than a typical FBS D) =====\n")
print(t5 %>% arrange(split, bucket), n = Inf)
gap <- function(s, early, late) with(filter(t5, split == s), csu_minus_fbs[bucket == late] - csu_minus_fbs[bucket == early])
t5_signal <- c(late_game  = mean(c(gap("Quarter", "Q1", "Q4"), gap("Snap # in game", "a 1-20", "c 41+"))),
               in_drive   = gap("Play # in drive", "a 1-3", "c 7+"),
               first_qtr  = with(filter(t5, split == "Quarter"),
                                 csu_minus_fbs[bucket == "Q1"] - mean(csu_minus_fbs[bucket != "Q1"])))
print(round(t5_signal, 3))

# Does tempo explain the in-drive fade? If CSU is fine vs huddle offenses but
# breaks vs no-huddle - especially later in drives - that points to
# communication/getting the call in, not talent.
cat("\nShare of snaps faced in no-huddle - CSU:", round(mean(str_detect(csu$text, "no huddle")), 3),
    "| FBS:", round(mean(str_detect(fbs_other$text, "no huddle")), 3), "\n")
t5_tempo_drive <- bind_rows(CSU = csu, FBS = fbs_other, .id = "grp") %>%
  mutate(tempo = if_else(str_detect(text, "no huddle"), "No huddle", "Huddle"),
         drive_part = if_else(drive_play <= 3, "plays 1-3", "plays 4+")) %>%
  group_by(tempo, drive_part, grp) %>%
  summarise(adj = mean(adj_epa), explosive = mean(explosive), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = grp, values_from = c(adj, explosive, n)) %>%
  mutate(gap = adj_CSU - adj_FBS)
cat("Tempo x drive stage (gap > 0 = CSU worse than FBS):\n"); print(t5_tempo_drive, width = Inf)

# Tempo check (communication): snaps that come quickly after the previous one
# in the same drive. Game clock stops on incompletions etc., so this is a rough
# proxy - but the same noise applies to the FBS baseline.
# (V3 skipped this silently: the clock columns have different names in cfbfastR 3.0)
min_col <- intersect(c("clock.minutes", "clock_minutes", "clock_minutes_start"), names(plays))[1]
sec_col <- intersect(c("clock.seconds", "clock_seconds", "clock_seconds_start"), names(plays))[1]
if (is.na(min_col) || is.na(sec_col)) {
  cat("\nTempo check skipped - clock columns not found. Send me this:\n")
  print(grep("clock|time", names(plays), value = TRUE))
} else {
  tempo <- plays %>%
    mutate(clock = as.numeric(.data[[min_col]]) * 60 + as.numeric(.data[[sec_col]])) %>%
    group_by(game_id, drive_id) %>%
    mutate(gap_secs = lag(clock) - clock, prev_period = lag(period)) %>%
    ungroup() %>%
    filter(!is.na(gap_secs), period == prev_period, gap_secs > 0, gap_secs < 60) %>%
    mutate(bucket = if_else(gap_secs <= 20, "a fast (<=20s)", "b normal (>20s)"))
  t5_tempo <- fade(tempo, "Tempo")
  cat("\nTempo (seconds of game clock since previous snap, same drive):\n"); print(t5_tempo)
}

# ---- T6. Play-calling change (Summers -> McClendon) -------------------------
# Change happened mid-game vs Oregon State, so that game is excluded.
change_week <- csu$week[csu$pos_team == CHANGE_OPP][1]
t6_data <- csu %>% filter(week != change_week) %>%
  mutate(era = if_else(week < change_week, "1 Before (Summers)", "2 After (McClendon)"))
t6 <- t6_data %>% group_by(era) %>%
  summarise(games = n_distinct(game_id), snaps = n(),
            ci_low = ci95(adj_epa)[1], ci_high = ci95(adj_epa)[2],
            adj_rush = mean(adj_epa[rush == 1]), adj_pass = mean(adj_epa[pass == 1]),
            adj_2nd_down = mean(adj_epa[down == 2]),
            explosive = mean(explosive), havoc = mean(havoc), sack_rate = mean(sack[pass == 1]),
            pressure_rate = mean((sack == 1 | str_detect(text, "hurried"))[pass == 1]),
            adj_first_3 = mean(adj_epa[drive_play <= 3]), adj_play_4_plus = mean(adj_epa[drive_play > 3]),
            adj_epa = mean(adj_epa))
cat("\n===== T6. Before vs after play-calling change =====\n"); print(t6, width = Inf)
t6_test <- t.test(adj_epa ~ era, data = t6_data)
cat("Difference p-value:", round(t6_test$p.value, 3), "(only a few games - trend matters more)\n")

# ---- 2nd-down thread: which "why" explains the 2nd-down problem? -----------
second <- bind_rows(CSU = csu, FBS = fbs_other, .id = "grp") %>%
  filter(down == 2) %>% mutate(type = if_else(pass == 1, "pass", "rush"))
t_2nd <- second %>% group_by(down_grp, type, grp) %>%
  summarise(adj = mean(adj_epa), explosive = mean(explosive), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = grp, values_from = c(adj, explosive, n)) %>%
  mutate(gap = adj_CSU - adj_FBS) %>% arrange(desc(gap))
cat("\n===== 2nd down: biggest gaps first =====\n"); print(t_2nd)
cat("\n2nd-down runs by level:\n")
print(second %>% filter(type == "rush") %>% count(grp, rush_level) %>%
        group_by(grp) %>% mutate(share = round(n / sum(n), 3)) %>% select(-n) %>%
        pivot_wider(names_from = grp, values_from = share))
cat("Avg 1st-down gain allowed (sets up 2nd down) - CSU:", round(mean(csu$yards_gained[csu$down == 1]), 2),
    "| FBS:", round(mean(fbs_other$yards_gained[fbs_other$down == 1]), 2), "\n")

# =============================================================================
# PART C - SCORECARD (thresholds are judgment calls, stated so they can be defended)
# =============================================================================
raw_pct <- a1$pct_worst[a1$metric == "EPA/play (raw)"]
adj_pct <- a1$pct_worst[a1$metric == "EPA/play (opp-adjusted)"]
worst_level <- NULL
if (!is.null(t2_levels)) {
  worst_level <- t2_levels %>%
    filter(metric %in% c("def_line_yds", "def_second_lvl_yds", "def_open_field_yds")) %>%
    slice_max(pct_worst, n = 1, with_ties = FALSE)
}
if (!is.null(worst_level) && nrow(worst_level) == 0) worst_level <- NULL

# T2 text built here (not inside tribble) to keep the table simple
t2_evidence <- "CFBD level data unavailable"
t2_verdict  <- "NO SINGLE LEVEL STANDS OUT"
if (!is.null(worst_level)) {
  t2_evidence <- paste0(worst_level$metric, " pct_worst ", worst_level$pct_worst)
  if (worst_level$pct_worst >= 75) {
    t2_verdict <- paste("SUPPORTED -", worst_level$metric)
  }
}

scorecard <- tribble(
  ~test, ~question, ~evidence, ~verdict,
  "T1", "Opponents just good?",
  paste0("pct_worst raw ", raw_pct, " -> adjusted ", adj_pct),
  case_when(adj_pct >= 85 ~ "NOT SUPPORTED - still near the bottom after adjusting",
            raw_pct - adj_pct >= 25 ~ "PARTLY - schedule explains a lot",
            TRUE ~ "MINOR FACTOR"),
  
  "T2", "Which unit?", t2_evidence, t2_verdict,
  
  "T3", "Coverage style fit?",
  paste0("comp% ", p("completion_pct"), ", yds/catch ", p("yards_per_catch"),
         ", explosive ", p("explosive_pass"), ", DPI ", p("dpi_hold_rate")),
  coverage_read,
  
  "T4", "Scheme vs opponent style?",
  paste0("run D pct_worst ", rush_pct, " vs pass D ", pass_pct),
  case_when(rush_pct - pass_pct >= 20 ~ "SUPPORTED - run D is the outlier (light box vs run fits)",
            pass_pct - rush_pct >= 20 ~ "SUPPORTED - pass D is the outlier (coverage, not front)",
            TRUE ~ "NOT SUPPORTED - run and pass similarly bad"),
  
  "T5", "Depth / conditioning? (and when it breaks)",
  paste0("late-game fade ", round(t5_signal["late_game"], 2),
         ", in-drive fade ", round(t5_signal["in_drive"], 2),
         ", Q1 vs rest ", round(t5_signal["first_qtr"], 2)),
  paste(
    if_else(t5_signal["late_game"] >= 0.15, "FATIGUE SUPPORTED - fades late in games",
            "FATIGUE NOT SUPPORTED - no late-game fade"),
    if_else(t5_signal["in_drive"] >= 0.15, "| breaks down as drives extend (adjustments/tempo/communication)", ""),
    if_else(t5_signal["first_qtr"] >= 0.15, "| worst in Q1 (opponents' scripted openers)", "")),
  
  "T6", "Play-calling change?",
  paste0("adj EPA before ", round(t6$adj_epa[1], 2), " vs after ", round(t6$adj_epa[2], 2),
         " (p = ", round(t6_test$p.value, 2), ")"),
  case_when(t6_test$p.value >= 0.1 ~ "NOT CONCLUSIVE - few games; report the trend",
            t6$adj_epa[2] < t6$adj_epa[1] ~ "SUPPORTED - better after change",
            TRUE ~ "WORSE after change")
)
cat("\n==================== WHY SCORECARD ====================\n")
print(scorecard, width = Inf)

# ---- Save tables and charts --------------------------------------------------
list(scorecard = scorecard, a1_rankings = a1, a2_downs = a2, a3_games = a3, a4_zones = a4,
     t2_runs = t2_runs, t3_coverage = t3, t4_opponents = t4_opp, t4_conference = t4_conf,
     t5_fatigue = t5, t5_tempo_drive = t5_tempo_drive, t6_playcalling = t6, second_down = t_2nd) %>%
  iwalk(~ write_csv(.x, file.path(OUT, paste0(.y, ".csv"))))
if (!is.null(t2_levels)) write_csv(t2_levels, file.path(OUT, "t2_levels.csv"))

g1 <- ggplot(a3, aes(week, adj_epa, fill = adj_epa > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = change_week, linetype = "dashed", color = "grey50") +
  geom_text(aes(label = pos_team, vjust = if_else(adj_epa > 0, -0.4, 1.3)), size = 3) +
  scale_fill_manual(values = c("TRUE" = "#C8102E", "FALSE" = "#1E4D2B")) +
  labs(title = "CSU defense vs what each offense normally does",
       subtitle = "Opponent-adjusted EPA/play | above 0 = allowed more than usual | dashed = play-calling change",
       x = "Week", y = "Adjusted EPA/play allowed") +
  theme_minimal()
ggsave(file.path(OUT, "by_game_adjusted.png"), g1, width = 9, height = 5, dpi = 200)

g2 <- ggplot(t3, aes(reorder(metric, pct_worst), pct_worst)) +
  geom_col(fill = "#1E4D2B") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  coord_flip(ylim = c(0, 100)) +
  labs(title = "Pass defense fingerprint (opponent-adjusted)",
       subtitle = "FBS percentile - 100 = worst", x = NULL, y = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "coverage_fingerprint.png"), g2, width = 8, height = 4.5, dpi = 200)

g3 <- inner_join(rename(m_rush$def, rush = d), rename(m_pass$def, pass = d), by = "def_pos_team") %>%
  filter(def_pos_team %in% CONF_TEAMS) %>%
  ggplot(aes(rush, pass, label = def_pos_team, color = def_pos_team == TEAM)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text(vjust = -1, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "#C8102E", "FALSE" = "grey40")) +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = paste(CONF, "defenses: opponent-adjusted run vs pass"),
       subtitle = "Top right = good at both", x = "Adj. rush EPA allowed", y = "Adj. pass EPA allowed") +
  theme_minimal()
ggsave(file.path(OUT, "conference_run_vs_pass.png"), g3, width = 8, height = 6, dpi = 200)

message("\nDone. Tables and charts saved in ./", OUT)