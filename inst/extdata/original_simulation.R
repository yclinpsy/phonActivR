# =============================================================================
# ITEM-LEVEL PHONEME FEATURE SIMULATION
# Purpose-built lexical activation competition model
# =============================================================================
#
# Study:   Japanese-English bilingual spoken word recognition
# Purpose: Generate predicted CV and C competition-effect curves under
#          unconstrained (Language-Specific Grain Size Hypothesis) and
#          moraic-constrained (L1-Transfer Hypothesis) conditions,
#          for comparison against empirical GAMM difference smooths
#
# Architecture: TRACE-style interactive activation (McClelland & Elman, 1986)
# Phonology:    TRACE 7-feature phoneme matrix (McClelland & Elman, 1986, Table 1)
# Stimuli:      CMU Pronouncing Dictionary transcriptions for all 42 items
# Output:       3 publication-ready figures + 1 numerical summary CSV
#
# ── PACKAGES (install once) ───────────────────────────────────────────────────
#   install.packages(c("ggplot2", "dplyr", "tidyr", "patchwork"))
#
# ── USAGE ─────────────────────────────────────────────────────────────────────
#   1. Open this file in RStudio
#   2. Session → Set Working Directory → To Source File Location
#   3. Ctrl+Shift+S  (Source — runs everything)
#
# ── OUTPUT FILES ──────────────────────────────────────────────────────────────
#   Figure1_competition_effects.png  — Model predictions: CV vs C curves under
#                                      each hypothesis (δ=0 and δ=20)
#                                      → MAIN TEXT (Methods or Results)
#   Figure2_asymmetry_gradient.png   — CV−C asymmetry gradient across all δ
#                                      → MAIN TEXT — primary converging-evidence figure
#   Figure3_onset_timing.png         — C onset timing as function of δ
#                                      → MAIN TEXT (Results, alongside GAMM results)
#   simulation_summary.csv           — Numerical results table
#
# ── CONVERGING EVIDENCE STRATEGY ─────────────────────────────────────────────
#   This simulation generates independent, a priori predictions under each
#   theoretical hypothesis (δ=0 vs. δ>0). The GAMM-derived empirical curves
#   are then compared against these predictions in the Results section.
#   The degree of correspondence between the empirical GAMM difference smooth
#   and the closest δ curve serves as the primary hypothesis test.
#
#   Figure 1: Model prediction figure — presented in Results before GAMM results
#             Left panel = Language-Specific Grain Size prediction for both groups
#             Right panel = L1-Transfer prediction for Japanese bilinguals
#
#   Figure 2: Core converging-evidence figure — empirical GAMM asymmetry smooth
#             overlaid on predicted δ curves. δ of closest matching curve
#             = estimated moraic constraint strength.
#             δ = 0 → Language-Specific Grain Size supported
#             δ > 0 → L1-Transfer supported (larger δ = stronger bottleneck)
#
#   Figure 3: Onset-timing figure — empirical C significance-window onset
#             (JP vs. EN) overlaid on predicted onset curve. Read δ from x-axis.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

cat("=============================================================\n")
cat("  Item-Level Phoneme Feature Simulation — Starting\n")
cat("=============================================================\n\n")

# =============================================================================
# SECTION 1: STIMULUS TABLE
# All 42 target items with their C competitor, C control,
# CV competitor, and CV control words
# =============================================================================
stimuli <- data.frame(
  item    = 1:42,
  target  = c("bench","bitter","bottle","budget","budge","desk","depth","dance",
               "fabric","fancy","ferry","fish","finger","gang","handle","heavy",
               "jacket","lever","letter","liquid","listen","lock","lunch","match",
               "magnet","middle","mint","monster","pants","pitch","pillow","rapid",
               "risk","rich","seven","single","sick","test","text","taxi","tick","wish"),
  C_comp  = c("bark","bank","bat","boom","boat","dust","dock","deal","feed","fold",
               "fire","fake","folk","gold","heal","hide","joy","law","lint","loss",
               "lone","lag","list","mop","mean","mark","mall","math","pea","pool",
               "poke","roll","rain","rub","sit","save","sand","tall","toy","tool",
               "toss","wax"),
  C_ctrl  = c("dark","tank","rat","zoom","coat","rust","rock","seal","seed","bold",
               "hire","lake","yolk","mold","meal","ride","boy","saw","hint","moss",
               "tone","tag","mist","top","bean","park","fall","path","tea","cool",
               "joke","poll","pain","pub","kit","cave","band","wall","soy","fool",
               "boss","tax"),
  CV_comp = c("bell","bill","box","bump","bust","dead","deck","dad","fan","fat",
               "fence","fit","fix","gap","hack","hell","jam","leg","lend","lip",
               "link","lot","luck","mad","map","mill","miss","mob","pack","pink",
               "pick","rack","rip","ring","set","silk","sip","tell","tend","task",
               "till","wit"),
  CV_ctrl = c("cell","hill","fox","jump","lust","head","peck","bad","ban","mat",
               "hence","hit","mix","lap","lack","yell","ham","beg","bend","hip",
               "wink","pot","duck","pad","rap","pill","kiss","rob","tack","sink",
               "kick","sack","dip","sing","let","milk","tip","fell","send","mask",
               "fill","bit"),
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION 2: MODEL PARAMETERS
# =============================================================================
TIME_STEPS <- 101     # normalized time steps (0–100% of trial)
ALPHA      <- 0.08    # activation growth rate
GAMMA      <- 0.04    # lateral inhibition strength
DECAY      <- 0.02    # passive decay rate
MAX_ACT    <- 1.0     # activation ceiling
MIN_ACT    <- 0.0     # activation floor
THRESHOLD  <- 0.04    # competition-effect onset detection threshold

# Moraic constraint parameter δ:
#   δ = 0  → unconstrained (Language-Specific Grain Size Hypothesis)
#   δ > 0  → moraic bottleneck (L1-Transfer Hypothesis)
#             C-only feature activation delayed by δ normalized time steps
DELTA_VALUES <- c(0, 10, 20, 30)

# Figure colours — consistent across all three figures
COL_CV     <- "#2166AC"   # CV competition: blue
COL_C      <- "#D6604D"   # C  competition: red-orange
DELTA_COLS <- c("0"  = "#4DAF4A",   # green  — Language-Specific Grain Size
                "10" = "#377EB8",   # blue   — L1-Transfer mild
                "20" = "#FF7F00",   # orange — L1-Transfer moderate
                "30" = "#E41A1C")   # red    — L1-Transfer strong

# =============================================================================
# SECTION 3: CORE ACTIVATION FUNCTION
#
# Implements TRACE-style interactive activation:
#   - Each candidate accumulates activation proportional to phonological overlap
#   - Lateral inhibition from all competing candidates
#   - Passive decay at each time step
#   - Moraic constraint: C-only features unavailable until t >= 1 + delta
#
# Arguments:
#   overlap_comp  : phonological overlap of competitor with target [0, 1]
#   overlap_ctrl  : phonological overlap of control with target    [0, 1]
#   overlap_onset : time step when competitor features become available
#                   = 1         for CV competitors and all controls (mora-aligned)
#                   = 1 + delta for C competitors under moraic constraint
#
# Returns data.frame: time, act_comp, act_ctrl, act_target, competition_effect
#   competition_effect = act_comp − act_ctrl  (mirrors empirical mouse-tracking DV)
# =============================================================================
run_activation <- function(overlap_comp, overlap_ctrl,
                           overlap_onset = 1,
                           time_steps    = TIME_STEPS) {

  act_comp   <- numeric(time_steps)
  act_ctrl   <- numeric(time_steps)
  act_target <- numeric(time_steps)

  for (t in 2:time_steps) {

    # Target word: always mora-aligned, activates from t = 1
    act_target[t] <- act_target[t-1] +
      ALPHA * 1.0 * (MAX_ACT - act_target[t-1]) -
      GAMMA * act_comp[t-1] -
      GAMMA * act_ctrl[t-1] -
      DECAY * act_target[t-1]
    act_target[t] <- max(MIN_ACT, min(MAX_ACT, act_target[t]))

    # Competitor: input gated by moraic constraint
    # C-only features unavailable until overlap_onset is reached
    inp_comp <- ifelse(t >= overlap_onset, overlap_comp, 0.0)
    act_comp[t] <- act_comp[t-1] +
      ALPHA * inp_comp * (MAX_ACT - act_comp[t-1]) -
      GAMMA * act_target[t-1] -
      GAMMA * act_ctrl[t-1] -
      DECAY * act_comp[t-1]
    act_comp[t] <- max(MIN_ACT, min(MAX_ACT, act_comp[t]))

    # Control word: minimal overlap, never moraic-constrained
    act_ctrl[t] <- act_ctrl[t-1] +
      ALPHA * overlap_ctrl * (MAX_ACT - act_ctrl[t-1]) -
      GAMMA * act_target[t-1] -
      GAMMA * act_comp[t-1] -
      DECAY * act_ctrl[t-1]
    act_ctrl[t] <- max(MIN_ACT, min(MAX_ACT, act_ctrl[t]))
  }

  data.frame(
    time               = seq_len(time_steps),
    act_comp           = act_comp,
    act_ctrl           = act_ctrl,
    act_target         = act_target,
    competition_effect = act_comp - act_ctrl
  )
}

# Onset detection: first time step where competition effect exceeds threshold
find_onset <- function(effect_curve, threshold = THRESHOLD) {
  idx <- which(effect_curve > threshold)
  if (length(idx) == 0) return(NA_integer_)
  return(idx[1])
}

# =============================================================================
# SECTION 4: TRACE 7-FEATURE PHONEME MATRIX
# Source: McClelland & Elman (1986), Table 1
# Features: Power (Pow), Vocalic (Voc), Diffuse (Dif), Acute (Acu),
#           Consonantal (Con), Voiced (Voi), Burst (Bur)
# Values:   +1 = feature present, −1 = feature absent
# =============================================================================
trace_features <- list(
  #        Pow  Voc  Dif  Acu  Con  Voi  Bur
  "p"  = c( -1,  -1,  +1,  -1,  +1,  -1,  +1),
  "b"  = c( -1,  -1,  +1,  -1,  +1,  +1,  +1),
  "t"  = c( -1,  -1,  -1,  +1,  +1,  -1,  +1),
  "d"  = c( -1,  -1,  -1,  +1,  +1,  +1,  +1),
  "k"  = c( -1,  -1,  -1,  -1,  +1,  -1,  +1),
  "g"  = c( -1,  -1,  -1,  -1,  +1,  +1,  +1),
  "f"  = c( -1,  -1,  +1,  +1,  +1,  -1,  -1),
  "v"  = c( -1,  -1,  +1,  +1,  +1,  +1,  -1),
  "s"  = c( -1,  -1,  -1,  +1,  +1,  -1,  -1),
  "z"  = c( -1,  -1,  -1,  +1,  +1,  +1,  -1),
  "sh" = c( -1,  -1,  +1,  -1,  +1,  -1,  -1),
  "zh" = c( -1,  -1,  +1,  -1,  +1,  +1,  -1),
  "m"  = c( -1,  +1,  +1,  -1,  +1,  +1,  -1),
  "n"  = c( -1,  +1,  -1,  +1,  +1,  +1,  -1),
  "ng" = c( -1,  +1,  -1,  -1,  +1,  +1,  -1),
  "l"  = c( +1,  +1,  +1,  +1,  +1,  +1,  -1),
  "r"  = c( +1,  +1,  -1,  +1,  +1,  +1,  -1),
  "w"  = c( +1,  +1,  +1,  -1,  -1,  +1,  -1),
  "y"  = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
  "hh" = c( -1,  -1,  -1,  -1,  -1,  -1,  -1),
  "ch" = c( -1,  -1,  +1,  -1,  +1,  -1,  +1),
  "jh" = c( -1,  -1,  +1,  -1,  +1,  +1,  +1),
  "th" = c( -1,  -1,  +1,  +1,  +1,  -1,  -1),
  "dh" = c( -1,  -1,  +1,  +1,  +1,  +1,  -1),
  # Vowels
  "ae" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
  "ah" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "ao" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "aw" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "ay" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
  "eh" = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
  "er" = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
  "ey" = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
  "ih" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
  "iy" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
  "ow" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "oy" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "uh" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "uw" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
  "aa" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1)
)

# Phoneme similarity: proportion of matching TRACE feature values (range 0–1)
phoneme_similarity <- function(p1, p2) {
  if (!(p1 %in% names(trace_features)) || !(p2 %in% names(trace_features))) {
    return(0.3)   # conservative default for phonemes not in matrix
  }
  sum(trace_features[[p1]] == trace_features[[p2]]) / 7.0
}

# =============================================================================
# SECTION 5: CMU ONSET TRANSCRIPTIONS — ALL 42 ITEMS + ALL COMPETITORS/CONTROLS
# Format: named list of character vectors
#   Targets and CV words: c("consonant", "vowel")  e.g. c("b", "eh")
#   C words (consonant-only onset): c("consonant") e.g. c("b")
# =============================================================================
cmu_onsets <- list(
  # ── Targets ──────────────────────────────────────────────────────────────
  "bench"   = c("b",  "eh"), "bitter"  = c("b",  "ih"),
  "bottle"  = c("b",  "ao"), "budget"  = c("b",  "ah"),
  "budge"   = c("b",  "ah"), "desk"    = c("d",  "eh"),
  "depth"   = c("d",  "eh"), "dance"   = c("d",  "ae"),
  "fabric"  = c("f",  "ae"), "fancy"   = c("f",  "ae"),
  "ferry"   = c("f",  "eh"), "fish"    = c("f",  "ih"),
  "finger"  = c("f",  "ih"), "gang"    = c("g",  "ae"),
  "handle"  = c("hh", "ae"), "heavy"   = c("hh", "eh"),
  "jacket"  = c("jh", "ae"), "lever"   = c("l",  "eh"),
  "letter"  = c("l",  "eh"), "liquid"  = c("l",  "ih"),
  "listen"  = c("l",  "ih"), "lock"    = c("l",  "ao"),
  "lunch"   = c("l",  "ah"), "match"   = c("m",  "ae"),
  "magnet"  = c("m",  "ae"), "middle"  = c("m",  "ih"),
  "mint"    = c("m",  "ih"), "monster" = c("m",  "ao"),
  "pants"   = c("p",  "ae"), "pitch"   = c("p",  "ih"),
  "pillow"  = c("p",  "ih"), "rapid"   = c("r",  "ae"),
  "risk"    = c("r",  "ih"), "rich"    = c("r",  "ih"),
  "seven"   = c("s",  "eh"), "single"  = c("s",  "ih"),
  "sick"    = c("s",  "ih"), "test"    = c("t",  "eh"),
  "text"    = c("t",  "eh"), "taxi"    = c("t",  "ae"),
  "tick"    = c("t",  "ih"), "wish"    = c("w",  "ih"),

  # ── C competitors (consonant-only onset) ─────────────────────────────────
  "bark"  = c("b"),  "bank"  = c("b"),  "bat"   = c("b"),  "boom"  = c("b"),
  "boat"  = c("b"),  "dust"  = c("d"),  "dock"  = c("d"),  "deal"  = c("d"),
  "feed"  = c("f"),  "fold"  = c("f"),  "fire"  = c("f"),  "fake"  = c("f"),
  "folk"  = c("f"),  "gold"  = c("g"),  "heal"  = c("hh"), "hide"  = c("hh"),
  "joy"   = c("jh"), "law"   = c("l"),  "lint"  = c("l"),  "loss"  = c("l"),
  "lone"  = c("l"),  "lag"   = c("l"),  "list"  = c("l"),  "mop"   = c("m"),
  "mean"  = c("m"),  "mark"  = c("m"),  "mall"  = c("m"),  "math"  = c("m"),
  "pea"   = c("p"),  "pool"  = c("p"),  "poke"  = c("p"),  "roll"  = c("r"),
  "rain"  = c("r"),  "rub"   = c("r"),  "sit"   = c("s"),  "save"  = c("s"),
  "sand"  = c("s"),  "tall"  = c("t"),  "toy"   = c("t"),  "tool"  = c("t"),
  "toss"  = c("t"),  "wax"   = c("w"),

  # ── C controls (consonant-only onset, different consonant from target) ────
  "dark"  = c("d"),  "tank"  = c("t"),  "rat"   = c("r"),  "zoom"  = c("z"),
  "coat"  = c("k"),  "rust"  = c("r"),  "rock"  = c("r"),  "seal"  = c("s"),
  "seed"  = c("s"),  "bold"  = c("b"),  "hire"  = c("hh"), "lake"  = c("l"),
  "yolk"  = c("y"),  "mold"  = c("m"),  "meal"  = c("m"),  "ride"  = c("r"),
  "boy"   = c("b"),  "saw"   = c("s"),  "hint"  = c("hh"), "moss"  = c("m"),
  "tone"  = c("t"),  "tag"   = c("t"),  "mist"  = c("m"),  "top"   = c("t"),
  "bean"  = c("b"),  "park"  = c("p"),  "fall"  = c("f"),  "path"  = c("p"),
  "tea"   = c("t"),  "cool"  = c("k"),  "joke"  = c("jh"), "poll"  = c("p"),
  "pain"  = c("p"),  "pub"   = c("p"),  "kit"   = c("k"),  "cave"  = c("k"),
  "band"  = c("b"),  "wall"  = c("w"),  "soy"   = c("s"),  "fool"  = c("f"),
  "boss"  = c("b"),  "tax"   = c("t"),

  # ── CV competitors (consonant + vowel onset, same CV as target) ───────────
  "bell"  = c("b",  "eh"), "bill"  = c("b",  "ih"), "box"   = c("b",  "ao"),
  "bump"  = c("b",  "ah"), "bust"  = c("b",  "ah"), "dead"  = c("d",  "eh"),
  "deck"  = c("d",  "eh"), "dad"   = c("d",  "ae"), "fan"   = c("f",  "ae"),
  "fat"   = c("f",  "ae"), "fence" = c("f",  "eh"), "fit"   = c("f",  "ih"),
  "fix"   = c("f",  "ih"), "gap"   = c("g",  "ae"), "hack"  = c("hh", "ae"),
  "hell"  = c("hh", "eh"), "jam"   = c("jh", "ae"), "leg"   = c("l",  "eh"),
  "lend"  = c("l",  "eh"), "lip"   = c("l",  "ih"), "link"  = c("l",  "ih"),
  "lot"   = c("l",  "ao"), "luck"  = c("l",  "ah"), "mad"   = c("m",  "ae"),
  "map"   = c("m",  "ae"), "mill"  = c("m",  "ih"), "miss"  = c("m",  "ih"),
  "mob"   = c("m",  "ao"), "pack"  = c("p",  "ae"), "pink"  = c("p",  "ih"),
  "pick"  = c("p",  "ih"), "rack"  = c("r",  "ae"), "rip"   = c("r",  "ih"),
  "ring"  = c("r",  "ih"), "set"   = c("s",  "eh"), "silk"  = c("s",  "ih"),
  "sip"   = c("s",  "ih"), "tell"  = c("t",  "eh"), "tend"  = c("t",  "eh"),
  "task"  = c("t",  "ae"), "till"  = c("t",  "ih"), "wit"   = c("w",  "ih"),

  # ── CV controls (consonant + vowel onset, different CV from target) ───────
  "cell"  = c("s",  "eh"), "hill"  = c("hh", "ih"), "fox"   = c("f",  "ao"),
  "jump"  = c("jh", "ah"), "lust"  = c("l",  "ah"), "head"  = c("hh", "eh"),
  "peck"  = c("p",  "eh"), "bad"   = c("b",  "ae"), "ban"   = c("b",  "ae"),
  "mat"   = c("m",  "ae"), "hence" = c("hh", "eh"), "hit"   = c("hh", "ih"),
  "mix"   = c("m",  "ih"), "lap"   = c("l",  "ae"), "lack"  = c("l",  "ae"),
  "yell"  = c("y",  "eh"), "ham"   = c("hh", "ae"), "beg"   = c("b",  "eh"),
  "bend"  = c("b",  "eh"), "hip"   = c("hh", "ih"), "wink"  = c("w",  "ih"),
  "pot"   = c("p",  "ao"), "duck"  = c("d",  "ah"), "pad"   = c("p",  "ae"),
  "rap"   = c("r",  "ae"), "pill"  = c("p",  "ih"), "kiss"  = c("k",  "ih"),
  "rob"   = c("r",  "ao"), "tack"  = c("t",  "ae"), "sink"  = c("s",  "ih"),
  "kick"  = c("k",  "ih"), "sack"  = c("s",  "ae"), "dip"   = c("d",  "ih"),
  "sing"  = c("s",  "ih"), "let"   = c("l",  "eh"), "milk"  = c("m",  "ih"),
  "tip"   = c("t",  "ih"), "fell"  = c("f",  "eh"), "send"  = c("s",  "eh"),
  "mask"  = c("m",  "ae"), "fill"  = c("f",  "ih"), "bit"   = c("b",  "ih")
)

# =============================================================================
# SECTION 6: OVERLAP COMPUTATION
# Phonological overlap derived from TRACE feature matrix
# CV overlap = mean of consonant similarity + vowel similarity
# C  overlap = consonant similarity × 0.5  (one of two onset positions shared)
# Control overlap = competitor overlap × 0.15  (minimal, near-zero baseline)
# RATIONALE: Controls are not phonologically identical to the target, but they
# are real English words that share some residual phonological neighbourhood
# activation. The 0.15 scaling approximates this baseline activation level,
# ensuring the competition effect (competitor − control) reflects genuine
# phonological overlap rather than comparing against a zero-activation baseline
# that would overestimate competition magnitude.
# =============================================================================
compute_item_overlap <- function(target_word, competitor_word, comp_type) {
  tgt <- cmu_onsets[[target_word]]
  cmp <- cmu_onsets[[competitor_word]]

  if (is.null(tgt) || is.null(cmp)) return(0.3)

  if (comp_type == "C") {
    return(phoneme_similarity(tgt[1], cmp[1]) * 0.5)

  } else if (comp_type == "CV") {
    c_sim <- phoneme_similarity(tgt[1], cmp[1])
    v_sim <- if (length(tgt) > 1 && length(cmp) > 1) {
      phoneme_similarity(tgt[2], cmp[2])
    } else { 0.5 }
    return((c_sim + v_sim) / 2.0)
  }
  return(0.1)
}

# Run activation model for a single stimulus item
run_item <- function(target, C_comp, C_ctrl, CV_comp, CV_ctrl, delta = 0) {

  ovl_CV_comp <- compute_item_overlap(target, CV_comp, "CV")
  ovl_CV_ctrl <- compute_item_overlap(target, CV_ctrl, "CV") * 0.15
  ovl_C_comp  <- compute_item_overlap(target, C_comp,  "C")
  ovl_C_ctrl  <- compute_item_overlap(target, C_ctrl,  "C") * 0.15

  # CV competitors: mora-aligned — no moraic constraint applied (onset = 1)
  cv_out <- run_activation(ovl_CV_comp, ovl_CV_ctrl, overlap_onset = 1)
  # C competitors: sub-moraic — delayed by delta under moraic constraint
  c_out  <- run_activation(ovl_C_comp,  ovl_C_ctrl,  overlap_onset = 1 + delta)

  list(cv_effect = cv_out$competition_effect,
       c_effect  = c_out$competition_effect)
}

# =============================================================================
# SECTION 7: RUN SIMULATION ACROSS ALL 42 ITEMS × 4 DELTA VALUES
# =============================================================================
cat("  Running item-level simulation...\n\n")

# Pre-flight check: verify that every word referenced in the stimuli table has
# a CMU onset transcription. Missing entries will fall back to the conservative
# default (overlap = 0.3) in compute_item_overlap(), which is acceptable but
# should be flagged explicitly so transcriptions can be verified.
all_words <- unique(c(stimuli$target,  stimuli$C_comp,  stimuli$C_ctrl,
                      stimuli$CV_comp, stimuli$CV_ctrl))
missing   <- setdiff(all_words, names(cmu_onsets))
if (length(missing) > 0) {
  cat("  WARNING: The following words lack CMU onset transcriptions and will\n")
  cat("  use the conservative default overlap (0.3). Verify transcriptions:\n")
  cat("  ", paste(missing, collapse = ", "), "\n\n")
} else {
  cat("  All 42 items verified: CMU onset transcriptions complete.\n\n")
}

n_items <- nrow(stimuli)
results <- list()

for (delta in DELTA_VALUES) {

  cv_matrix <- matrix(NA, nrow = n_items, ncol = TIME_STEPS)
  c_matrix  <- matrix(NA, nrow = n_items, ncol = TIME_STEPS)

  for (i in seq_len(n_items)) {
    row <- stimuli[i, ]
    item_out <- run_item(
      row$target, row$C_comp, row$C_ctrl,
      row$CV_comp, row$CV_ctrl,
      delta = delta
    )
    cv_matrix[i, ] <- item_out$cv_effect
    c_matrix[i, ]  <- item_out$c_effect
  }

  cv_mean <- colMeans(cv_matrix)
  c_mean  <- colMeans(c_matrix)
  cv_se   <- apply(cv_matrix, 2, sd) / sqrt(n_items)
  c_se    <- apply(c_matrix,  2, sd) / sqrt(n_items)

  results[[as.character(delta)]] <- list(
    cv_effect = cv_mean,
    c_effect  = c_mean,
    cv_se     = cv_se,
    c_se      = c_se,
    asymmetry = cv_mean - c_mean
  )

  cv_on <- find_onset(cv_mean)
  c_on  <- find_onset(c_mean)
  delay <- if (is.na(cv_on) || is.na(c_on)) "NA" else as.character(c_on - cv_on)

  cat(sprintf("  \u03b4 = %2d | CV onset = %s%% | C onset = %s%% | Delay = %s%%\n",
              delta,
              ifelse(is.na(cv_on), "NA", cv_on),
              ifelse(is.na(c_on),  "NA", c_on),
              delay))
}

cat("\n")

# =============================================================================
# SECTION 8: FIGURE 1 — CV AND C COMPETITION EFFECTS (δ=0 vs δ=20)
#
# What this figure shows:
#   Two panels — left is Language-Specific Grain Size prediction (δ=0),
#   right is L1-Transfer prediction (δ=20).
#   Each panel shows the CV competition curve (blue solid) and the
#   C competition curve (red dashed) with 95% CI bands across 42 items.
#
# Hypothesis reading:
#   LEFT panel  → both groups should show this if Language-Specific Grain Size
#   RIGHT panel → Japanese bilinguals should show this if L1-Transfer
#
# How to read:
#   After running GAMMs, the empirical CV and C smooths per group are overlaid
#   onto the matching panel (see Section 12 overlay). The panel whose predicted
#   curves most closely resemble the empirical JP bilingual curves determines
#   which hypothesis is supported.
#
# Placement: MAIN TEXT (Results — Model Predictions subsection)
# =============================================================================
cat("  Generating Figure 1 (CV vs C competition effects)...\n")

make_fig1_panel <- function(delta, panel_title, title_color) {
  res  <- results[[as.character(delta)]]
  time <- seq_len(TIME_STEPS)

  df <- data.frame(
    time      = rep(time, 2),
    effect    = c(res$cv_effect, res$c_effect),
    lower     = c(res$cv_effect - 1.96 * res$cv_se,
                  res$c_effect  - 1.96 * res$c_se),
    upper     = c(res$cv_effect + 1.96 * res$cv_se,
                  res$c_effect  + 1.96 * res$c_se),
    condition = rep(c("CV Competition Effect", "C Competition Effect"),
                    each = TIME_STEPS)
  )
  df$condition <- factor(df$condition,
    levels = c("CV Competition Effect", "C Competition Effect"))

  cv_on <- find_onset(res$cv_effect)
  c_on  <- find_onset(res$c_effect)

  p <- ggplot(df, aes(x = time, y = effect,
                      color = condition, fill = condition)) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.7) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.15, color = NA) +
    geom_line(aes(linetype = condition), linewidth = 1.2) +
    scale_color_manual(
      values = c("CV Competition Effect" = COL_CV,
                 "C Competition Effect"  = COL_C),
      name = NULL) +
    scale_fill_manual(
      values = c("CV Competition Effect" = COL_CV,
                 "C Competition Effect"  = COL_C),
      name = NULL) +
    scale_linetype_manual(
      values = c("CV Competition Effect" = "solid",
                 "C Competition Effect"  = "dashed"),
      name = NULL)

  if (!is.na(cv_on))
    p <- p + geom_vline(xintercept = cv_on, color = COL_CV,
                        linewidth = 0.8, linetype = "dotted", alpha = 0.7)
  if (!is.na(c_on)) {
    p <- p + geom_vline(xintercept = c_on, color = COL_C,
                        linewidth = 0.8, linetype = "dotted", alpha = 0.7)
    # Only annotate the delay if BOTH onsets are defined — cv_on=NA means no
    # threshold crossing yet, and c_on - NA would silently produce NA text.
    if (delta > 0 && !is.na(cv_on) && !is.na(c_on)) {
      delay <- c_on - cv_on
      p <- p + annotate("text",
                         x = c_on + 4, y = 0.14,
                         label = paste0("C onset delayed\nby ", delay, "% vs CV"),
                         color = COL_C, size = 3.2, hjust = 0)
    }
  }

  p +
    scale_x_continuous(breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%")) +
    scale_y_continuous(limits = c(-0.05, 0.52),
                       breaks = seq(0, 0.5, 0.1)) +
    labs(title    = panel_title,
         subtitle = ifelse(delta == 0,
           "Predicted pattern: English Monolinguals AND Japanese Bilinguals if no moraic bottleneck",
           "Predicted pattern: Japanese Bilinguals only if moraic bottleneck present"),
         x = "Normalized Time (% of trial)",
         y = "Competition Effect\n(Competitor \u2212 Control Activation)") +
    theme_bw(base_size = 11) +
    theme(
      legend.position   = "bottom",
      legend.text       = element_text(size = 10),
      plot.title        = element_text(size = 10.5, face = "bold",
                                       color = title_color),
      plot.subtitle     = element_text(size = 9, color = "gray45"),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_text(size = 10)
    )
}

p1_left  <- make_fig1_panel(0,
  "\u03b4 = 0  \u2192  Language-Specific Grain Size Hypothesis\n(No moraic bottleneck — C onset = CV onset)",
  "#2ca02c")

p1_right <- make_fig1_panel(20,
  "\u03b4 = 20  \u2192  L1-Transfer Hypothesis\n(Moraic bottleneck — C onset delayed relative to CV)",
  "#d62728")

fig1 <- (p1_left | p1_right) +
  plot_annotation(
    title    = "Figure 1: Predicted CV and C Competition Effects",
    subtitle = paste0(
      "Item-Level Phoneme Feature Simulation (N = 42 stimulus items, 95% CI across items)\n",
      "Competition effect = Competitor activation \u2212 Control activation (mirrors mouse-tracking DV)\n",
      "Left panel: predicted pattern for both groups (no moraic bottleneck) | ",
      "Right panel: predicted pattern for JP bilinguals (moraic bottleneck present)"
    ),
    caption  = paste0(
      "TRACE-style activation architecture (McClelland & Elman, 1986) | ",
      "Phonological overlap computed from TRACE 7-feature phoneme matrix | ",
      "CMU Pronouncing Dictionary transcriptions\n",
      "\u03b4 = moraic constraint parameter: C-only feature activation delayed by \u03b4 normalized time steps"
    ),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 9.5, color = "gray40"),
      plot.caption  = element_text(size = 8,   color = "gray55")
    )
  )

ggsave("Figure1_competition_effects.png", fig1,
       width = 14, height = 6.5, dpi = 300)
cat("    Saved: Figure1_competition_effects.png\n")

# =============================================================================
# SECTION 9: FIGURE 2 — CV−C ASYMMETRY GRADIENT (ALL δ VALUES)
#
# What this figure shows:
#   Four asymmetry curves — one per δ value.
#   Y-axis = CV competition effect minus C competition effect at each time step.
#   Green (δ=0) = Language-Specific Grain Size prediction: asymmetry arises
#     only from overlap magnitude difference, CV and C activate simultaneously.
#   Orange/Blue/Red (δ>0) = L1-Transfer predictions of increasing strength:
#     asymmetry grows larger and peaks earlier as moraic constraint strengthens.
#
# Hypothesis reading:
#   δ = 0 curve  → Language-Specific Grain Size supported
#   δ > 0 curve  → L1-Transfer supported; δ magnitude = bottleneck strength
#
# How to use with GAMM results:
#   Compute your empirical CV−C difference smooth for Japanese bilinguals
#   (CV smooth minus C smooth at each time step from your GAMM output).
#   Add it as a thick black geom_line() to this figure.
#   The predicted δ curve it most closely tracks = your answer.
#
# Placement: MAIN TEXT (Results section, after GAMM results)
#            This is your primary computational modeling figure
# =============================================================================
cat("  Generating Figure 2 (CV\u2212C asymmetry gradient)...\n")

asym_df <- bind_rows(lapply(DELTA_VALUES, function(d) {
  res <- results[[as.character(d)]]
  data.frame(
    time      = seq_len(TIME_STEPS),
    asymmetry = res$asymmetry,
    # BUG FIX: SE of difference (CV - C) uses error propagation (quadrature),
    # not linear sum. sqrt(se_cv^2 + se_c^2), not se_cv + se_c.
    asym_lo   = res$asymmetry - 1.96 * sqrt(res$cv_se^2 + res$c_se^2),
    asym_hi   = res$asymmetry + 1.96 * sqrt(res$cv_se^2 + res$c_se^2),
    delta     = factor(d, levels = as.character(DELTA_VALUES))
  )
}))

delta_labels <- c(
  "0"  = "\u03b4 = 0   \u2192  Language-Specific Grain Size (no moraic bottleneck)",
  "10" = "\u03b4 = 10  \u2192  L1-Transfer: mild moraic bottleneck",
  "20" = "\u03b4 = 20  \u2192  L1-Transfer: moderate moraic bottleneck",
  "30" = "\u03b4 = 30  \u2192  L1-Transfer: strong moraic bottleneck"
)

fig2 <- ggplot(asym_df, aes(x = time, y = asymmetry,
                             color = delta, fill = delta,
                             linetype = delta)) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.8,
             linetype = "dotted") +
  geom_ribbon(aes(ymin = asym_lo, ymax = asym_hi),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 1.3) +

  scale_color_manual(values = DELTA_COLS, labels = delta_labels, name = NULL) +
  scale_fill_manual( values = DELTA_COLS, labels = delta_labels, name = NULL) +
  scale_linetype_manual(
    values = c("0" = "solid", "10" = "longdash",
               "20" = "dashed", "30" = "dotted"),
    labels = delta_labels, name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%")) +
  scale_y_continuous(limits = c(-0.02, 0.50),
                     breaks = seq(0, 0.5, 0.1)) +
  labs(
    title    = "Figure 2: Predicted CV\u2212C Asymmetry as a Function of Moraic Constraint (\u03b4)",
    subtitle = paste0(
      "Item-Level Phoneme Feature Simulation (N = 42 items, 95% CI across items)\n",
      "Y-axis = CV competition effect \u2212 C competition effect at each normalized time step\n",
      "Empirical GAMM CV\u2212C difference smooth for each group is overlaid as converging evidence"
    ),
    x        = "Normalized Time (% of trial)",
    y        = "CV Competition Effect \u2212 C Competition Effect",
    caption  = paste0(
      "RESULT INTERPRETATION: ",
      "Empirical smooth tracks \u03b4 = 0 \u2192 Language-Specific Grain Size Hypothesis supported  |  ",
      "Empirical smooth tracks \u03b4 > 0 \u2192 L1-Transfer Hypothesis supported\n",
      "Larger \u03b4 = stronger moraic bottleneck. ",
      "Convert \u03b4 onset delay % to ms: multiply by mean trial duration / 100"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position   = "right",
    legend.text       = element_text(size = 9.5),
    legend.key.width  = unit(1.8, "cm"),
    plot.title        = element_text(size = 13, face = "bold"),
    plot.subtitle     = element_text(size = 9.5, color = "gray40"),
    plot.caption      = element_text(size = 8.2, color = "gray55"),
    panel.grid.minor  = element_blank()
  )

ggsave("Figure2_asymmetry_gradient.png", fig2,
       width = 12, height = 6.5, dpi = 300)
cat("    Saved: Figure2_asymmetry_gradient.png\n")

# =============================================================================
# SECTION 10: FIGURE 3 — C COMPETITION ONSET TIMING (ALL δ VALUES)
#
# What this figure shows:
#   X-axis = moraic constraint strength (δ value)
#   Y-axis = competition onset in % of normalized trial
#   Red circles = C competition onset (rises with increasing δ)
#   Blue squares = CV competition onset (flat — CV is always mora-aligned)
#   Gray band = onset delay gap between CV and C
#   Labels = delay in % and estimated ms (assumes 1000 ms trial)
#
# Hypothesis reading:
#   If empirical Japanese bilingual C onset matches red line at δ=0 → L-SGS
#   If empirical Japanese bilingual C onset matches red line at δ>0 → L1-Transfer
#
# How to read:
#   The empirical C significance-window onset for JP and EN groups (from GAMMs)
#   is added as horizontal reference lines (see Section 12 overlay). Reading
#   across to the x-axis gives the estimated δ — the moraic constraint strength.
#
# Placement: MAIN TEXT (Results — alongside GAMM onset comparisons)
# =============================================================================
cat("  Generating Figure 3 (C onset timing)...\n")

onset_df <- bind_rows(lapply(DELTA_VALUES, function(d) {
  res   <- results[[as.character(d)]]
  cv_on <- find_onset(res$cv_effect)
  c_on  <- find_onset(res$c_effect)
  data.frame(
    delta    = d,
    cv_onset = cv_on,
    c_onset  = c_on,
    delay    = ifelse(is.na(cv_on) | is.na(c_on), NA_integer_, c_on - cv_on)
  )
}))

delay_label_df <- onset_df %>%
  filter(!is.na(delay)) %>%
  mutate(
    label_pct = paste0("+", delay, "%"),
    label_ms  = paste0("(\u2248", delay * 10, " ms*)")
  )

fig3 <- ggplot(onset_df, aes(x = delta)) +
  # Onset delay shading
  geom_ribbon(data = onset_df %>% filter(!is.na(c_onset) & !is.na(cv_onset)),
              aes(ymin = cv_onset, ymax = c_onset),
              fill = "gray80", alpha = 0.6) +
  # CV reference line (constant)
  geom_line(aes(y = cv_onset),  color = COL_CV, linewidth = 1.4,
            linetype = "dashed") +
  geom_point(aes(y = cv_onset), color = COL_CV, size = 5, shape = 15) +
  # C onset line (rises with delta)
  geom_line(aes(y = c_onset),   color = COL_C,  linewidth = 1.8) +
  geom_point(aes(y = c_onset),  color = COL_C,  size = 6) +
  # Delay labels
  geom_text(data = delay_label_df,
            aes(x = delta + 1.0, y = c_onset + 2.5,
                label = paste(label_pct, label_ms)),
            color = COL_C, size = 3.8, fontface = "bold", hjust = 0) +
  # CV reference annotation
  annotate("text", x = 0.5, y = onset_df$cv_onset[1] - 2.5,
           label = "CV onset — constant\n(mora-aligned, never delayed)",
           color = COL_CV, size = 3.5, hjust = 0) +
  scale_x_continuous(breaks = DELTA_VALUES,
                     labels = paste0("\u03b4 = ", DELTA_VALUES)) +
  scale_y_continuous(breaks = seq(0, 55, 5),
                     labels = paste0(seq(0, 55, 5), "%"),
                     limits = c(0, 52)) +
  labs(
    title    = "Figure 3: Predicted C Competition Onset as a Function of Moraic Constraint (\u03b4)",
    subtitle = paste0(
      "Item-Level Phoneme Feature Simulation (N = 42 stimulus items)\n",
      "Red circles = C competition onset  |  Blue squares = CV onset (reference, constant)\n",
      "Shaded band = onset delay  |  Labels = delay in % and estimated ms (assuming 1000 ms trial)"
    ),
    x        = "Moraic Constraint Parameter (\u03b4)",
    y        = "Competition Onset (% of normalized trial)",
    caption  = paste0(
      "* ms estimates assume 1000 ms mean trial duration. ",
      "Adjust: onset_delay% \u00d7 your_mean_trial_duration / 100\n",
      "RESULT INTERPRETATION: ",
      "Empirical C onset at \u03b4 = 0 row \u2192 Language-Specific Grain Size  |  ",
      "Empirical C onset at \u03b4 > 0 row \u2192 L1-Transfer (read off bottleneck strength)"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 9.5, color = "gray40"),
    plot.caption     = element_text(size = 8.2, color = "gray55"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 11, face = "bold")
  )

ggsave("Figure3_onset_timing.png", fig3,
       width = 11, height = 6.5, dpi = 300)
cat("    Saved: Figure3_onset_timing.png\n")

# =============================================================================
# SECTION 11: NUMERICAL SUMMARY TABLE
# =============================================================================
cat("\n=============================================================\n")
cat("  NUMERICAL SUMMARY\n")
cat("=============================================================\n\n")

summary_df <- bind_rows(lapply(DELTA_VALUES, function(d) {
  res   <- results[[as.character(d)]]
  cv_on <- find_onset(res$cv_effect)
  c_on  <- find_onset(res$c_effect)
  data.frame(
    Delta_delta       = d,
    Hypothesis        = ifelse(d == 0,
      "Language-Specific Grain Size", "L1-Transfer"),
    CV_Onset_pct      = ifelse(is.na(cv_on), NA, cv_on),
    C_Onset_pct       = ifelse(is.na(c_on),  NA, c_on),
    Onset_Delay_pct   = ifelse(is.na(cv_on) | is.na(c_on), NA, c_on - cv_on),
    Max_CV_Effect     = round(max(res$cv_effect), 3),
    Max_C_Effect      = round(max(res$c_effect),  3),
    Max_CV_minus_C    = round(max(res$asymmetry), 3)
  )
}))

print(summary_df, row.names = FALSE)
write.csv(summary_df, "simulation_summary.csv", row.names = FALSE)
cat("\n  Saved: simulation_summary.csv\n")

# =============================================================================
# DONE
# =============================================================================
cat("\n=============================================================\n")
cat("  SIMULATION COMPLETE\n")
cat("  3 figures + 1 CSV saved to your working directory\n")
cat("=============================================================\n\n")

cat("  FILES SAVED:\n")
cat("  Figure1_competition_effects.png  \u2014 MAIN TEXT (Model Predictions)\n")
cat("     CV vs C predicted curves at \u03b4=0 (Language-Specific Grain Size)\n")
cat("     and \u03b4=20 (L1-Transfer) — presented before empirical GAMM results\n\n")
cat("  Figure2_asymmetry_gradient.png   \u2014 MAIN TEXT (Converging Evidence)\n")
cat("     CV\u2212C asymmetry curves across all \u03b4 values\n")
cat("     Empirical GAMM CV\u2212C difference smooth overlaid as thick black line\n")
cat("     Closest \u03b4 curve = estimated moraic constraint strength\n\n")
cat("  Figure3_onset_timing.png         \u2014 MAIN TEXT (Results, onset comparison)\n")
cat("     C onset timing as function of \u03b4\n")
cat("     Empirical C significance-window onset for JP and EN overlaid\n")
cat("     Read off \u03b4 from x-axis = estimated bottleneck strength\n\n")
cat("  simulation_summary.csv           \u2014 Numerical results table\n\n")

cat("  CONVERTING ONSET DELAY TO MILLISECONDS:\n")
cat("    ms = onset_delay_pct x mean_trial_duration_ms / 100\n")
cat("    e.g. if delay = 20% and mean trial = 1500 ms:\n")
cat("         20 x 1500 / 100 = 300 ms estimated bottleneck\n\n")

# =============================================================================
# REPRODUCIBILITY FOOTER
# =============================================================================
cat("=============================================================\n")
cat("  SESSION INFO (for reproducibility)\n")
cat("=============================================================\n\n")
print(sessionInfo())

# =============================================================================
# SECTION 12: EMPIRICAL GAMM OVERLAY — CONVERGING EVIDENCE ANALYSIS
# =============================================================================
#
# PURPOSE:
#   Reads the smooth-extraction CSVs produced by the GAMM scripts and overlays
#   empirical competition trajectories onto all three simulation figures.
#   This is the primary hypothesis-testing step: the degree of correspondence
#   between the empirical GAMM curves and the predicted simulation curves
#   (at specific δ values) constitutes converging evidence for each hypothesis.
#
#   The simulation predictions are INDEPENDENT of the empirical data — they
#   are derived purely from the theoretical architecture and phonological
#   overlap computed from the TRACE feature matrix. The match (or mismatch)
#   between predicted and observed curves therefore reflects whether the
#   theoretical mechanism (moraic bottleneck at δ>0, or no bottleneck at δ=0)
#   is consistent with how participants actually processed the spoken words.
#
# FILE PRIORITY (simulation_final.R prefers the between-experiment GAMM):
#   1. Combined_smooth_data.csv  — PREFERRED: between-experiment GAMM estimates
#      for JP and EN in a single unified model (Combined_GAMM.R Section 9)
#   2. Exp1_smooth_data.csv + Exp2_smooth_data.csv  — FALLBACK: within-
#      experiment estimates used if Combined not yet available
#   If neither is found, this section is skipped with an informative message.
#
# SCALE RECONCILIATION:
#   Simulation y-axis: activation units (0–1, dimensionless)
#   GAMM y-axis:       pixels (px) of mouse x-coordinate deflection
#   These cannot be directly plotted on the same axis. We apply
#   min-max normalization so that each empirical curve's peak maps to
#   the peak of the corresponding simulation curve. Shape is preserved;
#   only the scale changes. A footnote is added to all overlay figures
#   noting the normalization.
#
# OUTPUT FILES:
#   Figure1_with_empirical.png  — Model predictions + empirical curves (main text)
#   Figure2_with_empirical.png  — PRIMARY converging-evidence figure (main text)
#   Figure3_with_empirical.png  — Onset comparison figure (main text)
#
# FIGURE-BY-FIGURE LOGIC:
#
#   FIGURE 1  (CV vs C competition effects, δ=0 left / δ=20 right)
#     Left panel (δ=0):  EN C and CV effects overlaid as thin grey lines
#                        with ± 1 SE ribbon in transparent grey
#     Right panel (δ=20): JP C and CV effects overlaid as thin dark lines
#                         with ± 1 SE ribbon
#     If JP empirical lines match the LEFT panel (δ=0) → Language-Specific
#       Grain Size supported.
#     If JP empirical lines match the RIGHT panel (δ=20) → L1-Transfer
#       supported.
#
#   FIGURE 2  (CV−C asymmetry gradient, all δ values)
#     JP asymmetry smooth overlaid as a THICK BLACK LINE with grey ± 1 SE band.
#     EN asymmetry overlaid as a thin grey line for comparison.
#     The predicted δ curve the JP black line most closely tracks =
#       your estimated moraic constraint strength.
#     This is the PRIMARY hypothesis-adjudication figure.
#
#   FIGURE 3  (C onset timing as function of δ)
#     JP derivative-based C onset (% of trial) added as a HORIZONTAL DASHED
#       RED LINE. The δ value on the x-axis directly below where this line
#       intersects the simulated C onset curve = estimated δ.
#     EN C onset added as a HORIZONTAL DOTTED GREY LINE for reference.
# =============================================================================

cat("\n=============================================================\n")
cat("  SECTION 12: EMPIRICAL GAMM OVERLAY\n")
cat("=============================================================\n\n")

# ── Load empirical smooth data ──────────────────────────────────────────────

empirical_data <- NULL

if (file.exists("Combined_smooth_data.csv")) {

  empirical_data <- read.csv("Combined_smooth_data.csv",
                             stringsAsFactors = FALSE)
  cat("  Loaded Combined_smooth_data.csv (between-experiment GAMM — preferred)\n")
  cat("  Rows:", nrow(empirical_data),
      "| Groups:", paste(unique(empirical_data$group), collapse = ", "), "\n\n")

} else if (file.exists("Exp1_smooth_data.csv") &&
           file.exists("Exp2_smooth_data.csv")) {

  d1 <- read.csv("Exp1_smooth_data.csv", stringsAsFactors = FALSE)
  d2 <- read.csv("Exp2_smooth_data.csv", stringsAsFactors = FALSE)
  empirical_data <- rbind(d1, d2)
  cat("  Loaded Exp1_smooth_data.csv + Exp2_smooth_data.csv (within-experiment fallback)\n")
  cat("  Rows:", nrow(empirical_data),
      "| Groups:", paste(unique(empirical_data$group), collapse = ", "), "\n\n")

} else {

  cat("  No empirical smooth data found.\n")
  cat("  Run the GAMM scripts first to generate the CSV files:\n")
  cat("    Exp1_GAMM.R  → Exp1_smooth_data.csv\n")
  cat("    Exp2_GAMM.R  → Exp2_smooth_data.csv\n")
  cat("    Combined_GAMM.R → Combined_smooth_data.csv  (preferred)\n")
  cat("  Then re-run simulation_final.R.\n\n")

}

if (!is.null(empirical_data)) {

  # Split by group
  emp_JP <- empirical_data[empirical_data$group == "JP", ]
  emp_EN <- empirical_data[empirical_data$group == "EN", ]
  has_JP <- nrow(emp_JP) > 0
  has_EN <- nrow(emp_EN) > 0

  # ── Scale reconciliation helper ────────────────────────────────────────────
  # Normalizes an empirical vector (px) to the simulation scale (activation).
  # norm_target = the simulation value to map the empirical peak onto.
  # If the empirical vector is all-zero or all-NA, returns zeros.
  norm_to_sim <- function(emp_vec, norm_target) {
    emp_max <- max(abs(emp_vec), na.rm = TRUE)
    if (!is.finite(emp_max) || emp_max == 0) return(rep(0, length(emp_vec)))
    emp_vec / emp_max * norm_target
  }

  # Maximum simulation CV competition effect across all delta values
  # (used as the common normalization ceiling for empirical CV curves)
  sim_max_CV <- max(results[["0"]]$cv_effect, na.rm = TRUE)
  sim_max_C  <- max(results[["0"]]$c_effect,  na.rm = TRUE)
  sim_max_asym <- max(results[["30"]]$asymmetry, na.rm = TRUE)

  time_axis <- seq_len(TIME_STEPS)   # 1:101

  # ── NORMALIZATION NOTE (appended to figure captions) ──────────────────────
  norm_note <- paste0(
    "Empirical lines (thin = EN monolinguals, thick = JP bilinguals) normalized ",
    "to simulation scale by mapping peak empirical value to peak predicted value. ",
    "Shape is preserved; only amplitude is rescaled. Shaded bands = \u00b11 SE."
  )

  # ===========================================================================
  # OVERLAY FIGURE 1  — CV vs C competition effects with empirical smooths
  # ===========================================================================
  cat("  Generating Figure1_with_empirical.png...\n")

  # Scale empirical curves to simulation activation scale
  jp_CV_norm <- if (has_JP) norm_to_sim(emp_JP$CV_effect, sim_max_CV) else NULL
  jp_C_norm  <- if (has_JP) norm_to_sim(emp_JP$C_effect,  sim_max_C)  else NULL
  en_CV_norm <- if (has_EN) norm_to_sim(emp_EN$CV_effect, sim_max_CV) else NULL
  en_C_norm  <- if (has_EN) norm_to_sim(emp_EN$C_effect,  sim_max_C)  else NULL

  jp_CV_se_norm <- if (has_JP)
    emp_JP$CV_se / max(abs(emp_JP$CV_effect), na.rm=TRUE) * sim_max_CV else NULL
  jp_C_se_norm  <- if (has_JP)
    emp_JP$C_se  / max(abs(emp_JP$C_effect),  na.rm=TRUE) * sim_max_C  else NULL
  en_CV_se_norm <- if (has_EN)
    emp_EN$CV_se / max(abs(emp_EN$CV_effect), na.rm=TRUE) * sim_max_CV else NULL
  en_C_se_norm  <- if (has_EN)
    emp_EN$C_se  / max(abs(emp_EN$C_effect),  na.rm=TRUE) * sim_max_C  else NULL

  # Re-build Fig 1 panels with empirical lines added
  make_fig1_panel_overlay <- function(delta, panel_title, title_color,
                                      cv_emp, c_emp, cv_emp_se, c_emp_se,
                                      emp_label, emp_col) {
    # Base prediction panel (same as make_fig1_panel())
    p <- make_fig1_panel(delta, panel_title, title_color)

    if (!is.null(cv_emp)) {
      emp_df <- data.frame(
        time      = time_axis,
        CV_emp    = cv_emp,
        CV_lo     = cv_emp - cv_emp_se,
        CV_hi     = cv_emp + cv_emp_se,
        C_emp     = c_emp,
        C_lo      = c_emp  - c_emp_se,
        C_hi      = c_emp  + c_emp_se
      )

      p <- p +
        # CV empirical ± 1 SE ribbon
        geom_ribbon(data = emp_df,
                    aes(x = time, ymin = CV_lo, ymax = CV_hi),
                    fill = COL_CV, alpha = 0.12, inherit.aes = FALSE) +
        # CV empirical line
        geom_line(data = emp_df,
                  aes(x = time, y = CV_emp),
                  color = COL_CV, linewidth = 0.7,
                  linetype = "solid", inherit.aes = FALSE) +
        # C empirical ± 1 SE ribbon
        geom_ribbon(data = emp_df,
                    aes(x = time, ymin = C_lo, ymax = C_hi),
                    fill = COL_C, alpha = 0.12, inherit.aes = FALSE) +
        # C empirical line
        geom_line(data = emp_df,
                  aes(x = time, y = C_emp),
                  color = COL_C, linewidth = 0.7,
                  linetype = "dashed", inherit.aes = FALSE) +
        # Legend tag in corner
        annotate("text",
                 x = 2, y = 0.48,
                 label = paste0("Thin lines = empirical\n(", emp_label, ")"),
                 color = emp_col, size = 2.9, hjust = 0, fontface = "italic")
    }
    p
  }

  # Left panel (δ=0): EN monolingual overlay
  p1_left_ov <- make_fig1_panel_overlay(
    delta      = 0,
    panel_title = "\u03b4 = 0  \u2192  Language-Specific Grain Size Hypothesis\n(No moraic bottleneck \u2014 C onset = CV onset)",
    title_color = "#2ca02c",
    cv_emp     = en_CV_norm,
    c_emp      = en_C_norm,
    cv_emp_se  = en_CV_se_norm,
    c_emp_se   = en_C_se_norm,
    emp_label  = "EN monolinguals",
    emp_col    = "grey40"
  )

  # Right panel (δ=20): JP bilingual overlay
  p1_right_ov <- make_fig1_panel_overlay(
    delta      = 20,
    panel_title = "\u03b4 = 20  \u2192  L1-Transfer Hypothesis\n(Moraic bottleneck \u2014 C onset delayed relative to CV)",
    title_color = "#d62728",
    cv_emp     = jp_CV_norm,
    c_emp      = jp_C_norm,
    cv_emp_se  = jp_CV_se_norm,
    c_emp_se   = jp_C_se_norm,
    emp_label  = "JP bilinguals",
    emp_col    = "black"
  )

  fig1_ov <- (p1_left_ov | p1_right_ov) +
    plot_annotation(
      title    = "Figure 1 (with Empirical Overlay): Predicted and Empirical CV and C Competition Effects",
      subtitle = paste0(
        "Item-Level Phoneme Feature Simulation (N = 42 stimulus items, 95% CI across items)\n",
        "Thin lines = empirical GAMM smooths | Thick lines = simulation predictions\n",
        "LEFT: EN monolingual empirical overlay | RIGHT: JP bilingual empirical overlay"
      ),
      caption  = paste0(norm_note, "\n",
                        "TRACE-style activation architecture (McClelland & Elman, 1986) | ",
                        "CMU Pronouncing Dictionary transcriptions"),
      theme = theme(
        plot.title    = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9.5, color = "gray40"),
        plot.caption  = element_text(size = 8,   color = "gray55")
      )
    )

  ggsave("Figure1_with_empirical.png", fig1_ov,
         width = 14, height = 6.5, dpi = 300)
  cat("    Saved: Figure1_with_empirical.png\n")

  # ===========================================================================
  # OVERLAY FIGURE 2  — CV−C asymmetry gradient with JP empirical line
  # ===========================================================================
  cat("  Generating Figure2_with_empirical.png...\n")

  jp_asym_norm  <- if (has_JP) norm_to_sim(emp_JP$asymmetry, sim_max_asym) else NULL
  jp_asym_se_n  <- if (has_JP)
    emp_JP$asym_se / max(abs(emp_JP$asymmetry), na.rm=TRUE) * sim_max_asym else NULL
  en_asym_norm  <- if (has_EN) norm_to_sim(emp_EN$asymmetry, sim_max_asym) else NULL
  en_asym_se_n  <- if (has_EN)
    emp_EN$asym_se / max(abs(emp_EN$asymmetry), na.rm=TRUE) * sim_max_asym else NULL

  # Build fig2_ov from asym_df directly — DO NOT inherit from fig2.
  # fig2 contains the "ADD YOUR GAMM RESULT HERE" instruction annotation box,
  # which must not appear in the overlay figure (the result IS already there).
  # We reproduce every layer of fig2 except that annotation box.
  fig2_ov <- ggplot(asym_df, aes(x = time, y = asymmetry,
                                  color = delta, fill = delta,
                                  linetype = delta)) +
    geom_hline(yintercept = 0, color = "gray60", linewidth = 0.8,
               linetype = "dotted") +
    geom_ribbon(aes(ymin = asym_lo, ymax = asym_hi),
                alpha = 0.10, color = NA) +
    geom_line(linewidth = 1.3) +
    # NOTE: instruction annotation box intentionally omitted here —
    #       empirical data replaces the placeholder instruction.
    scale_color_manual(values = DELTA_COLS, labels = delta_labels, name = NULL) +
    scale_fill_manual( values = DELTA_COLS, labels = delta_labels, name = NULL) +
    scale_linetype_manual(
      values = c("0" = "solid", "10" = "longdash",
                 "20" = "dashed", "30" = "dotted"),
      labels = delta_labels, name = NULL) +
    scale_x_continuous(breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%")) +
    scale_y_continuous(limits = c(-0.02, 0.50),
                       breaks = seq(0, 0.5, 0.1)) +
    theme_bw(base_size = 12) +
    theme(
      legend.position   = "right",
      legend.text       = element_text(size = 9.5),
      legend.key.width  = unit(1.8, "cm"),
      plot.title        = element_text(size = 13, face = "bold"),
      plot.subtitle     = element_text(size = 9.5, color = "gray40"),
      plot.caption      = element_text(size = 8.2, color = "gray55"),
      panel.grid.minor  = element_blank()
    )

  if (has_EN && !is.null(en_asym_norm)) {
    en_asym_df <- data.frame(time = time_axis,
                              asym = en_asym_norm,
                              asym_lo = en_asym_norm - en_asym_se_n,
                              asym_hi = en_asym_norm + en_asym_se_n)
    fig2_ov <- fig2_ov +
      geom_ribbon(data = en_asym_df,
                  aes(x = time, ymin = asym_lo, ymax = asym_hi),
                  fill = "grey50", alpha = 0.18, inherit.aes = FALSE) +
      geom_line(data  = en_asym_df,
                aes(x = time, y = asym),
                color = "grey50", linewidth = 0.8,
                linetype = "dashed", inherit.aes = FALSE)
  }

  if (has_JP && !is.null(jp_asym_norm)) {
    jp_asym_df <- data.frame(time = time_axis,
                              asym = jp_asym_norm,
                              asym_lo = jp_asym_norm - jp_asym_se_n,
                              asym_hi = jp_asym_norm + jp_asym_se_n)
    fig2_ov <- fig2_ov +
      # ± 1 SE band for JP asymmetry
      geom_ribbon(data = jp_asym_df,
                  aes(x = time, ymin = asym_lo, ymax = asym_hi),
                  fill = "black", alpha = 0.15, inherit.aes = FALSE) +
      # The critical thick black line — JP empirical CV−C asymmetry
      geom_line(data  = jp_asym_df,
                aes(x = time, y = asym),
                color = "black", linewidth = 2.0, inherit.aes = FALSE) +
      # Label the empirical line directly
      annotate("text",
               x = time_axis[which.max(jp_asym_norm)] + 3,
               y = max(jp_asym_norm, na.rm = TRUE) + 0.02,
               label = "Empirical:\nJP bilinguals",
               color = "black", size = 3.2, hjust = 0, fontface = "bold")
  }

  fig2_ov <- fig2_ov +
    labs(
      title    = "Figure 2 (with Empirical Overlay): CV\u2212C Asymmetry \u2014 Predicted and Empirical",
      subtitle = paste0(
        "Item-Level Phoneme Feature Simulation (N = 42 items, 95% CI across items)\n",
        "Y-axis = CV competition effect \u2212 C competition effect at each normalized time step\n",
        "Thick black line = empirical JP bilingual CV\u2212C asymmetry | ",
        "Dashed grey = empirical EN monolingual"
      ),
      x        = "Normalized Time (% of trial)",
      y        = "CV Competition Effect \u2212 C Competition Effect",
      caption  = paste0(norm_note, "\n",
                        "Thick black line = JP bilingual empirical asymmetry | ",
                        "Dashed grey = EN monolingual | ",
                        "Colored curves = simulation predictions\n",
                        "INTERPRETATION: The colored \u03b4 curve the black line most closely tracks = ",
                        "estimated moraic bottleneck strength.")
    )

  ggsave("Figure2_with_empirical.png", fig2_ov,
         width = 12, height = 6.5, dpi = 300)
  cat("    Saved: Figure2_with_empirical.png\n")

  # ===========================================================================
  # OVERLAY FIGURE 3  — C onset timing with empirical horizontal reference lines
  # ===========================================================================
  cat("  Generating Figure3_with_empirical.png...\n")

  # Compute derivative-based C onset as a RAW STEP NUMBER (1–101).
  # Figure 3's y-axis uses raw step values labeled as "N%" (step 15 → "15%"),
  # so geom_hline(yintercept = onset_step) places the line at the correct
  # y position. We keep a separate human-readable label_pct string for the
  # annotate() call only.
  # BUG NOTE: an earlier version divided by 101*100 here, which shifted the
  # yintercept ~1% and corrupted the delta interpolation. Fixed below.
  jp_C_onset_step <- if (has_JP) {
    os <- min(which(emp_JP$C_deriv_sig), na.rm = TRUE)   # prefer derivative onset
    if (!is.finite(os)) os <- min(which(emp_JP$C_sig), na.rm = TRUE)  # fallback naive
    if (!is.finite(os)) NA_integer_ else as.integer(os)
  } else NULL

  en_C_onset_step <- if (has_EN) {
    os <- min(which(emp_EN$C_deriv_sig), na.rm = TRUE)
    if (!is.finite(os)) os <- min(which(emp_EN$C_sig), na.rm = TRUE)
    if (!is.finite(os)) NA_integer_ else as.integer(os)
  } else NULL

  # Build fig3_ov from onset_df directly — DO NOT inherit from fig3.
  # fig3 contains the "ADD YOUR GAMM RESULT" instruction annotation box,
  # which must not appear in the overlay figure (the reference lines replace it).
  fig3_ov <- ggplot(onset_df, aes(x = delta)) +
    # Onset delay shading
    geom_ribbon(data = onset_df %>% filter(!is.na(c_onset) & !is.na(cv_onset)),
                aes(ymin = cv_onset, ymax = c_onset),
                fill = "gray80", alpha = 0.6) +
    # CV reference line (constant — mora-aligned, never delayed)
    geom_line(aes(y = cv_onset),  color = COL_CV, linewidth = 1.4,
              linetype = "dashed") +
    geom_point(aes(y = cv_onset), color = COL_CV, size = 5, shape = 15) +
    # C onset line (rises with delta)
    geom_line(aes(y = c_onset),   color = COL_C,  linewidth = 1.8) +
    geom_point(aes(y = c_onset),  color = COL_C,  size = 6) +
    # Delay labels
    geom_text(data = delay_label_df,
              aes(x = delta + 1.0, y = c_onset + 2.5,
                  label = paste(label_pct, label_ms)),
              color = COL_C, size = 3.8, fontface = "bold", hjust = 0) +
    # CV annotation
    annotate("text", x = 0.5, y = onset_df$cv_onset[1] - 2.5,
             label = "CV onset \u2014 constant\n(mora-aligned, never delayed)",
             color = COL_CV, size = 3.5, hjust = 0) +
    # NOTE: "ADD YOUR GAMM RESULT" instruction box omitted — replaced by empirical lines.
    scale_x_continuous(breaks = DELTA_VALUES,
                       labels = paste0("\u03b4 = ", DELTA_VALUES)) +
    scale_y_continuous(breaks = seq(0, 55, 5),
                       labels = paste0(seq(0, 55, 5), "%"),
                       limits = c(0, 52)) +
    theme_bw(base_size = 12) +
    theme(
      plot.title       = element_text(size = 13, face = "bold"),
      plot.subtitle    = element_text(size = 9.5, color = "gray40"),
      plot.caption     = element_text(size = 8.2, color = "gray55"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(size = 11, face = "bold")
    )

  # Add EN reference line (dotted grey)
  if (has_EN && !is.null(en_C_onset_step) && !is.na(en_C_onset_step)) {
    fig3_ov <- fig3_ov +
      geom_hline(yintercept = en_C_onset_step,        # raw step = correct y-value
                 color = "grey50", linewidth = 1.0, linetype = "dotted") +
      annotate("text",
               x = 31, y = en_C_onset_step + 1.5,
               label = paste0("EN C onset: step ", en_C_onset_step,
                              " (", en_C_onset_step, "%)"),
               color = "grey40", size = 3.2, hjust = 0)
  }

  # Add JP reference line (dashed red) + estimate delta
  if (has_JP && !is.null(jp_C_onset_step) && !is.na(jp_C_onset_step)) {
    fig3_ov <- fig3_ov +
      geom_hline(yintercept = jp_C_onset_step,        # raw step = correct y-value
                 color = COL_C, linewidth = 1.2, linetype = "dashed") +
      annotate("text",
               x = 31, y = jp_C_onset_step + 1.5,
               label = paste0("JP C onset: step ", jp_C_onset_step,
                              " (", jp_C_onset_step, "%) \u2014 derivative-based"),
               color = COL_C, size = 3.4, hjust = 0, fontface = "bold")

    # Estimate delta: find which simulated C onset (raw step) is closest to the
    # empirical JP C onset step. sim_onsets uses the same units (raw steps from
    # find_onset), so the comparison is now apples-to-apples.
    sim_onsets <- sapply(DELTA_VALUES, function(d) {
      oc <- find_onset(results[[as.character(d)]]$c_effect)
      if (is.na(oc)) NA_real_ else as.numeric(oc)
    })
    delta_diff    <- abs(sim_onsets - jp_C_onset_step)
    nearest_delta <- DELTA_VALUES[which.min(delta_diff)]
    nearest_step  <- sim_onsets[which.min(delta_diff)]

    cat("\n  JP C ONSET ESTIMATION:\n")
    cat("    Empirical JP C onset:      step", jp_C_onset_step,
        "(=", jp_C_onset_step, "%% of 101 steps)\n")
    cat("    Closest simulated delta:   \u03b4 =", nearest_delta,
        "(predicted onset = step", nearest_step, ")\n")
    cat("    To convert to ms: onset_delay_steps \u00d7 mean_trial_ms / 101\n\n")

    fig3_ov <- fig3_ov +
      labs(
        title    = "Figure 3 (with Empirical Overlay): C Onset Timing \u2014 Predicted and Empirical",
        subtitle = paste0(
          "Item-Level Phoneme Feature Simulation (N = 42 stimulus items)\n",
          "Red circles = simulated C onset | Blue squares = CV onset (reference) | ",
          "Dashed red = empirical JP C onset | Dotted grey = empirical EN C onset\n",
          "Estimated moraic constraint: \u03b4 \u2248 ", nearest_delta
        ),
        x       = "Moraic Constraint Parameter (\u03b4)",
        y       = "Competition Onset (% of normalized trial)",
        caption = paste0(
          "Dashed red line = JP bilingual empirical C onset (derivative-based, step ",
          jp_C_onset_step, ") | ",
          "Dotted grey = EN monolingual C onset",
          if (!is.null(en_C_onset_step) && !is.na(en_C_onset_step))
            paste0(" (step ", en_C_onset_step, ")") else "",
          "\n",
          "Estimated moraic constraint: \u03b4 \u2248 ", nearest_delta,
          " | Convert to ms: JP_onset_step \u00d7 mean_trial_ms / 101\n",
          "* ms labels on figure assume 1000 ms trial. ",
          "Adjust: onset_delay_steps \u00d7 your_mean_trial_ms / 101"
        )
      )

    # Bug 4 fix: use real if/else instead of embedding condition text in cat()
    cat("  HYPOTHESIS RESULT:\n")
    if (nearest_delta == 0) {
      cat("    \u03b4 \u2248 0  \u2192  Language-Specific Grain Size Hypothesis supported.\n")
      cat("    JP C onset matches the no-bottleneck prediction.\n\n")
    } else {
      cat("    \u03b4 \u2248", nearest_delta,
          " \u2192  L1-Transfer Hypothesis supported.\n")
      cat("    Moraic bottleneck estimate \u2248 ", nearest_delta,
          "\u00d7 [mean trial duration ms] / 101  ms\n\n")
    }

  } else {
    fig3_ov <- fig3_ov +
      labs(
        title    = "Figure 3 (with Empirical Overlay): C Onset Timing \u2014 Predicted and Empirical",
        subtitle = paste0(
          "Item-Level Phoneme Feature Simulation (N = 42 stimulus items)\n",
          "No JP empirical C onset detected in smooth data. ",
          "Check C_deriv_sig and C_sig columns."
        ),
        x       = "Moraic Constraint Parameter (\u03b4)",
        y       = "Competition Onset (% of normalized trial)"
      )
  }

  ggsave("Figure3_with_empirical.png", fig3_ov,
         width = 11, height = 6.5, dpi = 300)
  cat("    Saved: Figure3_with_empirical.png\n")

  # ===========================================================================
  # OVERLAY SUMMARY
  # ===========================================================================
  cat("\n=============================================================\n")
  cat("  OVERLAY COMPLETE — 3 figures generated\n")
  cat("=============================================================\n\n")
  cat("  Figure1_with_empirical.png  \u2014 MAIN TEXT (Model Predictions + Empirical)\n")
  cat("    EN smooths on left panel (delta=0), JP smooths on right (delta=20)\n\n")
  cat("  Figure2_with_empirical.png  \u2014 MAIN TEXT (Primary Converging Evidence)\n")
  cat("    Thick black line = JP CV-C asymmetry. Match to closest delta curve.\n\n")
  cat("  Figure3_with_empirical.png  \u2014 MAIN TEXT (Onset Comparison)\n")
  cat("    JP C onset reference line. Read delta from x-axis intersection.\n\n")

  if (has_JP && !is.null(jp_C_onset_step) && !is.na(jp_C_onset_step)) {
    cat("  QUICK RESULT SUMMARY:\n")
    cat("    JP C onset (derivative-based): step", jp_C_onset_step,
        "(", jp_C_onset_step, "%% of 101 steps)\n")
    cat("    Most similar simulated delta:  \u03b4 =", nearest_delta, "\n")
    if (nearest_delta == 0) {
      cat("    Result: \u03b4 = 0  \u2192  Language-Specific Grain Size Hypothesis supported.\n\n")
    } else {
      cat("    Result: \u03b4 =", nearest_delta,
          " \u2192  L1-Transfer Hypothesis supported.\n")
      cat("    Bottleneck estimate \u2248 ", nearest_delta,
          "\u00d7 [mean trial ms] / 101  ms\n\n")
    }
  }

}  # end if (!is.null(empirical_data))
