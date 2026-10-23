# =============================================================================
# phonActivR: Built-in Example Data
# =============================================================================

#' Built-in 42-Item English Stimulus Set (Lin et al., 2023)
#'
#' The English stimulus set of Lin, Lin, and Yeh (2023, Experiments 3 and
#' 4). Contains 42 target words, each paired with a CV competitor (sharing
#' onset consonant + vowel with the target), a C competitor (sharing only
#' the onset consonant), and a matched control for each competitor (a CV
#' control shares the target's vowel but not its onset; a C control shares
#' none of the target's initial segments). The CV/C contrast makes the set usable for any prosodic-constraint
#' hypothesis in which the initial CV is the complete unit (e.g., the mora
#' for Japanese listeners, the syllable body for Mandarin listeners).
#' \code{example_stimuli()} and \code{example_stimuli_jp()} are identical;
#' the second name is kept for backward compatibility (it reflects the
#' Japanese application for which the function was first written). Real
#' mouse-tracking data for these items are bundled as
#' \code{\link{lin2023_english}}.
#'
#' The stimulus materials are from Lin et al. (2023, Experiments 3 and 4),
#' who designed and validated the items for mouse-tracking studies of
#' phonological competition. Competitors and controls were matched on
#' number of letters, morphemes, phonemes, syllables, log frequency,
#' familiarity, prevalence, concreteness, age of acquisition, and
#' orthographic/phonological neighborhood density (see Lin et al., 2023: Appendix C
#' for the complete stimulus list; the matching statistics are reported in
#' the Experiment 3 Method).
#' Onset transcriptions follow the CMU Pronouncing Dictionary / TRACE
#' notation.
#'
#' @return A \code{"phonActivR_stimuli"} object ready for
#'   \code{\link{run_simulation}}.
#' @references
#' Lin, Y.-C., Lin, P.-Y., & Yeh, L.-H. (2023). Syllable or phoneme? A
#' mouse-tracking investigation of phonological units in Mandarin Chinese
#' and English spoken word recognition. \emph{Journal of Experimental
#' Psychology: Learning, Memory, and Cognition}, 49(1), 130--176.
#' @export
#' @aliases example_stimuli
#' @examples
#' stim <- example_stimuli()
#' sim <- run_simulation(stim)
#' summary(sim)
example_stimuli_jp <- function() {

  targets <- c("bench","bitter","bottle","budget","budge","desk","depth","dance",
               "fabric","fancy","ferry","fish","finger","gang","handle","heavy",
               "jacket","lever","letter","liquid","listen","lock","lunch","match",
               "magnet","middle","mint","monster","pants","pitch","pillow","rapid",
               "risk","rich","seven","single","sick","test","text","taxi","tick","wish")

  CV_comp <- c("bell","bill","box","bump","bust","dead","deck","dad","fan","fat",
               "fence","fit","fix","gap","hack","hell","jam","leg","lend","lip",
               "link","lot","luck","mad","map","mill","miss","mob","pack","pink",
               "pick","rack","rip","ring","set","silk","sip","tell","tend","task",
               "till","wit")

  CV_ctrl <- c("cell","hill","fox","jump","lust","head","peck","bad","ban","mat",
               "hence","hit","mix","lap","lack","yell","ham","beg","bend","hip",
               "wink","pot","duck","pad","rap","pill","kiss","rob","tack","sink",
               "kick","sack","dip","sing","let","milk","tip","fell","send","mask",
               "fill","bit")

  C_comp <- c("bark","bank","bat","boom","boat","dust","dock","deal","feed","fold",
              "fire","fake","folk","gold","heal","hide","joy","law","lint","loss",
              "lone","lag","list","mop","mean","mark","mall","math","pea","pool",
              "poke","roll","rain","rub","sit","save","sand","tall","toy","tool",
              "toss","wax")

  C_ctrl <- c("dark","tank","rat","zoom","coat","rust","rock","seal","seed","bold",
              "hire","lake","yolk","mold","meal","ride","boy","saw","hint","moss",
              "tone","tag","mist","top","bean","park","fall","path","tea","cool",
              "joke","poll","pain","pub","kit","cave","band","wall","soy","fool",
              "boss","tax")

  onsets <- list(
    # Targets
    "bench"=c("b","eh"),"bitter"=c("b","ih"),"bottle"=c("b","ao"),
    "budget"=c("b","ah"),"budge"=c("b","ah"),"desk"=c("d","eh"),
    "depth"=c("d","eh"),"dance"=c("d","ae"),"fabric"=c("f","ae"),
    "fancy"=c("f","ae"),"ferry"=c("f","eh"),"fish"=c("f","ih"),
    "finger"=c("f","ih"),"gang"=c("g","ae"),"handle"=c("hh","ae"),
    "heavy"=c("hh","eh"),"jacket"=c("jh","ae"),"lever"=c("l","eh"),
    "letter"=c("l","eh"),"liquid"=c("l","ih"),"listen"=c("l","ih"),
    "lock"=c("l","ao"),"lunch"=c("l","ah"),"match"=c("m","ae"),
    "magnet"=c("m","ae"),"middle"=c("m","ih"),"mint"=c("m","ih"),
    "monster"=c("m","ao"),"pants"=c("p","ae"),"pitch"=c("p","ih"),
    "pillow"=c("p","ih"),"rapid"=c("r","ae"),"risk"=c("r","ih"),
    "rich"=c("r","ih"),"seven"=c("s","eh"),"single"=c("s","ih"),
    "sick"=c("s","ih"),"test"=c("t","eh"),"text"=c("t","eh"),
    "taxi"=c("t","ae"),"tick"=c("t","ih"),"wish"=c("w","ih"),
    # CV competitors
    "bell"=c("b","eh"),"bill"=c("b","ih"),"box"=c("b","ao"),
    "bump"=c("b","ah"),"bust"=c("b","ah"),"dead"=c("d","eh"),
    "deck"=c("d","eh"),"dad"=c("d","ae"),"fan"=c("f","ae"),
    "fat"=c("f","ae"),"fence"=c("f","eh"),"fit"=c("f","ih"),
    "fix"=c("f","ih"),"gap"=c("g","ae"),"hack"=c("hh","ae"),
    "hell"=c("hh","eh"),"jam"=c("jh","ae"),"leg"=c("l","eh"),
    "lend"=c("l","eh"),"lip"=c("l","ih"),"link"=c("l","ih"),
    "lot"=c("l","ao"),"luck"=c("l","ah"),"mad"=c("m","ae"),
    "map"=c("m","ae"),"mill"=c("m","ih"),"miss"=c("m","ih"),
    "mob"=c("m","ao"),"pack"=c("p","ae"),"pink"=c("p","ih"),
    "pick"=c("p","ih"),"rack"=c("r","ae"),"rip"=c("r","ih"),
    "ring"=c("r","ih"),"set"=c("s","eh"),"silk"=c("s","ih"),
    "sip"=c("s","ih"),"tell"=c("t","eh"),"tend"=c("t","eh"),
    "task"=c("t","ae"),"till"=c("t","ih"),"wit"=c("w","ih"),
    # CV controls
    "cell"=c("s","eh"),"hill"=c("hh","ih"),"fox"=c("f","ao"),
    "jump"=c("jh","ah"),"lust"=c("l","ah"),"head"=c("hh","eh"),
    "peck"=c("p","eh"),"bad"=c("b","ae"),"ban"=c("b","ae"),
    "mat"=c("m","ae"),"hence"=c("hh","eh"),"hit"=c("hh","ih"),
    "mix"=c("m","ih"),"lap"=c("l","ae"),"lack"=c("l","ae"),
    "yell"=c("y","eh"),"ham"=c("hh","ae"),"beg"=c("b","eh"),
    "bend"=c("b","eh"),"hip"=c("hh","ih"),"wink"=c("w","ih"),
    "pot"=c("p","ao"),"duck"=c("d","ah"),"pad"=c("p","ae"),
    "rap"=c("r","ae"),"pill"=c("p","ih"),"kiss"=c("k","ih"),
    "rob"=c("r","ao"),"tack"=c("t","ae"),"sink"=c("s","ih"),
    "kick"=c("k","ih"),"sack"=c("s","ae"),"dip"=c("d","ih"),
    "sing"=c("s","ih"),"let"=c("l","eh"),"milk"=c("m","ih"),
    "tip"=c("t","ih"),"fell"=c("f","eh"),"send"=c("s","eh"),
    "mask"=c("m","ae"),"fill"=c("f","ih"),"bit"=c("b","ih"),
    # C competitors
    "bark"=c("b"),"bank"=c("b"),"bat"=c("b"),"boom"=c("b"),
    "boat"=c("b"),"dust"=c("d"),"dock"=c("d"),"deal"=c("d"),
    "feed"=c("f"),"fold"=c("f"),"fire"=c("f"),"fake"=c("f"),
    "folk"=c("f"),"gold"=c("g"),"heal"=c("hh"),"hide"=c("hh"),
    "joy"=c("jh"),"law"=c("l"),"lint"=c("l"),"loss"=c("l"),
    "lone"=c("l"),"lag"=c("l"),"list"=c("l"),"mop"=c("m"),
    "mean"=c("m"),"mark"=c("m"),"mall"=c("m"),"math"=c("m"),
    "pea"=c("p"),"pool"=c("p"),"poke"=c("p"),"roll"=c("r"),
    "rain"=c("r"),"rub"=c("r"),"sit"=c("s"),"save"=c("s"),
    "sand"=c("s"),"tall"=c("t"),"toy"=c("t"),"tool"=c("t"),
    "toss"=c("t"),"wax"=c("w"),
    # C controls
    "dark"=c("d"),"tank"=c("t"),"rat"=c("r"),"zoom"=c("z"),
    "coat"=c("k"),"rust"=c("r"),"rock"=c("r"),"seal"=c("s"),
    "seed"=c("s"),"bold"=c("b"),"hire"=c("hh"),"lake"=c("l"),
    "yolk"=c("y"),"mold"=c("m"),"meal"=c("m"),"ride"=c("r"),
    "boy"=c("b"),"saw"=c("s"),"hint"=c("hh"),"moss"=c("m"),
    "tone"=c("t"),"tag"=c("t"),"mist"=c("m"),"top"=c("t"),
    "bean"=c("b"),"park"=c("p"),"fall"=c("f"),"path"=c("p"),
    "tea"=c("t"),"cool"=c("k"),"joke"=c("jh"),"poll"=c("p"),
    "pain"=c("p"),"pub"=c("p"),"kit"=c("k"),"cave"=c("k"),
    "band"=c("b"),"wall"=c("w"),"soy"=c("s"),"fool"=c("f"),
    "boss"=c("b"),"tax"=c("t")
  )

  create_stimuli(
    targets    = targets,
    large_comp = CV_comp,
    large_ctrl = CV_ctrl,
    small_comp = C_comp,
    small_ctrl = C_ctrl,
    onsets     = onsets,
    large_type = "CV",
    small_type = "C"
  )
}


#' Mouse-Tracking Competition Effects From Lin, Lin, and Yeh (2023)
#'
#' Group-mean competition-effect curves from Experiments 3 and 4 of Lin, Lin,
#' and Yeh (2023), which used the same 42 English stimulus items that
#' \code{\link{example_stimuli_jp}} provides. Experiment 3 tested
#' Chinese--English bilinguals (n = 41) and Experiment 4 English
#' monolinguals (n = 49) in a mouse-tracking spoken word recognition task.
#' The dataset is used in the tutorial's real-data application: because the
#' stimuli are identical, the simulation and the data refer to exactly the
#' same items.
#'
#' Effects are differences in time-normalized horizontal mouse position
#' (control minus competitor x-coordinate; correct trials only), so positive
#' values mean the trajectory was attracted toward the competitor. The
#' normalized time axis runs from trial onset to response and therefore
#' includes pre-response and motor latency; estimate a response latency when
#' fitting (the \code{lag} argument of \code{\link{goodness_of_fit}}).
#' Only group means are included here; de-identified participant- and
#' item-level means are in \code{inst/extdata/lin2023_validation/}.
#'
#' @format A data frame with 202 rows and 6 variables:
#' \describe{
#'   \item{time}{Integer. Normalized time step (1--101).}
#'   \item{cv_effect}{Numeric. CV (larger-grain) competition effect.}
#'   \item{c_effect}{Numeric. C (smaller-grain) competition effect.}
#'   \item{asymmetry}{Numeric. \code{cv_effect - c_effect}.}
#'   \item{group}{Character. "Chinese-English bilinguals" (Experiment 3) or
#'     "English monolinguals" (Experiment 4).}
#'   \item{n_subjects}{Integer. Number of participants in the group.}
#' }
#' @source Lin, Y.-C., Lin, P.-Y., & Yeh, L.-H. (2023). Syllable or phoneme?
#'   A mouse-tracking investigation of phonological units in Mandarin Chinese
#'   and English spoken word recognition. \emph{Journal of Experimental
#'   Psychology: Learning, Memory, and Cognition, 49}(1), 130--176.
#'   \doi{10.1037/xlm0001128}. Build script: \code{data-raw/make_lin2023_english.R}.
#' @usage data(lin2023_english)
#' @examples
#' data(lin2023_english)
#' sim <- run_simulation(example_stimuli_jp(), delta_values = c(0, 10, 20, 30),
#'                       verbose = FALSE)
#' gof <- goodness_of_fit(sim, lin2023_english, lag = 0:50)
"lin2023_english"


#' @rdname example_stimuli_jp
#' @export
example_stimuli <- function() example_stimuli_jp()


#' Built-in 24-Item Mandarin Stimulus Set (Lin et al., 2023, Experiment 1)
#'
#' The Mandarin monosyllabic stimulus set of Lin, Lin, and Yeh (2023,
#' Experiment 1), in which Chinese--English bilinguals heard Mandarin words.
#' Each of the 24 targets is paired with a CV competitor (sharing the onset
#' consonant and the vowel, e.g., \emph{ba1}--\emph{ban1}), a C competitor
#' (sharing only the onset consonant, e.g., \emph{ba1}--\emph{bin1}), and one
#' control word that shares the target's vowel (often its whole rime) but
#' not its onset (e.g., \emph{pan1}). Unlike the English set
#' (\code{\link{example_stimuli}}), in which the C control shares nothing
#' with the target, the same vowel-sharing control serves both competitor
#' types here, so it is entered as both \code{large_ctrl} and
#' \code{small_ctrl}; the C competition effect is therefore the attraction
#' of an onset-sharing word minus that of a vowel-sharing word. In the
#' experiment, each target was tested with one competitor type (12 targets
#' with a CV competitor, 12 with a C competitor) and with its control; the
#' group-mean curves are bundled as \code{\link{lin2023_chinese}}.
#'
#' Transcriptions use Pinyin initials and the first vowel letter of the
#' final as phoneme codes, written in upper case so that they cannot be
#' confused with the ARPAbet codes of the built-in English feature matrix
#' (e.g., \emph{ban1} = \code{c("B", "A")}; \emph{zhen3} = \code{c("ZH", "E")}).
#' These codes are not covered by \code{\link{trace_features}}, so a
#' similarity table must be supplied; \code{\link{mandarin_similarity}}
#' provides one. Tone is not represented: within each item, the target and
#' its competitors share the tone. Coding the first vowel letter of the
#' final as the vowel is a simplification (e.g., \emph{zhe}--\emph{zhen},
#' \emph{ya}--\emph{yan} differ in vowel quality). Item codes are those of
#' Lin et al. (2023).
#'
#' @return A \code{"phonActivR_stimuli"} object with 24 items.
#' @references
#' Lin, Y.-C., Lin, P.-Y., & Yeh, L.-H. (2023). Syllable or phoneme? A
#' mouse-tracking investigation of phonological units in Mandarin Chinese
#' and English spoken word recognition. \emph{Journal of Experimental
#' Psychology: Learning, Memory, and Cognition}, 49(1), 130--176.
#' @seealso \code{\link{mandarin_similarity}}, \code{\link{lin2023_chinese}}
#' @export
#' @examples
#' stim_zh <- example_stimuli_zh()
#' sim_zh <- run_simulation(stim_zh, delta_values = c(0, 10, 20),
#'                          similarity_matrix = mandarin_similarity(),
#'                          gate = "truncate", verbose = FALSE)
example_stimuli_zh <- function() {
  tab <- utils::read.csv(system.file("extdata", "stimuli_24items_chinese.csv",
                                     package = "phonActivR"),
                         stringsAsFactors = FALSE)
  code <- function(p) {
    p <- sub("[0-9]$", "", p)
    ini <- regmatches(p, regexpr("^(zh|ch|sh|[bpmfdtnlgkhjqxrzcsyw])", p))
    if (length(ini) == 0) ini <- ""
    rest <- sub(paste0("^", ini), "", p)
    c(toupper(ini), toupper(substr(rest, 1, 1)))
  }
  onsets <- list()
  for (w in unique(c(tab$target, tab$cv_competitor, tab$control))) onsets[[w]] <- code(w)
  # The C competitor shares only its onset consonant with the target.
  c_lab <- paste0(tab$c_competitor, "_C")
  for (i in seq_len(nrow(tab))) onsets[[c_lab[i]]] <- code(tab$c_competitor[i])[1]
  create_stimuli(targets    = tab$target,
                 large_comp = tab$cv_competitor, large_ctrl = tab$control,
                 small_comp = c_lab,             small_ctrl = tab$control,
                 onsets = onsets, large_type = "CV", small_type = "C")
}


#' Illustrative Similarity Table for the Mandarin Stimulus Set
#'
#' Pairwise similarities for every phoneme pair that the comparisons in
#' \code{\link{example_stimuli_zh}} generate, in the form accepted by
#' \code{\link{custom_similarity}}. The rule is deliberately simple:
#' identical codes have similarity 1; initials that differ only in
#' aspiration (B/P, D/T, G/K, Z/C, ZH/CH, J/Q) have similarity
#' \code{aspiration}; every other pair has similarity \code{other}. The
#' values are illustrative, not measured; researchers should replace them
#' with values from perceptual confusion data when such data are available.
#' Because every competitor in the set shares its onset exactly with its
#' target, these values affect only the controls' input; the fits in the
#' tutorial do not change when \code{aspiration} and \code{other} are
#' varied.
#'
#' @param aspiration Similarity of initials that differ only in aspiration
#'   (default 0.7).
#' @param other Similarity of all other non-identical pairs (default 0.3,
#'   the package's default similarity).
#' @return A similarity matrix from \code{\link{custom_similarity}}.
#' @export
#' @examples
#' sm <- mandarin_similarity()
#' check_phoneme_coverage(example_stimuli_zh(), similarity_matrix = sm)
mandarin_similarity <- function(aspiration = 0.7, other = 0.3) {
  stim <- example_stimuli_zh(); on <- stim$onsets; it <- stim$items
  pairs <- unique(do.call(rbind, lapply(seq_len(nrow(it)), function(i) {
    t <- on[[it$target[i]]]
    rbind(cbind(t[1], on[[it$large_comp[i]]][1]), cbind(t[2], on[[it$large_comp[i]]][2]),
          cbind(t[1], on[[it$large_ctrl[i]]][1]), cbind(t[2], on[[it$large_ctrl[i]]][2]),
          cbind(t[1], on[[it$small_comp[i]]][1]))
  })))
  asp <- c(B = "P", D = "T", G = "K", Z = "C", ZH = "CH", J = "Q")
  val <- function(a, b) {
    if (a == b) return(1)
    if ((!is.na(asp[a]) && asp[a] == b) || (!is.na(asp[b]) && asp[b] == a)) return(aspiration)
    other
  }
  custom_similarity(data.frame(p1 = pairs[, 1], p2 = pairs[, 2],
                               similarity = mapply(val, pairs[, 1], pairs[, 2]),
                               stringsAsFactors = FALSE))
}


#' Mandarin-Listening Competition Effects From Lin, Lin, and Yeh (2023)
#'
#' Group-mean competition-effect curves from Experiment 1 of Lin, Lin, and
#' Yeh (2023), in which Chinese--English bilinguals (n = 41) heard the 24
#' Mandarin monosyllables of \code{\link{example_stimuli_zh}} in the same
#' mouse-tracking task as the English experiments
#' (\code{\link{lin2023_english}}). Each target was tested with one
#' competitor type (12 targets with a CV competitor, 12 with a C competitor)
#' and with its control, so \code{cv_effect} is computed from the CV targets
#' and \code{c_effect} from the C targets, each as the control-trial minus
#' competitor-trial x-coordinate (correct trials only, trial-weighted means
#' at 101 normalized time steps). Positive values mean attraction toward
#' the competitor. Participant-level means are in
#' \code{inst/extdata/lin2023_validation/}.
#'
#' @format A data frame with 101 rows and 6 variables, as
#'   \code{\link{lin2023_english}} (\code{group} = "Chinese-English
#'   bilinguals (Mandarin)"; \code{n_subjects} = 41).
#' @source Lin, Y.-C., Lin, P.-Y., & Yeh, L.-H. (2023). Syllable or phoneme?
#'   A mouse-tracking investigation of phonological units in Mandarin Chinese
#'   and English spoken word recognition. \emph{Journal of Experimental
#'   Psychology: Learning, Memory, and Cognition, 49}(1), 130--176.
#'   \doi{10.1037/xlm0001128}. Build script: \code{data-raw/make_lin2023_chinese.R}.
#' @usage data(lin2023_chinese)
#' @examples
#' data(lin2023_chinese)
#' max(lin2023_chinese$c_effect) / max(lin2023_chinese$cv_effect)
"lin2023_chinese"
