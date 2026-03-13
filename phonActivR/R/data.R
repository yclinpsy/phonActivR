# =============================================================================
# phonActivR: Built-in Example Data
# =============================================================================

#' Example Japanese-English Bilingual Stimulus Set (42 Items)
#'
#' A complete stimulus set for testing moraic processing constraints in
#' Japanese-English bilingual spoken word recognition. Contains 42 target
#' words, each paired with a CV competitor (sharing onset consonant + vowel
#' with the target), a C competitor (sharing only the onset consonant),
#' and matched phonologically unrelated controls for each.
#'
#' The stimulus materials are from Lin et al. (2023, Experiments 3 and 4),
#' who designed and validated the items for mouse-tracking studies of
#' phonological competition. Competitors and controls were matched on
#' number of letters, morphemes, phonemes, syllables, log frequency,
#' familiarity, prevalence, concreteness, age of acquisition, and
#' orthographic/phonological neighborhood density (see Lin et al., 2023,
#' Appendix C for the complete stimulus list and lexical statistics).
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
#' @examples
#' stim <- example_stimuli_jp()
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
