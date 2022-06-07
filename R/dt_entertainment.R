# Entertainment ####
if( length(suppressWarnings(findWavPlayer())) == 0) setWavPlayer("C:/Users/Public/wav_play/wv_player.exe")

.entertainment <- list()
.entertainment$alarm <- load.wave(paste0(wd$entertain,"sound/alarm.wav"))
.entertainment$egal <- load.wave(paste0(wd$entertain,"sound/egal.wav"))
.entertainment$egal_image <- load.image(paste0(wd$entertain,"pics/egal.png"))
.entertainment$alarm_image <- load.image(paste0(wd$entertain,"pics/alarm.png"))

.entertainment$vfl <- load.wave(paste0(wd$entertain,"sound/11.wav"))
.entertainment$drum <- load.wave(paste0(wd$entertain,"sound/drum_roll.wav"))

.breakfun <- function(x)
{
  #p1 <- proc.time()
  Sys.sleep(x)
  #proc.time() - p1 # The cpu usage should be negligible
}
