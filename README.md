# avutils
Some utilities dealing with audio and video (based on `ffmpeg` and `sox`) and dealing with ELAN .eaf files

I wrote this package on a MacOS system and I don't know whether it will work in Windows (but suspect it should work with Linux).

Current functionality:

  * `extract_audio`: extract and save audio stream from video (and convert to 44.1kHz mono .wav)
  
  * `split_audio`: split audio files into chunks of regular duration
  
  * `audio_info`: obtain info of audio file
  
  * `read_elan`: extract annotations and time stamps, separated by tiers, from ELAN .eaf files

Requirements:

  * `sox` binary from https://sourceforge.net/projects/sox/
  
  * `ffmpeg` binary from https://ffmpeg.org/download.html 
  
These don't need to be installed, but you should make sure that you know the location of the files on your system.

To install the package, use:

`library(devtools)`

`install_github("gobbios/avutils")`

