# avutils

Some utilities dealing with audio and video (based on `ffmpeg` and `sox`) and dealing with ELAN .eaf files. In addition, it's an attempt to provide an R interface to the [DiViMe tools](https://divime.readthedocs.io/en/latest/index.html).

I wrote this package on a MacOS system and I don't know whether it will work in Windows (but suspect it should work with Linux).

Current functionality:

  * get info about audio and video files and some basic file manipulations (extracting audio from video, split audio files)

  * `read_elan`: extract annotations and time stamps, separated by tiers, from ELAN `.eaf` files
  
  * set-up a virtual machine to run the DiViMe tools
  
  * run specific DiViMe tools, currently:
  
    - SAD noisemse
    
    - yunitator

Requirements:

  * for audio and video processing: `sox` binary (https://sourceforge.net/projects/sox/) and `ffmpeg` binary (https://ffmpeg.org/download.html). These don't need to be installed, but you should make sure that you know the location of the files on your system.
  
  * for DiViMe tools: `vagrant` software installed (e.g. https://releases.hashicorp.com/vagrant/)

To install the package, use:

`library(devtools)`

`install_github("gobbios/avutils")`

[link to](doc/audiooperations.html)
