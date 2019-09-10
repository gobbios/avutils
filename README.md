# avutils

This package is an attempt to provide an R interface to the [DiViMe tools](https://divime.readthedocs.io/en/latest/index.html). In addition, it provides some utilities dealing with audio and video (based on `ffmpeg` and `sox`) and dealing with ELAN .eaf files. 

I wrote and tested this package on a MacOS system. Testing in Windows was very limitied up to now and I suspect there might be some glitches specific to how file paths are handled in Windows.

Current functionality:

  * get info about audio and video files (duration, size, bit rate etc) and some basic file manipulations (extracting audio from video, split audio files)

  * `read_elan` extracts annotations and time stamps, separated by tiers, from ELAN `.eaf` files
  
  * set-up a virtual machine to run the DiViMe tools
  
  * run specific DiViMe tools, currently:
  
    - SAD noisemse, SAD opensmile, SAD tocombo
    
    - diartk
    
    - yunitator
    
    - vcm

Requirements:

  * for audio and video processing: `sox` binary (https://sourceforge.net/projects/sox/) and `ffmpeg` binary (https://ffmpeg.org/download.html). These don't need to be installed, but you should make sure that you know the location of the files on your system.
  
  * for DiViMe tools: `vagrant` software installed (e.g. https://releases.hashicorp.com/vagrant/) and `VirtualBox` (https://www.virtualbox.org/wiki/Download_Old_Builds_5_2)
  
  * if you have a Windows machine, you'll also need `git`

To install the package, use:

`library(devtools)`

`install_github("gobbios/avutils")`

[link to](doc/audiooperations.html)


# FAQ



