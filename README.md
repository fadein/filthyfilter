# FilthyFilter

Given an subtitles file in SRT format, output an EDL file for use with mplayer.
The EDL file causes mplayer to mute the audio for lines of dialog containing
profanity.

http://www.mplayerhq.hu/DOCS/HTML/en/edl.html


## Usage:

```
 -c, --cusses[=WORDS]     regex of cusswords to filter out
 -h, --help               show help text
 -f, --file=FILE          name of subtitle file to process
 -e, --encrypt=REGEXP     encrypt plaintext cusses so source isn't offensive
 -d, --dump               print encrypted cusses in plaintext. OFFENSIVE
 -v, --version            show program version (0.2)
```
