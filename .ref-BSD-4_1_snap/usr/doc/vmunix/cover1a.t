.IP doc 25
Source for documentation about the system (volumes 2a, 2b and 2c
and other miscellaenous material.)
.IP games
Binaries for the games; put this in \fB/usr/games\fR.
.IP ingres
Source for the INGRES database system.  To install INGRES follow
the instructions in the READ_ME file in \fBsrc/cmd/ingres\fR.
.IP lib/learn
Data files for the \fIlearn\fR\|(1) command.
.IP lib/fontinfo
Data files needed by the raster typesetting software \fIvpr\fR\|(1), etc.
.IP lib/vfont
The bit mapped fonts for \fIvpr\fR\|(1).  The files *.r are rotated fonts
for 11 by 8 paper, and the others are unrotated.
.IP lib/vsamples
Sample files which produce the font catalog in volume 2c.
.IP src/cmd/berknet
Source for the Berkeley Network.  See the READ_ME file in its directory
and the supplied printed documentation.
.IP src/cmd/fed
A font editor for the bitmap fonts (see below).
.IP src/cmd/learn
Source for the \fIlearn\fR\|(1) command.
.IP src/cmd/oldcsh
Source for \fB/bin/oldcsh\fR, an older version of \fIcsh\fR\|(1).
.IP src/cmd/vpr
Source for routines to drive Varian and Versatec printer-plotters;
see \fIvpr\fR\|(1).
.IP src/games
Source for some of the games.
.LP
Desired materials can be extracted from the tape by ``tar x''.  Thus
if you wish to extract the learn materials:
.DS
# cd /usr
# tar x  src/cmd/learn  lib/learn
.DE
