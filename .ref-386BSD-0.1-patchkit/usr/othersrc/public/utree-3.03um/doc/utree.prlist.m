'''\"
'''\"   utree.prlist.man
'''\"   klin, Fri Mar  6 11:21:41 1992
'''\"
'''\"   [x]roff -man utree.prlist.man
'''\"   groff   -man [-Tdevice] utree.prlist.man
'''\"
'''\"   SCCSID = @(#) utree.prlist..man 3.03-um Mar  6 1992
'''\"
.TH UTREE.PRLIST 1L "UTREE Version 3.03-um" "March 6 1992"
.SH NAME
.LP
\fButree.prlist\fR \- Filter \fButree\fR tree list files
.SH SYNOPSIS
.LP
utree.prlist [options] listfile
.SH DESCRIPTION
.LP
\fButree.prlist\fR reads a
formatted directory tree list file \fBlistfile\fR
previously created from within \fButree\fR
on the \fItree screen\fR with
the out-command (\fBo\fR) and the list-option (\fBl\fR).
The tree list can be converted to various output formats
for different output devices and printers (see below).
The converted tree list is written to stdout.
.SH OPTIONS
.LP
The following command line options are interpreted by \fButree.prlist\fR:
.sp
.TP 16
\fB-T dev\fR
Create output for device \fBdev\fR.
Currently supported devices see below.
.TP
\fB-V\fR
Display program version and exit.
.TP
\fB-d dev\fR
Create output for device \fBdev\fR.
Currently supported devices see below.
.TP
\fB-f fnt\fR
Use font \fBfnt\fR.
This option is meaningful only for postscript (default: Courier-Bold).
.TP
\fB-h\fR
Display some help about usage and options.
.TP
\fB-i ind\fR
Set tree indention to \fBind\fR columns (3 .. 9, default: 6).
.TP
\fB-s siz\fR
Use font size \fBsiz\fR.
This option is meaningful only for postscript (default: 10).
.SH OUTPUT FORMATS
.LP
Currently supported output formats or devices are:
.sp
.TP 16
\fBascii\fR
ASCII using graphical meta characters (-|+).
.TP
\fB850\fR
Printers supporting the IBM international character set PC850.
.TP
\fBps\fR
ADOBE POSTSCRIPT printers or previewers.
.TP
\fBterm\fR
Terminals using graphical characters if defined (default).
.PP
.SH SEE ALSO
.LP
utree(1L)
.SH AUTHOR
.LP
Peter Klingebiel
.SH COPYRIGHT
.LP
\(co 1991/92 Peter Klingebiel & UNIX Magazin Munich
.sp
Permission is granted to copy and distribute \fButree\fR in modified
or unmodified form, for noncommercial use, provided (a) this copyright
notice is preserved, (b) no attempt is made to restrict redistribution
of this file, and (c) this file is not distributed as part of any
collection whose redistribution is restricted by a compilation copyright.
