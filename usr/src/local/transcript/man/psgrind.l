.\"	@(#)psgrind.l	1.1 %G%
.TH PSGRIND 1
.ds PS P\s-2OST\s+2S\s-2CRIPT\s+2
.SH NAME
psgrind \- convert C source files to P\s-2OST\s+2S\s-2CRIPT\s+2 format for printing
.SH SYNOPSIS
.B psgrind
[ 
.I options
] [
.I files
]
.SH DESCRIPTION
.I psgrind
reads in C source files, converts them to \*(PS format, and spools them for 
printing on a \*(PS printer.  The different fonts used for the different
syntactic elements of C source code may be specified on the command line.
.I Psgrind
is also much faster than any other grind program since it outputs a
direct printer language without going through a normal document formatter
as other pretty source printers must.
.sp
The five syntactic categories that
.I psgrind
recognizes and displays in different ways are page headings (which are
actually not part of the C source code), the ``body'' or main text,
keywords, which are C reserved words, literal strings and comments.
Normally, the heading and keywords are set in a bold Helvetica font,
the comments in a slanted Helvetica font, the body in a normal Helvetica
font and literal strings and character constants in Courier font.
.sp
All the above fonts may be changed, see the
.BR \-H ,
.BR \-B ,
.BR \-K ,
.BR \-L
and
.B -C
options.
All these options take as their argument a font specfication.
These font specifications have two parts:
A font name as known to \*(PS (e.g., Times-Roman, Times-BoldItalic,
Helvetica, Courier), and a 
point size (1 point = 1/72 inch\-8 point is a good small font).
So ``Courier-Bold8'' is 8 point Courier Bold, ``Helvetica12'' is 12 point 
Helvetica.
.sp
The environment variable PSGRIND may be used to specify defaults.  The 
value of PSGRIND is parsed as a string of arguments 
.I before 
the arguments 
that appear on the command line.  For example setting the
environment variable PSGRIND to the string ``-B Times-Roman12''
sets your default body font to 12 point Times Roman.
.sp
The possible options are:
.TP
.B \-2
Sets two column mode.
.TP
.B \-1
Force one column mode.
.TP
.B \-g
Causes the fact that a file is garbage to be ignored.  Normally, any file
with more than a small number of non-printing characters is suspected of
being garbage, and not printed\-this option means ``print it anyway.''
.TP
.B \-o
If
.I psgrind
cannot find characters in a font, the missing characters are listed.
.TP
.BI \-p " file"
Causes the \*(PS file to be written to the named file rather than
being spooled for printing.  As a special case, 
.B \-p
.B \-
will send the \*(PS to the standard output.
.TP
.B \-r
Causes the output to be rotated 90 degrees on the page (landscape mode).
This is good for output that requires a wide page or for program listings
when used in conjunction with two column mode.
``psgrind -2r
.IR files ''
is a nice way to get program listings.
.TP
.B \-R
Force portrait (non-rotated) mode.
.TP
.B \-k
Enable page prefeed (if the printer supports it).  This allows
simple documents (e.g., program listings in one font)
to print somewhat faster by keeping the printer
running between pages.  
.TP
.BI \-s " pagespec"
Allows you to specify a range (or several ranges) of pages to be printed.
.I Pagespec
is a string, not containing spaces, of the form ``pagerange[,pagespec]''.
.I Pagerange
may be a single page number or a range 
of the form ``N-M'' which means print pages N through M.
-N means print from the beginning of the docment to page N.  
M- means print from page M to the end of the document.
.TP
.BI \-H " font"
This allows the respecification of the font used to print running
titles listing the source file name and current page number.
The option argument to
.BR \-H ,
and the other font options,
should be a font specification as described above.
.TP
.BI \-B " font"
This allows one to respecify the font to be used for the main body
of the source, that not set specially in another font.
.TP
.BI \-K " font"
This option allows the respecification of the font used to show
keywords as separate from normal body text.
.TP
.BI \-L " font"
This option allows the respecification of the font used to show
string and character constants (literals) separate from normal body text.
.TP
.BI \-C " font"
This option respecifies the font used to print comment text to
show its separateness from normal text.
.PP
The following options are passed on as is to 
.I lpr.
.TP
.BI \-P printer
Causes the output to be sent to the named printer.
.TP
.BI \-# n
Causes n copies of the output to be produced.  The default is one.
.TP
.B \-m
Causes the printer daemon to send mail upon job completion.
.TP
.B \-h
Suppress the printing of the burst page.
.TP
.BI \-J " name"
Set the job name for use on the burst page.  Otherwise, the name of the
first input file will be used.
.SH ENVIRONMENT
.PP
.IP PSGRIND
string of options to be used by
.I psgrind.
.IP METRICS
path name of a directory to use instead of
/usr/lib/ps for font metric files.
.IP PRINTER
the name of a printer (as in the 
.B \-P
option) for 
.I lpr
to use.  If no 
.B \-P 
option is specified, 
.I lpr
will use this printer.  If 
.B \-P 
is not specified and 
PRINTER is not defined in the environment,
.I psgrind
will spool to a printer named ``PostScript''.
.SH FILES
.TP 2.2i
/usr/local/lib/ps/*.afm
font metrics files.
.TP
/usr/local/lib/ps/psgrind.pro
prologue for
.I psgrind
\*(PS files.
.TP
lpr
Printer spooler
.br
.SH "SEE ALSO"
enscript(1), lpr(1)
.SH AUTHOR
John Coker,
.br
University of California, Berkeley
.br
This code is based on the
.I enscript
utility from Adobe Systems, Inc.
.fi
.SH BUGS
If you give the 
.B \-p 
argument after the file to be converted,
.I psgrind
will lie and tell you that the file has been put in the right
place, when in fact it is in some obscure temporary file.  Always give
the 
.B \-p 
argument first.
.sp
Long lines are not truncated in 2 column mode.  Line truncation may be 
off by a little bit as printer margins vary.  There should be a 
``wrap'' option and multiple (truncated or wrapped) columns.
.sp
.I Psgrind
generates temporary files which are spooled for printing.  The temporary file
names are used by the spooling software (e.g., 
.IR lpq ),
rather than the symbolic job name passed to 
.IR lpr .
.SH NOTES
\*(PS is a trademark of Adobe Systems, Incorporated.
.br
Times is a trademark of Allied Corporation.
.br
Helvetica is a registered trademark of Allied Corporation.
