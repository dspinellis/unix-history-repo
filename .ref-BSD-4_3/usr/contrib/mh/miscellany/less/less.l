.TH LESS l
.SH NAME
less \- opposite of more
.SH SYNOPSIS
.B "less [-cdepstwmMqQuU] [-h\fIn\fB] [-b[fp]\fIn\fB] [-x\fIn\fB] [+\fIcmd\fB] [\fIname\fB] ..."
.SH DESCRIPTION
.I Less
is a program similar to 
.I more
(1), but which allows backwards movement
in the file as well as forward movement.
Also,
.I less
does not have to read the entire input file before starting,
so with large input files it starts up faster than text editors like
.I vi
(1).
.I Less
uses termcap, so it can run on a variety of terminals.
There is even limited support for hardcopy terminals.
(On a hardcopy terminal, lines which should be printed at the top
of the screen are prefixed with an up-arrow.)
.PP
Commands are based on both
.I more
and
.I vi.
Commands may be preceeded by a decimal number, 
called N in the descriptions below.
The number is used by some commands, as indicated.

.SH COMMANDS
.IP h
Help: display a summary of these commands.
If you forget all the other commands, remember this one.
.PP
.IP SPACE
Scroll forward N lines, default one screen.
If N is more than the screen size, only one screenful is displayed.
.PP
.IP f
Same as SPACE.
.PP
.IP b
Scroll backward N lines, default one screen.
If N is more than the screen size, only one screenful is displayed.
.PP
.IP RETURN
Scroll forward N lines, default 1.
If N is more than the screen size, the entire N lines are displayed.
.PP
.IP e
Same as RETURN.
.PP
.IP j
Also the same as RETURN.
.PP
.IP y
Scroll backward N lines, default 1.
If N is more than the screen size, the entire N lines are displayed.
.IP k
Same as y.
.PP
.IP d
Scroll forward N lines, default 10.
If N is specified, it becomes the new default for all d and u commands.
.PP
.IP u
Scroll backward N lines, default 10.
If N is specified, it becomes the new default for all d and u commands.
.PP
.IP r
Repaint the screen.
.PP
.IP R
Repaint the screen, discarding any buffered input.
Useful if the file is changing while it is being viewed.
.PP
.IP g
Go to line N in the file, default 1 (beginning of file).
(Warning: this may be slow if N is large.)
.PP
.IP G
Go to line N in the file, default the end of the file.
(Warning: this may be slow if standard input, 
rather than a file, is being read.)
.PP
.IP p
Go to a position N percent into the file.
N should be between 0 and 100.
(This is possible if standard input is being read,
but only if
.I less
has already read to the end of the file.
It is always fast, but not always useful.)
.PP
.IP %
Same as p.
.PP
.IP m
Followed by any lowercase letter, marks the current position with that letter.
.PP
.IP "'"
Followed by any lowercase letter, returns to the position which
was previously marked with that letter.
All marks are lost when a new file is examined.
.PP
.IP /pattern
Search forward in the file for the N-th occurence of the pattern.
N defaults to 1.
The pattern is a regular expression, as recognized by
.I ed.
The search starts at the second line displayed
(but see the -t option, which changes this).
.PP
.IP ?pattern
Search backward in the file for the N-th occurence of the pattern.
The search starts at the line immediately before the top line displayed.
.PP
.IP n
Repeat previous search, for N-th occurence of the last pattern.
.PP
.IP E [filename]
Examine a new file.
If the filename is missing, the "current" file (see the N and P commands
below) from the list of files in the command line is re-examined.
.PP
.IP N
Examine the next file (from the list of files given in the command line).
If a number N is specified (not to be confused with the command N),
the N-th next file is examined.
.PP
.IP P
Examine the previous file.
If a number N is specified, the N-th previous file is examined.
.PP
.IP =
Prints the name of the file being viewed
and the byte offset of the bottom line being displayed.
If possible, it also prints the length of the file
and the percent of the file above the last displayed line.
.PP
.IP \-
Followed by one of the command line option letters (see below),
this will toggle the setting of that option
and print a message describing the new setting.
.PP
.IP V
Prints the version number of 
.I less 
being run.
.PP
.IP q
Exits
.I less.
.PP
The following 
two 
commands may or may not be valid, depending on your particular installation.
.PP
.IP v
Invokes an editor to edit the current file being viewed.
The editor is taken from the environment variable EDITOR,
or defaults to "vi".
.PP
.IP "! shell-command"
Invokes a shell to run the shell-command given.
.PP
.SH OPTIONS
Command line options are described below.
Options are also taken from the environment variable "LESS".
(The environment variable is parsed before the command line,
so command line options override the LESS environment variable.
Options may be changed while
.I less 
is running via the "\-" command.)
For example, if you like 
more-style prompting, to avoid typing "less -m ..." each time 
.I less 
is invoked, you might tell 
.I csh:
.sp
setenv LESS m
.sp
or if you use 
.I sh:
.sp
LESS=m; export LESS
.IP -s
The -s flag causes
consecutive blank lines to be squeezed into a single blank line.
This is useful when viewing
.I nroff
output.
.IP -t
Normally, forward searches start just after
the top displayed line (that is, at the second displayed line).
Thus forward searches include the currently displayed screen.
The -t command line option causes forward searches to start 
just after the bottom line displayed,
thus skipping the currently displayed screen.
.IP -m
Normally,
.I less
prompts with a colon.
The -m command line option causes 
.I less
to prompt verbosely like 
.I more,
printing the file name and percent into the file.
.IP -M
The -M command line option causes 
.I less
to prompt even more verbosely than 
.I more.
.IP -q
Normally, if an attempt is made to scroll past the end of the file
or before the beginning of the file, the terminal bell is rung to
indicate this fact.
The -q command line option tells
.I less
not to ring the bell at such times.
If the terminal has a "visual bell", it is used instead.
.IP -Q
Even if -q is given, 
.I less 
will ring the bell on certain other errors,
such as typing an invalid character.
The -Q command line option tells
.I less
to be quiet all the time; that is, never ring the terminal bell.
If the terminal has a "visual bell", it is used instead.
.IP -e
Normally the only way to exit less is via the "q" command.
The -e command line option tells less to automatically exit
the second time it reaches end-of-file.
.IP -u
If the -u command line option is given, 
backspaces are treated as printable characters;
that is, they are sent to the terminal when they appear in the input.
.IP -U
If the -U command line option is given,
backspaces are printed as the two character sequence "^H".
If neither -u nor -U is given,
backspaces which appear adjacent to an underscore character
are treated specially:
the underlined text is displayed 
using the terminal's hardware underlining capability.
.IP -w
Normally,
.I less
uses a tilde character to represent lines past the end of the file.
The -w option causes blank lines to be used instead.
.IP -d
Normally,
.I less
will complain if the terminal is dumb; that is, lacks some important capability,
such as the ability to clear the screen or scroll backwards.
The -d flag suppresses this complaint 
(but does not otherwise change the behavior of the program on a dumb terminal).
.IP -p
Normally, 
.I less 
will repaint the screen by scrolling from the bottom of the screen.
If the -p flag is set, when
.I less 
needs to change the entire display, it will clear the screen
and paint from the top line down.
.IP -h
Normally,
.I less
will scroll backwards when backwards movement is necessary.
The -h option specifies a maximum number of lines to scroll backwards.
If it is necessary to move backwards more than this many lines,
the screen is repainted in a forward direction.
(If the terminal does not have the ability to scroll
backwards, -h0 is implied.)
.IP -x
The -x\fIn\fR command line option sets tab stops every \fIn\fR positions.
The default for \fIn\fR is 8.
.IP -b
The -b\fIn\fR command line option tells
.I less
to use a non-standard buffer size.
There are two standard (default) buffer sizes,
one is used when a file is being read and the other
when a pipe (standard input) is being read.
The current defaults are 5 buffers for files and 12 for pipes.
(Buffers are 1024 bytes.)
The number \fIn\fR specifies a different number of buffers to use.
The -b may be followed by "f", in which case only 
the file default is changed, or by "p" in which case only the 
pipe default is changed.  Otherwise, both are changed.
.IP -c
Normally, when data is read by
.I less,
it is scanned to ensure that bit 7 (the high order bit) is turned off in
each byte read, and to ensure that there are no null (zero) bytes in
the data (null bytes are turned into "@" characters).
If the data is known to be "clean",
the -c command line option will tell 
.I less
to skip this checking, causing an imperceptible speed improvement.
(However, if the data is not "clean", unpredicatable results may occur.)
.IP +
If a command line option begins with \fB+\fR,
the remainder of that option is taken to be an initial command to
.I less.
For example, +G tells
.I less
to start at the end of the file rather than the beginning,
and +/xyz tells it to start at the first occurence of "xyz" in the file.
As a special case, +<number> acts like +<number>g; 
that is, it starts the display at the specified line number
(however, see the caveat under the "g" command above).
If the option starts with \fB++\fR, the initial command applies to
every file being viewed, not just the first one.

.SH BUGS
When used on standard input (rather than a file), you can move
backwards only a finite amount, corresponding to that portion
of the file which is still buffered.
