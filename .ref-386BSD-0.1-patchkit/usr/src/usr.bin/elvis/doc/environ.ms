.Go 11 "ENVIRONMENT VARIABLES"
.PP
\*E examines several environment variables when it starts up.
The values of these variables are used internally for a variety
of purposes.
You don't need to define all of these;
on most systems, \*E only requires TERM to be defined.
On AmigaDOS, MS-DOS or TOS systems, even that is optional.
.SH
TERM, TERMCAP
.PP
TERM tells \*E the name of the termcap entry to use.
TERMCAP may contain either the entire termcap entry,
or the full pathname of the termcap file to search through.
.PP
If your version of \*E is using tinytcap instead of the full termcap library,
then the value of TERMCAP \fIcannot\fR contain any backslash escapes (\\E, \\r, etc.)
or carat escapes (^[, ^M, etc.), because tinytcap doesn't understand them.
Instead, you should embed the actual control character into the string.
.SH
TMP, TEMP
.PP
These only work for AmigaDOS, MS-DOS and Atari TOS.
Either of these variables may be used to set the "directory" option,
which controls where temporary files are stored.
If you define them both, then TMP is used, and TEMP is ignored.
.SH
LINES, COLUMNS
.PP
The termcap entry for your terminal should specify the size of your screen.
If you're using a windowing interface, then there is an ioctl() call which
will provide the size of the window; the ioctl() values will override the
values in the termcap entry.
The LINES and COLUMNS environment variables (if defined)
will override either of these sources.
They, in turn, can be overridden by a ":set" command.
.PP
Normally, the LINES and COLUMNS variables shouldn't need to be defined.
.SH
EXINIT
.PP
This variable's value may contain one or more colon-mode commands,
which will be executed after all of the ".exrc" files
but before interactive editing begins.
.PP
To put more than one command in EXINIT, you can separate the commands
with either a newline or a '|' character.
.SH
SHELL, COMSPEC
.PP
You can use COMSPEC in MS-DOS, or SHELL in any other system,
to specify which shell should be used for executing commands and
expanding wildcards.
.SH
HOME
.PP
This variable should give the full pathname of your home directory.
\*E needs to know the name of your home directory so it can locate
the ".exrc" file there.
.SH
TAGPATH
.PP
This variable is used by the "ref" program.
It contains a list of directories that might contain a relevent "tags" file.
Under AmigaDOS, MS-DOS or Atari TOS, the names of the directories should be separated by
semicolons (";").
Under other operating systems, the names should be separated by colons (":").
.PP
If you don't define TAGPATH, then "ref" will use a default list which includes
the current directory and a few other likely places.
See the definition of DEFTAGPATH at the start of ref.c for an accurate list.
