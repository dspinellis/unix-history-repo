.hw de-limit
.hw de-limit-ing
.RP
....TM 78-1270-1 39199 39199-11
.ND August 15, 1978
.TL
SED \(em A Non-interactive Text Editor
.AU "MH 2C-555" 3302
Lee E. McMahon
.AI
.MH
.OK
Context search
Editing
.AB
.ul
Sed
is a non-interactive context editor
that runs on the
.UX
operating system.
.ul
Sed
is
designed to be especially useful in
three cases:
.in +1i
.LP
.ti -.5i
1)
To edit files too large for comfortable
interactive editing;
.ti -.5i
2)
To edit any size file when the sequence
of editing commands is too complicated to be comfortably
typed in interactive mode.
.ti -.5i
3)
To perform multiple `global' editing functions
efficiently in one pass through the input.
.in -1i
.LP
This memorandum constitutes a manual for users of
.ul
sed.
.AE
.CS 10 0 10 0 0 1
.SH
Introduction
.LP
.ul
Sed
is a non-interactive context editor designed to be especially useful in
three cases:
.in +1i
.LP
.ti -.5i
1)
To edit files too large for comfortable
interactive editing;
.ti -.5i
2)
To edit any size file when the sequence
of editing commands is too complicated to be comfortably
typed in interactive mode;
.ti -.5i
3)
To perform multiple `global' editing functions
efficiently in one pass through the input.
.in -1i
.LP
Since only a few lines of the input reside in core
at one time, and no temporary files are used,
the effective size of file that can be edited is limited only
by the requirement that the input and output fit simultaneously
into available secondary storage.
.LP
Complicated editing scripts can be created separately and given
to 
.ul
sed
as a command file.
For complex edits, this saves considerable typing, and its
attendant errors.
.ul
Sed
running from a command file is much more efficient than any interactive
editor known to the author, even if that editor
can be driven by a pre-written script.
.LP
The principal loss of functions compared to an interactive editor
are lack of relative addressing (because of the line-at-a-time
operation), and lack of immediate verification that a command has
done what was intended.
.LP
.ul
Sed
is a lineal descendant of the UNIX editor,
.ul
ed.
Because of the differences between interactive and non-interactive
operation, considerable changes have been made between
.ul
ed
and
.ul
sed;
even confirmed users of
.ul
ed
will frequently be surprised (and probably chagrined),
if they rashly use 
.ul
sed
without reading Sections 2 and 3 of this document.
The most striking family resemblance between the two
editors is in the class of patterns (`regular expressions') they
recognize;
the code for matching patterns is copied almost
verbatim from the code for
.ul
ed,
and the description of regular expressions in Section 2
is copied almost verbatim from the UNIX Programmer's
Manual[1]. (Both code and description were written by Dennis
M. Ritchie.)
.LP
.SH
1. Overall Operation
.LP
.ul
Sed
by default copies the standard input to the standard output,
perhaps performing one or more editing commands on each
line before writing it to the output.
This behavior may be modified by flags on the command line;
see Section 1.1 below.
.LP
The general format of an editing command is:
.LP
.in +1i
[address1,address2][function][arguments]
.LP
.in -1i
One or both addresses may be omitted; the format of addresses is
given in Section 2.
Any number of blanks or tabs may separate the addresses
from the function.
The function must be present; the available commands are discussed
in Section 3.
The arguments may be required or optional, according to which function
is given; again, they are discussed in Section 3 under each individual
function.
.LP
Tab characters and spaces at the beginning of lines are ignored.
.LP
.SH
1.1. Command-line Flags
.LP
Three flags are recognized on the command line:
.in +1i
.ti -.5i
.B
-n:
.R
tells
.ul
sed
not to copy all lines, but only those specified by
.ul
p
functions or
.ul
p
flags after 
.ul
s
functions (see Section 3.3);
.ti -.5i
.B
-e:
.R
tells
.ul
sed
to take the next argument as an editing command;
.ti -.5i
.B
-f:
.R
tells
.ul
sed
to take the next argument as a file name;
the file should contain editing commands, one to a line.
.in -1i
.SH
1.2. Order of Application of Editing Commands
.LP
Before any editing is done (in fact, before any input file is
even opened), all the editing commands are compiled into
a form which will be moderately efficient during
the execution phase (when the commands are actually applied to
lines of the input file).
The commands are compiled in the order in which they are
encountered; this is generally the order in which they will
be attempted at execution time.
The commands are applied one at a time; the input to each command
is the output of all preceding commands.
.LP
The default linear order of application of editing commands can
be changed by the flow-of-control commands,
.ul
t
and
.ul
b
(see Section 3).
Even when the order of application is changed
by these commands, it is still true that the input line to any
command is the output of any previously applied command.
.SH
1.3.  Pattern-space
.LP
The range of pattern matches is called the pattern space.
Ordinarily, the pattern space is one line of the input text,
but more than one line can be read into the pattern space
by using the
.ul
N
command (Section 3.6.).
.LP
.SH
1.4. Examples
.LP
Examples are scattered throughout the text.
Except where otherwise noted,
the examples all assume the following input text:
.LP
.in +.5i
.nf
In Xanadu did Kubla Khan
A stately pleasure dome decree:
Where Alph, the sacred river, ran
Through caverns measureless to man
Down to a sunless sea.
.in -.5i
.fi
.LP
(In no case is the output of the 
.ul
sed
commands to be considered an improvement
on Coleridge.)
.LP
.SH
Example:
.LP
The command
.in +.5i
.LP
2q
.in -.5i
.LP
will quit after copying the first two lines of the input.
The output will be:
.LP
.in +.5i
.nf
In Xanadu did Kubla Khan
A stately pleasure dome decree:
.in -.5i
.fi
.LP
.SH
2. ADDRESSES: Selecting lines for editing
.LP
Lines in the input file(s) to which editing commands are
to be applied can be selected by addresses.
Addresses may be either line numbers or context addresses.
.LP
The application of a group of commands can be controlled by
one address (or address-pair) by grouping
the commands with curly braces (`{ }')(Sec. 3.6.).
.SH
2.1. Line-number Addresses
.LP
A line number is a decimal integer.
As each line is read from the input, a line-number counter
is incremented;
a line-number address matches (selects) the input
line which causes the internal counter to equal the
address line-number.
The counter runs cumulatively through multiple input files;
it is not reset when a new input file is opened.
.LP
As a special case, the character
$
matches the last line of the last input file.
.SH
2.2. Context Addresses
.LP
A context address is a pattern (`regular expression') enclosed in slashes (`/').
The regular expressions recognized by
.ul
sed
are constructed as follows:
.in +1i
.LP
.ti -.5i
1)
An ordinary character (not one of those discussed below)
is a regular expression, and matches that character.
.LP
.ti -.5i
2)
A circumflex `^' at the beginning of a regular expression
matches the null character at the beginning of a line.
.ti -.5i
3)
A dollar-sign `$' at the end of a regular expression
matches the null character at the end of a line.
.ti -.5i
4)
The characters `\en' match an imbedded newline character,
but not the newline at the end of the pattern space.
.ti -.5i
5)
A period `.' matches any character except the terminal newline
of the pattern space.
.ti -.5i
6)
A regular expression followed by an asterisk `*' matches any
number (including 0) of adjacent occurrences of the regular
expression it follows.
.ti -.5i
7)
A string of characters in square brackets `[ ]' matches any character
in the string, and no others.
If, however, the first character of the string is circumflex `^',
the regular expression matches any character
.ul
except
the characters in the string and the terminal newline of the pattern space.
.ti -.5i
8)
A concatenation of regular expressions is a regular expression
which matches the concatenation of strings matched by the
components of the regular expression.
.ti -.5i
9)
A regular expression between the sequences `\e(' and `\e)' is
identical in effect to the unadorned regular expression, but has
side-effects which are described under the
.ul
s
command below and specification 10) immediately below.
.ti -.5i
10)
The expression
.ul
`\|\ed'
means the same string of characters matched
by an expression enclosed in `\e(' and `\e)'
earlier in the same pattern.
Here
.ul
d
is a single digit;
the string specified is that beginning with the
\fId\|\fRth
occurrence of `\e(' counting from the left.
For example, the expression
`^\e(.*\e)\e1' matches a line beginning with
two repeated occurrences of the same string.
.ti -.5i
11)
The null regular expression standing alone (e.g., `//') is
equivalent to the  last regular expression compiled.
.in -1i
.LP
To use one of the special characters (^ $ . * [ ] \e /) as a literal
(to match an occurrence of itself in the input), precede the
special character by a backslash `\e'.
.LP
For a context address to `match' the input requires that
the whole pattern within the address match some
portion of the pattern space.
.SH
2.3. Number of Addresses
.LP
The commands in the next section can have 0, 1, or 2 addresses.
Under each command the maximum number of allowed addresses is
given.
For a command to have more addresses than the maximum allowed
is considered an error.
.LP
If a command has no addresses, it is applied to every line
in the input.
.LP
If a command has one address, it is applied to all
lines which match that address.
.LP
If a command has two addresses, it is applied to the first
line which matches the first address, and to all subsequent lines
until (and including) the first subsequent line which matches
the second address.
Then an attempt is made on subsequent lines to again match the first
address, and the process is repeated.
.LP
Two addresses are separated by a comma.
.SH
Examples:
.LP
.nf
.in +.5i
.ta 1i 2i
/an/	matches lines 1, 3, 4 in our sample text
/an.*an/	matches line 1
/^an/	matches no lines
/./	matches all lines
/\e./	matches line 5
/r*an/	matches lines 1,3, 4 (number = zero!)
/\e(an\e).*\e1/	matches line 1
.fi
.in 0
.LP
.SH
3. FUNCTIONS
.LP
All functions are named by a single character.
In the following summary, the maximum number of allowable addresses
is given enclosed in parentheses, then the single character
function name, possible arguments enclosed in angles (< >),
an expanded English translation of the single-character name,
and finally a description of what each function does.
The angles around the arguments are
.ul
not
part of the argument, and should not be typed
in actual editing commands.
.SH
3.1. Whole-line Oriented Functions
.LP
.in +1i
.ti -.5i
(2)d -- delete lines
.if t .sp .5
The
.ul
d
function deletes from the file (does not write to the output)
all those lines matched by its address(es).
.if t .sp .5
It also has the side effect that no further commands are attempted
on the corpse of a deleted line;
as soon as the
.ul
d
function is executed, a new line is read from the input, and
the list of editing commands is re-started from the beginning
on the new line.
.ti -.5i
.if t .sp .5
(2)n -- next line
.if t .sp .5
The
.ul
n
function reads the next line from the input, replacing
the current line.
The current line is written to the output if it should
be.
The list of editing commands is continued 
following the 
.ul
n
command.
.if t .sp .5
.nf
.in -.5i
(1)a\e
<text> -- append lines
.in +.5i
.fi
.if t .sp .5
The
.ul
a
function causes the argument <text> to be written to the
output after the line matched by its address.
The
.ul
a
command is inherently multi-line;
.ul
a
must appear at the end of a line, and <text> may contain
any number of lines.
To preserve the one-command-to-a-line fiction,
the interior newlines must be hidden by a
backslash character (`\e') immediately preceding the
newline.
The <text> argument is terminated by the first unhidden
newline (the first one not immediately preceded
by backslash).
.if t .sp .5
Once an
.ul
a
function is successfully executed, <text> will be
written to the output regardless of what later commands do to
the line which triggered it.
The triggering line may be 
deleted entirely; <text> will still be written to the output.
.if t .sp .5
The <text> is not scanned for address matches, and no editing
commands are attempted on it.
It does not cause any change in the line-number counter.
.if t .sp .5
.nf
.in -.5i
(1)i\e
<text> -- insert lines
.in +.5i
.fi
.if t .sp .5
The
.ul
i
function  behaves identically to the
.ul
a
function, except that <text> is written to the output
.ul
before
the matched line.
All other comments about the
.ul
a
function apply to the
.ul
i
function as well.
.if t .sp .5
.nf
.in -.5i
(2)c\e
<text> -- change lines
.in +.5i
.fi
.if t .sp .5
The
.ul
c
function deletes the lines selected by its address(es),
and replaces them with the lines in <text>.
Like
.ul
a
and
.ul
i,
.ul
c
must be followed by a newline hidden by a backslash;
and interior new lines in <text> must be hidden by
backslashes.
.if t .sp .5
The
.ul
c
command may have two addresses, and therefore select a range
of lines.
If it does, all the lines in the range are deleted, but only
one copy of <text> is written to the output,
.ul
not
one copy per line deleted.
As with
.ul
a
and
.ul
i,
<text> is not scanned for address matches, and no
editing commands are attempted on it.
It does not change the  line-number counter.
.if t .sp .5
After a line has been deleted by a
.ul
c
function, no further commands are attempted on the corpse.
.if t .sp .5
If text is appended after a line by
.ul
a
or
.ul
r
functions, and the line is subsequently changed, the text
inserted by the
.ul
c
function will be placed
.ul
before
the text of the
.ul
a
or
.ul
r
functions.
(The
.ul
r
function is described in Section 3.4.)
.if t .sp .5
.in -1i
.ul
Note:
Within the text put in the output by these functions,
leading blanks and tabs will disappear, as always in 
.ul
sed
commands.
To get leading blanks and tabs into the output, precede the first
desired blank or tab by a backslash; the backslash will not
appear in the output.
.SH
Example:
.LP
The list of editing commands:
.LP
.in +.5i
.nf
n
a\e
XXXX
d
.in -.5i
.fi
.LP
applied to our standard input, produces:
.LP
.in +.5i
.nf
In Xanadu did Kubhla Khan
XXXX
Where Alph, the sacred river, ran
XXXX
Down to a sunless sea.
.in -.5i
.fi
.LP
In this particular case,
the same effect would be produced by either
of the two following command lists:
.LP
.in +.5i
.nf
n		n
i\e		c\e
XXXX	XXXX
d
.in -.5i
.fi
.LP
.in 0
.SH
3.2. Substitute Function
.LP
One very important function changes parts of lines selected by
a context search within the line.
.if t .sp .5
.in +1i
.ti -.5i
(2)s<pattern><replacement><flags> -- substitute
.if t .sp .5
The
.ul
s
function replaces
.ul
part
of a line (selected by <pattern>) with <replacement>.
It can best be read:
.if t .sp .5
.ti +1i
Substitute for <pattern>, <replacement>
.if t .sp .5
The <pattern> argument contains a pattern,
exactly like the patterns in addresses (see 2.2 above).
The only difference between <pattern> and a context address is
that the context address must be delimited by slash (`/') characters;
<pattern> may be delimited by any character other than space or
newline.
.if t .sp .5
By default, only the first string matched by <pattern> is replaced,
but see the
.ul
g
flag below.
.if t .sp .5
The
<replacement> argument begins immediately after the
second delimiting character of <pattern>, and must be followed
immediately by another instance of the delimiting character.
(Thus there are exactly 
.ul
three
instances of the delimiting character.)
.if t .sp .5
The <replacement> is not a pattern,
and the characters which are special in patterns
do not have special meaning in <replacement>.
Instead, other characters are special:
.if t .sp .5
.in +1i
.ti -.5i
&	is replaced by the string matched by <pattern>
.if t .sp .5
.ti -.5i
.ul
\ed
(where
.ul
d
is a single digit) is replaced by the \fId\fRth substring
matched by parts of <pattern> enclosed in `\e(' and `\e)'.
If nested substrings occur in <pattern>, the \fId\fRth
is determined by counting opening delimiters (`\e(').
.if t .sp .5
As in patterns, special characters may be made
literal by preceding them with backslash (`\e').
.if t .sp .5
.in -1i
The <flags> argument may contain the following flags:
.if t .sp .5
.in +1i
.ti -.5i
g -- substitute <replacement> for all (non-overlapping)
instances of <pattern> in the line.
After a successful substitution, the scan for the next
instance of <pattern> begins just after the end of the
inserted characters; characters put into the line from
<replacement> are not rescanned.
.if t .sp .5
.ti -.5i
p -- print the line if a successful replacement was done.
The
.ul
p
flag causes the line to be written to the output if and only
if a substitution was actually made by the
.ul
s
function.
Notice that if several
.ul
s
functions, each followed by a
.ul
p
flag, successfully substitute in the same input line,
multiple copies of the line will be written to the
output: one for each successful substitution.
.if t .sp .5
.ti -.5i
w <filename> -- write the line to a file if a successful
replacement was done.
The
.ul
w
flag causes lines which are actually substituted by the
.ul
s
function to be written to a file named by <filename>.
If <filename> exists before
.ul
sed
is run, it is overwritten;
if not, it is created.
.if t .sp .5
A single space must separate
.ul
w
and <filename>.
.if t .sp .5
The possibilities of multiple, somewhat different copies of
one input line being written are the same as for 
.ul
p.
.if t .sp .5
A maximum of 10 different file names may be mentioned after
.ul
w
flags and
.ul
w
functions (see below), combined.
.in 0
.SH
Examples:
.LP
The following command, applied to our standard input,
.LP
.in +.5i
s/to/by/w changes
.in -.5i
.LP
produces, on the standard output:
.LP
.in +.5i
.nf
In Xanadu did Kubhla Khan
A stately pleasure dome decree:
Where Alph, the sacred river, ran
Through caverns measureless by man
Down by a sunless sea.
.fi
.in -.5i
.LP
and, on the file `changes':
.LP
.in +.5i
.nf
Through caverns measureless by man
Down by a sunless sea.
.fi
.in -.5i
.LP
If the nocopy option is in effect, the command:
.LP
.in +.5i
.nf
s/[.,;?:]/*P&*/gp
.fi
.in -.5i
.LP
produces:
.LP
.in +.5i
.nf
A stately pleasure dome decree*P:*
Where Alph*P,* the sacred river*P,* ran
Down to a sunless sea*P.*
.LP
.in -.5i
.fi
Finally, to illustrate the effect of the
.ul
g
flag,
the command:
.LP
.in +.5i
.nf
/X/s/an/AN/p
.in -.5i
.fi
.LP
produces (assuming nocopy mode):
.in +.5i
.LP
.nf
In XANadu did Kubhla Khan
.fi
.in -.5i
.LP
and the command:
.LP
.in +.5i
.nf
/X/s/an/AN/gp
.in -.5i
.fi
.LP
produces:
.LP
.in +.5i
.nf
In XANadu did Kubhla KhAN
.in -.5i
.fi
.LP
.in 0
.SH
3.3. Input-output Functions
.LP
.in +1i
.ti -.5i
(2)p -- print
.if t .sp .5
The print function writes the addressed lines to the standard output file.
They are written at the time the
.ul
p
function is encountered, regardless of what succeeding
editing commands may do to the lines.
.if t .sp .5
.ti -.5i
(2)w <filename> -- write on <filename>
.if t .sp .5
The write function writes the addressed lines to the file named
by <filename>.
If the file previously existed, it is overwritten; if not, it is created.
The lines are written exactly as they exist when the write function
is encountered for each line, regardless of what subsequent
editing commands may do to them.
.if t .sp .5
Exactly one space must separate the
.ul
w
and <filename>.
.if t .sp .5
A maximum of ten different files may be mentioned in write
functions and
.ul
w
flags after
.ul
s
functions, combined.
.if t .sp .5
.ti -.5i
(1)r <filename> -- read the contents of a file
.if t .sp .5
The read function reads the contents of <filename>, and appends
them after the line matched by the address.
The file is read and appended regardless of what subsequent
editing commands do to the line which matched its address.
If
.ul
r
and
.ul
a
functions are executed on the same line,
the text from the 
.ul
a
functions and the
.ul
r
functions is written to the output in the order that
the functions are executed.
.if t .sp .5
Exactly one space must separate the
.ul
r
and <filename>.
If a file mentioned by a
.ul
r
function cannot be opened, it is considered a null file,
not an error, and no diagnostic is given.
.if t .sp .5
.in -1i
NOTE:
Since there is a limit to the number of files that can be opened
simultaneously, care should be taken that no more than ten
files be mentioned in
.ul
w
functions or flags; that number is reduced by one if any
.ul
r
functions are present.
(Only one read file is open at one time.)
.in 0
.SH
Examples
.LP
Assume that the file `note1'
has the following contents:
.LP
.in +1i
Note:  Kubla Khan (more properly Kublai Khan; 1216-1294)
was the grandson and most eminent successor of Genghiz
(Chingiz) Khan, and founder of the Mongol dynasty in China.
.LP
.in 0
Then the following command:
.LP
.nf
.in +.5i
/Kubla/r note1
.in -.5i
.fi
.LP
produces:
.LP
.nf
.in +.5i
In Xanadu did Kubla Khan
.in +.5i
.fi
Note:  Kubla Khan (more properly Kublai Khan; 1216-1294)
was the grandson and most eminent successor of Genghiz
(Chingiz) Khan, and founder of the Mongol dynasty in China.
.in -.5i
.nf
A stately pleasure dome decree:
Where Alph, the sacred river, ran
Through caverns measureless to man
Down to a sunless sea.
.in -.5i
.fi
.LP
.in 0
.SH
3.4.
Multiple Input-line Functions
.LP
Three functions, all spelled with capital letters, deal
specially with pattern spaces containing imbedded newlines;
they are intended principally to provide pattern matches across
lines in the input.
.if t .sp .5
.in +1i
.ti -.5i
(2)N -- Next line
.if t .sp .5
The next input line is appended to the current line in the
pattern space; the two input lines are separated by an imbedded
newline.
Pattern matches may extend across the imbedded newline(s).
.if t .sp .5
.ti -.5i
(2)D -- Delete first part of the pattern space
.if t .sp .5
Delete up to and including the first newline character
in the current pattern space.
If the pattern space becomes empty (the only newline
was the terminal newline),
read another line from the input.
In any case, begin the list of editing commands again
from its beginning.
.if t .sp .5
.ti -.5i
(2)P -- Print first part of the pattern space
.if t .sp .5
Print up to and including the first newline in the pattern space.
.if t .sp .5
.in 0
The 
.ul
P
and
.ul
D
functions are equivalent to their lower-case counterparts
if there are no imbedded newlines in the pattern space.
.in 0
.SH
3.5.  Hold and Get Functions
.LP
Four functions save and retrieve part of the input for possible later
use.
.if t .sp .5
.in 1i
.ti -.5i
(2)h -- hold pattern space
.if t .sp .5
The
.ul
h
functions copies the contents of the pattern space
into a hold area (destroying the previous contents of the
hold area).
.if t .sp .5
.ti -.5i
(2)H -- Hold pattern space
.if t .sp .5
The
.ul
H
function appends the contents of the pattern space
to the contents of the hold area; the former and new contents
are separated by a newline.
.if t .sp .5
.ti -.5i
(2)g -- get contents of hold area
.if t .sp .5
The
.ul
g
function copies the contents of the hold area into
the pattern space (destroying the previous contents of the
pattern space).
.if t .sp .5
.ti -.5i
(2)G -- Get contents of hold area
.if t .sp .5
The
.ul
G
function appends the contents of the hold area to the
contents of the pattern space; the former and new contents are separated by
a newline.
.if t .sp .5
.ti -.5i
(2)x -- exchange
.if t .sp .5
The exchange command interchanges the contents
of the pattern space and the hold area.
.in 0
.SH
Example
.LP
The commands
.nf
.if t .sp .5
	1h
	1s/ did.*//
	1x
	G
	s/\en/  :/
.if t .sp .5
.fi
applied to our standard example, produce:
.nf
.if t .sp .5
	In Xanadu did Kubla Khan  :In Xanadu
	A stately pleasure dome decree:  :In Xanadu
	Where Alph, the sacred river, ran  :In Xanadu
	Through caverns measureless to man  :In Xanadu
	Down to a sunless sea.  :In Xanadu
.if t .sp .5
.fi
.SH
3.6.  Flow-of-Control Functions
.LP
These functions do no editing on the input
lines, but control the application of functions
to the lines selected by the address part.
.if t .sp .5
.in +1i
.ti -.5i
(2)! -- Don't
.if t .sp .5
The
.ul
Don't
command causes the next command
(written on the same line), to be applied to all and only those input lines
.ul
not
selected by the adress part.
.if t .sp .5
.ti -.5i
(2){ -- Grouping
.if t .sp .5
The grouping command `{' causes the
next set of commands to be applied
(or not applied) as a block to the
input lines selected by the addresses
of the grouping command.
The first of the commands under control of the grouping
may appear on the same line as the
`{' or on the next line.
.LP
The group of commands is terminated by a
matching `}' standing on a line by itself.
.LP
Groups can be nested.
.ti -.5i
.if t .sp .5
(0):<label> -- place a label
.if t .sp .5
The label function marks a place in the list
of editing commands which may be referred to by
.ul
b
and
.ul
t
functions.
The <label> may be any sequence of eight or fewer characters;
if two different colon functions have identical labels,
a compile time diagnostic will be generated, and
no execution attempted.
.if t .sp .5
.ti -.5i
(2)b<label> -- branch to label
.if t .sp .5
The branch function causes  the sequence of editing commands being
applied to the current input line to be restarted immediately
after the place where a colon function with the same <label>
was encountered.
If no colon function with the same label can be found after
all the editing commands have been compiled, a compile time diagnostic
is produced, and no execution is attempted.
.if t .sp .5
A
.ul
b
function with no <label> is taken to be a branch to the end of the
list of editing commands;
whatever should be done with the current input line is done, and
another input line is read; the list of editing commands is restarted from the
beginning on the new line.
.if t .sp .5
.ti -.5i
(2)t<label> -- test substitutions
.if t .sp .5
The
.ul
t
function tests whether 
.ul
any
successful substitutions have been made on the current input
line;
if so, it branches to <label>;
if not, it does nothing.
The flag which indicates that a successful substitution has
been executed is reset by:
.if t .sp .5
.in +1i
1) reading a new input line, or
.br
2) executing a
.ul
t
function.
.if t .sp .5
.in 0
.SH
3.7. Miscellaneous Functions
.LP
.in +1i
.ti -.5i
(1)= -- equals
.if t .sp .5
The = function writes to the standard output the line number of the
line matched by its address.
.if t .sp .5
.ti -.5i
(1)q -- quit
.if t .sp .5
The
.ul
q
function causes the current line to be written to the
output (if it should be), any appended or read text to be written, and
execution to be terminated.
.in 0
.SH
.SH
Reference
.IP [1]
Ken Thompson and Dennis M. Ritchie,
.ul
The UNIX Programmer's Manual.
Bell Laboratories, 1978.
