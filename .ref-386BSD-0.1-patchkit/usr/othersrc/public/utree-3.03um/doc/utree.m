'''\"
'''\"   utree.man, utree 3.03-um manual page
'''\"   klin, Fri Mar  6 11:31:02 1992
'''\"
'''\"   [x]roff -man utree.man
'''\"   groff   -man [-Tdevice] utree.man
'''\"
'''\"   SCCSID = @(#) utree.man 3.03d-um Apr  2 1992
'''\"
.TH UTREE 1L "UTREE Version 3.03-um" "April 2 1992"
.SH NAME
.LP
\fButree\fR \- Screen oriented filesystem browser and utility
.SH SYNOPSIS
.LP
utree [options] [rootdirectory]
.SH DESCRIPTION
.LP
The screen oriented filesystem browser and utility
\fButree\fR descends the directory hierarchy
rooted in your home directory,
defined in a tree list file if given,
or rooted in \fBrootdirectory\fR if given,
and displays the directory tree on the so called \fItree screen\fR.
On the \fItree screen\fR \fButree\fR lets you walk thru the tree,
execute some default and user defined commands on it
and change to any directory in the tree.
.br
If you have changed to a directory \fButree\fR displays all files
in this directory on the so called \fIfile screen\fR.
On the \fIfile screen\fR \fButree\fR lets you walk thru the file list
and execute some default and user defined commands on files.
.SH OPTIONS
.LP
The following command line options are interpreted by \fButree\fR:
.sp
.TP 16
\fB-L\fR
Follow symbolic links to directories
.TP
\fB-S\fR
Ignore default minimal screen size of 80 columns and 24 lines
.TP
\fB-V\fR
Display \fButree\fR version and copyright notice
.TP
\fB-a\fR
Read in all directories,
including those that begin with a dot (.),
which are normally skipped
.TP
\fB-b\fR
Suppress ringing of the bell (useful for visual bells)
.TP
\fB-c\fR
Don't display and update a clock every second
.TP
\fB-d var=[val]\fR
Set the variable \fBvar\fR to value \fBval\fR
or unset the variable \fBvar\fR
.TP
\fB-d typ:[cmd]\fR
Define the command \fBcmd\fR for the filetype \fBtyp\fR
or undefine any command for the filetype \fBtyp\fR
.TP
\fB-f lst\fR
Build directory tree from list file \fBlst\fR
.TP
\fB-g\fR
Don't use graphical characters
.TP
\fB-h\fR
Display usage and some help about options
.TP
\fB-i ind\fR
Set the tree level indention to \fBind\fR columns (3..9)
.TP
\fB-n\fR
Avoid scanning the tree for changes i.e. after shell escape
.TP
\fB-l lev\fR
Build the tree up to level \fBlev\fR for quicker startup
.TP
\fB-o\fR
Omit saving changes in variables and filetype command definitions,
command history and key bindings
.TP
\fB-p lin\fR
Use \fBlin\fR lines for displaying filenames of the current directory
on the \fItree screen\fR (default: 3)
.TP
\fB-q\fR
Build the tree up to level 2 (like -l 2)
.TP
\fB-r\fR
Build the tree scanning and reading the file system
instead of reading a tree list file
.TP
\fB-s\fR
Don't use hardware scrolling
.TP
\fB-u\fR
Read and update all file lists after building
the tree from a tree list file
.TP
\fB-v mod\fR
Set using video attributes to mode \fBmod\fR.
\fBmod\fR may be \fB2\fR for all possible,
\fB1\fR for bold and underline
and \fB0\fR for none video attributes.
.TP
\fB-w\fR
Suppress warnings about unreadable directories
.TP
\fB-x cmd\fR
Use and execute the string \fBcmd\fR as initial input at startup.
The string \fBcmd\fR is a simple sequence of \fButree\fR commands
.PP
The boolean options \fBbcgnosw\fR and the
numerical options \fBipv\fR may also be pre-set in
the environment variable \fBUTREE\fR.
E.g. if you want to suppress ringing of the bell and
displaying and updating the clock and if you prefer a tree indention
of 4 columns set the environment variable \fBUTREE\fR to '\fBbci4\fR'
for this reason.
.br
Most of the options correspond to \fButree\fR
variables and therefore they may also be set or unset
with the variable command (\fB=\fR).
See the sections \fIglobal commands\fR and \fIvariables\fR below.
.SH UTREE STARTUP
.LP
If \fButree\fR is called without the optional command line parameter
\fBrootdirectory\fR or this directory is rooted in your
home directory \fButree\fR tries to build up the directory tree
reading a file \fB.utreelist\fR in your home directory,
which contains a list of your directory tree created
by a previous \fButree\fR session
or by the additional shell script \fButree.mklist\fR called before.
If the command line option \fB-f lst\fR is given
\fButree\fR builds up the tree from this list file \fBlst\fR
which may be created with the denoted shell script \fButree.mklist\fR
or a command like \fBfind\fR.
Entries in such list files have to start with the
directory separator / (slash) in the first column of the line.
All other entries are ignored and skipped.
.sp
If building the tree from a list file
the file lists of directories are created and read in on demand only,
which means when a directory becomes the current directory
by moving the cursor to this directory.
This speeds up the start of \fButree\fR because there is no need
for scanning and reading the filesystem for subdirectories
what may take some time for larger filesystems.
.sp
In all others cases,
if no list file is given with the
command line option \fB-f lst\fR,
\fBrootdirectory\fR is rooted in your home directory
and the file \fB.utreelist\fR is not found,
or the command line option \fB-r\fR is given
\fButree\fR builds up the tree by
scanning and reading the filesystem recursively.
You can speed up the start with the commandline option
\fB-l lev\fR which causes \fButree\fR to build the
initial directory tree only up to level \fBlev\fR similar to
the option \fB-l\fR of the well known \fBfind\fR command.
At runtime some commands let you expand the directory
tree later and inspect directories and subtrees
not visible at startup (see below).
.SH SCREEN LAYOUT
.LP
The screen is divided into three regions.
The top screen line,
the so called \fIecho line\fR is for displaying
messages and entering input.
The second screen line,
the so called \fIhelp line\fR is for displaying
help messages and the \fButree\fR default
or user defined menu lines.
.br
The rest of the screen forms a window on the directory tree
on the \fItree screen\fR or
on the file list of the current directory
on the \fIfile screen\fR.
Or he is used for displaying variable or filetype commands settings,
for displaying help pages and displaying command outputs.
.SH UTREE SCREENS AND MENUS
.LP
\fBUtree\fR knows the following screens and menus:
.sp
.TP 20
\fBtree screen\fR
This is the initial screen displayed when \fButree\fR is started
and has built up the directory tree.
The \fItree screen\fR forms a window on the directory tree.
The current directory on which most commands are
working is highlighted and the last
screen lines are used to display the first files
of the file list of the current directory.
You may move the window over the directory tree,
enlarge or shrink the directory tree window
or walk thru the directory tree and execute
default and user defined commands on directories and subtrees.
On the help line the most important commands available
for directories or subtrees on
the \fItree screen\fR are shown in the default \fItree menu\fR.
The tree menu shows the names of the
the \fItree screen\fR commands,
the keystrokes to invoke a command are marked with uppercase letters.
You may switch the menu to a user defined menu line
displaying user defined commands for the \fItree screen\fR.
For further information about all \fItree screen\fR commands
see the sections \fIglobal commands\fR,
\fItree screen commands\fR and \fIvariables\fR of this manual page.
.sp
.TP
\fBfile screen\fR
The \fIfile screen\fR forms a window on all files of the
current directory.
The current file on which most commands are
working is highlighted.
Yoy may move the window over the file list
or walk thru the file list and execute
default and user defined commands on files.
On the help line the most important commands
for files available on
the \fIfile screen\fR are shown in the default \fIfile menu\fR.
The file menu displays the names of the
the \fIfile screen\fR commands,
the keystrokes to invoke a command are marked with uppercase letters.
You may switch the menu to a user define menu line
displaying user defined commands for the \fIfile screen\fR.
For further information about all \fIfile screen\fR commands
see the sections \fIglobal commands\fR,
\fIfile screen commands\fR and \fIvariables\fR of this manual page.
.sp
.TP
\fBhelp screen\fR
If you have switched to the \fIhelp screen\fR with
the help command (\fBh\fR)
on the help line the
\fIhelp menu\fR is displayed and you may
select help about interesting topics.
The help menu displays the names of all
available help topics,
the keystrokes to select help about a topic
are marked with uppercase letters.
For more information see the section \fIhelp pages\fR below.
.sp
.TP
\fBvariables screen\fR
If you have switched to the \fIvariables\fR screen with
the variables command (\fB=\fR)
all variables and their settings are displayed
and you may set or unset any of the variables.
On the help line is shown how to set or unset a variable
in a short form.
All changes in variables definitions are
saved to the file \fB.utree\fR in your home directory
if the variable \fBAUTOSAVE\fR is set.
For more information see the section \fIvariables\fR below.
.sp
.TP
\fBcommands screen\fR
If you have switched to the filetype \fIcommands screen\fR with
the filetype commands command (\fB:\fR)
all filetype commands and their settings are displayed
and you may set or unset any of the filetype commands.
On the help line is shown how to set or
unset a filetype command in a short form.
All changes in filetype command definitions are
saved to the file \fB.utree\fR in your home directory
if the variable \fBAUTOSAVE\fR is set.
For more information see the section \fIfiletype commands\fR below.
.sp
.TP
\fBshell screen\fR
If you have switched to the \fIshell screen\fR with
the shell command (\fB!\fR) for executing commands
not supported directly by \fButree\fR
all previously entered commands saved in a so called
history list are displayed on the \fIshell screen\fR.
The last executed command is marked with \fB->\fR.
You may get any command from the history list
into the line editor for editing and execution
or enter and execute a new command.
To get a command from the history list into the line editor
you can use the keys \fBC-p\fR for the previous command or
\fBC-n\fR for the next command in the history list.
To get a command by number enter \fI!number\fR,
to get a command by a search pattern enter \fI!pattern\fR.
All commands up to a maximal number defined in
the variable \fBHISTSIZE\fR are saved in the
history list.
Before leaving \fButree\fR all saved commands
in the history list are saved to a
history file \fB.utreehist\fR in your home directory
if the variable \fBAUTOSAVE\fR is set.
At startup this file is searched for and read in
if found.
For more information see also the section \fIvariables\fR below.
.sp
.TP
\fBbindings screen\fR
If you have switched to the \fIbindings screen\fR
with the bindings command (\fB|\fR) all
currently defined key bindings and their meaning are
displayed and you may bind any key to an appropriate
utree command or to insert a string into the
input buffer.
At startup a file \fB.utree-TERM\fR in your home directory
or a file \fButree-TERM\fR in a global startup directory
is read in if found containing key bindings for
the terminal type defined in the environment variable \fBTERM\fR.
All changes in key bindings you have done on the
\fIbindings screen\fR at runtime are saved to \fB.utree-TERM\fR
if the variable \fBAUTOSAVE\fR is set.
For more information see also the section \fBkey bindings\fR
below.
.sp
.TP
\fBstatus screen\fR
The status command (\fBs\fR) displays all information
available about a file or directory on the \fIstatus screen\fR.
Here you may change the ownership, group membership
or access rights of a file or directory.
On BSD systems not all of the denoted changes may be allowed
for normal users.
.SH KEY NAMING CONVENTIONS AND DEFAULT KEY BINDINGS
.LP
All \fButree\fR commands are simple single letter commands or
control sequences.
The default or user defined commands therefore
are invoked with a single keystroke or a combination
of the <\fICONTROL\fR> key with
another key.
The naming conventions in the following manual sections
for \fButree\fR commands invoked by a keystroke are:
.sp
.TP  20
\fBkey\fR
means hit this <\fIkey\fR> only
.TP
\fBC-key\fR
means hold down the <\fICONTROL\fR>-key and hit <\fIkey\fR>
.PP
To permit rebinding of pre-defined keys or
binding functions keys to \fButree\fR commands
all control sequences have special names.
All key or functions names and their default bindings
are list in the table below
.TP 20
\fBSELECT\fR
CR, NL
.TP
\fBFORWARD\fR
C-f
.TP
\fBBACKWARD\fR
C-b
.TP
\fBNEXT\fR
C-n
.TP
\fBPREVIOUS\fR
C-p
.TP
\fBNEXTPAGE\fR
C-v
.TP
\fBPREVPAGE\fR
C-w
.TP
\fBBEGIN\fR
C-a
.TP
\fBEND\fR
C-e
.TP
\fBUP\fR
C-u
.TP
\fBDOWN\fR
C-d
.TP
\fBINSERT\fR
C-o
.TP
\fBDELETE\fR
BS
.TP
\fBKILL\fR
C-k
.TP
\fBSETMARK\fR
C-@
.TP
\fBGOTOMARK\fR
C-g
.TP
\fBGOTOTAG\fR
C-t,TAB
.TP
\fBHELP\fR
C-r
.TP
\fBREFRESH\fR
C-l
.TP
\fBCANCEL\fR
C-x
.TP
\fBBREAK\fR
C-c,C-y
.TP
\fBEXIT\fR
C-z
.PP
In the following sections of this manual page the
default key bindings are used instead of the
names or functions.
.PP
Some function keys are supported by \fButree\fR,
i.e. the four arrow or cursor keys,
and are bound to appropriate functions.
See the sections \fIkey bindings\fR and \fIfunction keys\fR
below for more details.
.SH HELP PAGES
.LP
If the \fButree\fR help pages contained in the file \fButree.help\fR
are accessible in a directory defined at compile time or defined in
the environment variable \fBUTLIB\fR you can get help
on all screens or from within the line editor
with the help command (\fBh\fR or \fB?\fR) or the help key (\fBC-r\fR).
After displaying help about your current context,
i.e. help about tree commands if you are on the \fItree screen\fR,
you can switch to the help menu and
select help about all topics with a single keystroke.
All \fIhelp screen\fR commmands and the menu items of the help menu
displayed on the help line are:
.sp
.TP 16
\fBh\fR
(Help) About the help pages and the help menu
.TP
\fBa\fR
(About) Information about \fButree\fR and key naming conventions
.TP
\fBu\fR
(Usage) Description of \fButree\fR usage and commandline options
.TP
\fBg\fR
(Global) Global commands common for the \fItree\fR and the \fIfile screen\fR
.TP
\fBt\fR
(Tree) Commands for the \fItree screen\fR
.TP
\fBf\fR
(File) Commands for the \fIfile screen\fR
.TP
\fBe\fR
(Edit) Commands of the builtin line editor
.TP
\fBv\fR
(Vars) \fBUtree\fR variables and variable definition
.TP
\fBc\fR
(Cmds) Filetype commands and filetype command definition
.TP
\fBl\fR
(Line) Line format for user defined tree, file and filetype commands.
.TP
\fBk\fR
(Keys) Function keys used by \fButree\fR
.TP
\fBp\fR
(Patterns) File pattern matching, filename, modification time and
file size patterns
.TP
\fBq\fR
(Quit)
Leave \fIhelp screen\fR
.PP
The help pages contain in short form most information given
in the sections of this manual page.
.SH GLOBAL COMMANDS
.LP
The following commands are common for the \fItree\fR and \fIfile screen\fR
They can be given in lowercase or uppercase letters:
.sp
.TP 20
\fBC-z\fR
Exit \fButree\fR from all screens
.TP
\fBC-c,C-y\fR
Cancel or break current command or input
.TP
\fBC-l\fR
Redisplay the current screen or the input line
.TP
\fBTAB,C-t\fR
Move to the next tagged file or the next directory
containing tagged files
.TP
\fBh,?,C-r\fR
Display help pages and switch to the \fIhelp screen\fR
and the help menu
.TP
\fB@,C-@\fR
Mark the current directory or file
.TP
\fB#,C-g\fR
Goto to a previously marked directory or file
.TP
\fBa\fR
Display \fButree\fR version and copyright notice
.TP
\fBd\fR
Display current date and time
.TP
\fBj\fR
Move to the next directory or file (for vi fans)
.TP
\fBk\fR
Move to the previous directory or file (for vi fans)
.TP
\fBn\fR
Change sort criteria from lexical order to modification time order
or vice versa and resort files in the file list
.TP
\fBt\fR
Tag files matching a file pattern for further processing
.TP
\fBu\fR
Untag files
.TP
\fBw\fR
Display full pathname of the current directory
.TP
\fBz\fR
Zoom files from filelist matching a file pattern
.TP
\fB=\fR
Switch to the \fIvariables screen\fR,
display and set or unset variables
.TP
\fB:\fR
Switch to the \fIcommands screen\fR,
display and set or unset
filetype commands
.TP
\fB|\fR
Switch to the \fIbindings screen\fR,
display all key bindings
and bind or rebind keys
.TP
\fB!\fR
Switch to the \fIshell screen\fR, display all commands
from the shell command history list, enter and execute
commands not supported directly from \fButree\fR.
Before a given command is executed the command line
is searched for some sprintf like format characters
lead in by a percent sign (%) which are expanded.
See the section \fIline formats\fR for more
information
.TP
\fB$\fR
Escape to an interactive shell
.SH TREE SCREEN COMMANDS
.LP
All commands on the \fItree screen\fR can be given in
lowercase or uppercase letters with the meaning denoted below.
Commands given in lowercase letters affect the current directory only.
Commands given in uppercase letters
indicated by an uppercase letter in the table below
affect the subtree rooted in the current directory
or all tagged files in the subtree rooted in the current directory.
The \fItree screen\fR commmands and the menu items of the
default tree menu displayed on the help line are:
.sp
.TP 20
\fB>,CR,NL,SP,>\fR
Change to the \fIfile screen\fR of the current directory
.TP
\fB<\fR
Change to the \fIfile screen\fR of the parent directory
.TP
\fBC-n\fR
Move to the next directory
.TP
\fBC-p\fR
Move to the previous directory
.TP
\fBC-f\fR
Move to the next directory on same level as the current directory
.TP
\fBC-b\fR
Move to the previous directory on same level as the current directory
.TP
\fBC-v\fR
Move one page forward
.TP
\fBC-w\fR
Move one page backward
.TP
\fBC-a\fR
Move to the beginning of the directory tree
.TP
\fBC-e\fR
Move to the end of the directory tree
.TP
\fBTAB,C-t\fR
Move to next the directory containing tagged files
.TP
\fBC-u\fR
Scroll up one line the directory tree
.TP
\fBC-d\fR
Scroll down one line the directory tree
.TP
\fB@,C-@\fR
Mark the current directory
.TP
\fB#,C-g\fR
Move to a previously marked directory
.TP
\fBh,?\fR
(Help) Help about \fItree screen\fR commands
.TP
\fBb,B\fR
(Backup) Backup the current directory or tree or
backup all tagged files in the subtree
.TP
\fBc,C\fR
(Chdir) Move to a directory or copy all tagged files
in the subtree. Before you move to a directory you are requested
for the name of this directory before.
Instead entering a name you can select a directory
from the tree with C-n and C-p.
Instead of entering a name of a directory
where to copy the tagged files you can
select a directory from the tree with C-n and C-p
or directly from \fItree screen\fR using CR.
.TP
\fBf,F\fR
(Find) Find files in the current directory or subtree matching a
file pattern you have to enter before.
If a file matching the given pattern is found
you may tag this file,
change to the directory containing the found file
or continue find
.TP
\fBg,G\fR
(Grep) Search for pattern in files in the current directory or subtree.
You are requested for a file and a search pattern.
If a file matching the search pattern is found
you may tag this file,
change to the directory containing the found file
or continue search
.TP
\fBi\fR
(Info) Display some short information about the current directory.
Displayed are access rights, modification time and
the disk usage of the current directory or subtree
.TP
\fBl,L\fR
(List) List files in the current directory or subtree matching a file pattern
you are requested before or list all tagged files in the subtree
.TP
\fBm,M\fR
(Mkdir) Create a new directory rooted in the current directory
or move all tagged files to a destination directory.
Instead of entering the directory name you can select
this name from the directory tree with C-n and C-p
or directly from \fItree screen\fR using CR.
.TP
\fBn,N\fR
Change the sort criteria from lexical order to modification time order
or vice versa and resort the file list of the current directory
or subtree
.TP
\fBo\fR
(Out) Write a list of directories, files, tagged files,
files matching a file pattern or a formatted tree list to a list file.
A formatted tree list file can later be displayed on the screen
or send to a printer using the additional filter command
\fButree.prlist\fR
.TP
\fBq\fR
(Quit) Leave the \fItree screen\fR and exit \fButree\fR
.TP
\fBr,R\fR
(Rmdir) Remove the current directory or all tagged files in
the subtree.
A directory to be removed may not
contain any subdirectories.
.TP
\fBs\fR
(Stat) Switch to the \fIstatus screen\fR,
display all status information of the current directory
and change owner, group and access rights of the current directory
.TP
\fBt,T\fR
(Tag) Tag files in the current directory or subtree matching a file pattern
you are requested before
.TP
\fBu,U\fR
(Untag) Untag files in the current directory or subtree
.TP
\fBz,Z\fR
Zoom files matching a file pattern in the
current directory or subtree.
Zooming means that only those files matching the file pattern
are displayed and visible for further processing.
.TP
\fB+\fR
Enlarge the tree window, shrink the file window one line
.TP
\fB-\fR
Shrink the tree window, enlarge the file window one line
.TP
\fB/\fR
Scan the current directory or tree and rebuild directories
if they need rebuilding
(i.e. if they are not yet read in or have changed)
.TP
\fB\e\fR
Scan the current directory for subdirectories
and build up and insert the subtree into the directory tree.
You are requested for the maximal tree level to build up
.TP
\fB0\fR
Switch the tree menuline from the default to the user defined
tree commands or vice versa
.TP
\fB1..9\fR
Execute the user defined tree command 1 .. 9
.PP
For further information about file patterns for the commands find,
grep, list, tag and untag see the section \fIfile patterns\fR below.
For user defined tree commands see the section \fIvariables\fR.
.SH FILE SCREEN COMMANDS
.LP
All commands on the \fIfile screen\fR can be given in lowercase
or uppercase letters with the meaning denoted below.
Commands given in lowercase letters  affect the current file only.
Commands given in uppercase letters indicated by an uppercase letter
in the table below affect
all tagged (selected) files if files are tagged or the current
file if no files are tagged.
The file \fIscreen commmands\fR and the menu items of the
default file menu displayed on the help line are:
.sp
.TP 20
\fBq,CR,NL,SP\fR
(Quit) Leave the \fIfile screen\fR and change back to the \fItree screen\fR
.TP
\fBC-f\fR
Move to the next file
.TP
\fBC-b\fR
Move to the previous file
.TP
\fBC-n\fR
Move to the file on the next line
.TP
\fBC-p\fR
Move to the file on the previous line
.TP
\fBC-v\fR
Move one page forward
.TP
\fBC-w\fR
Move one page backward
.TP
\fBC-a\fR
Move to the beginning of the file list
.TP
\fBC-e\fR
Move to the end of the file list
.TP
\fBTAB,C-t\fR
Move to the next tagged file
.TP
\fBC-u\fR
Scroll up one line the \fIfile screen\fR
.TP
\fBC-d\fR
Scroll down one line the \fIfile screen\fR
.TP
\fB@,C-@\fR
Mark the current file
.TP
\fB#,C-g\fR
Move to a previously marked file
.TP
\fBh,?\fR
(Help) Help about \fIfile screen\fR commands and
switch to the \fIhelp screen\fR
.TP
\fBc,C\fR
(Copy) Copy the current file or tagged files.
You are requested for a destination file or directory
where to copy the file or tagged files.
Instead of entering a directory name you can select
a destination directory using C-n and C-p or
select directly on the \fItree screen\fR with CR
.TP
\fBe,E\fR
(Edit) Edit the current file or tagged files
.TP
\fBf\fR
(Find) Find files matching a file pattern you are requested before
.TP
\fBg,G\fR
(Grep) Search for a pattern in the current file or tagged files.
Before search you are requested for a file pattern and the
search pattern to search for
.TP
\fBi,I\fR
Display some short information about the current file or tagged files.
Displayed are the access rights, the size and the
modification time of the current file or tagged files
.TP
\fBl,L\fR
(List) List files matching a file pattern you are requested before
or all tagged files
.TP
\fBm,M\fR
(Move) Move or rename the current file or tagged files.
You are requested for the new file name or a destination
directory where to move the current file or tagged files
Instead of entering the name of a destination directory
you can select a directory using C-n and C-p
or directly on the \fItree screen\fR with CR
.TP
\fBn\fR
Change the sort criteria from lexical order to modification time order
or vice versa and resort the file list
.TP
\fBp,P\fR
(Print) Print out the current file or tagged files
.TP
\fBr,R\fR
(Remove) Remove the current file or tagged files.
Before removing you are asked if you really
want to remove the current file or tagged files
.TP
\fBs,S\fR
(Stat) Switch to the \fIstatus screen\fR
display all status information of the current or tagged files
and change owner, group and access rights of
the current file or tagged files
.TP
\fBt,T\fR
(Tag) Tag  the current file or files matching a file pattern
you are requested before for further processing
.TP
\fBu,U\fR
(Untag) Untag the current file or files matching a file pattern
you are requested before
.TP
\fBv,V\fR
(View) View the current file or tagged files
.TP
\fBx,X\fR
Execute the current file or tagged files.
If a filetype command is defined for this file
you can execute this filetype command,
otherwise you are requested for a command
or for parameters to execute
.TP
\fBz\fR
Zoom files matching a file pattern
.TP
\fB>\fR
If the current file is a directory
change to the \fIfile screen\fR of this directory
.TP
\fB<\fR
Change back to the \fIfile screen\fR of the parent directory
.TP
\fB/\fR
Rebuild the file list (i.e. after shell escape)
.TP
\fB0\fR
Switch the menuline from the default to the user defined
file commands or vice versa
.TP
\fB1..9\fR
Execute the user defined file commands 1 .. 9
.PP
For further information about file patterns for the commands find,
grep, list, tag and untag see the section \fIfile patterns\fR below.
For user defined file commands see the section
\fIvariables\fR.
For filetype command execution invoked with the command \fBx\fR
see the section \fIfiletype commands\fR below.
.SH LINE EDITOR COMMANDS
.LP
Many \fButree\fR commands need some user input for
further processing which is done with a builtin
simple line editor.
Many commands pre-set the input buffer with
a default input line if this default is known.
For some commands you can use the keys
C-n and C-p to scroll in already existing
input lists and select an input line
for editing or processing
without entering the line completely.
The line editor knows about the following functions:
.sp
.TP 20
\fBCR,NL\fR
Accept and send the input line
.TP
\fBC-c,C-y\fR
Cancel input and leave the line editor
.TP
\fBC-o\fR
Switch from overwrite-mode to insert-mode or vice versa
.TP
\fBC-l\fR
Redisplay the input line
.TP
\fBC-f\fR
Move the cursor one character forward
.TP
\fBC-b\fR
Move the cursor one character backward
.TP
\fBC-a\fR
Move the cursor to the beginning of the input line
.TP
\fBC-e\fR
Move the cursor to the end of the input line
.TP
\fBC-v\fR
Scroll horizontally forward the input line
.TP
\fBC-w\fR
Scroll horizontally backward the input line
.TP
\fBC-d\fR
Delete one character under the cursor
.TP
\fBC-h,DEL\fR
Delete one character left from the cursor
.TP
\fBC-x\fR
Delete the input line completely
.TP
\fBC-k\fR
Delete the input line from the cursor position to the end
.TP
\fBC-t\fR
Transpose two characters under and left from the cursor
.TP
\fBC-r\fR
Display help pages and switch to the \fIhelp screen\fR
.TP
\fBC-@\fR
Set a mark at the current cursor position
.TP
\fBC-g\fR
Move the cursor to the previously marked position
.TP
\fBC-n\fR
Get the next entry into the line editor
.TP
\fBC-p\fR
Get the previous entry into the line editor
.PP
All other printable characters are appended at the end of input line,
inserted in insert-mode
or overwrite the character under the cursor in overwrite-mode.
The current mode is displayed at the end of the help line.
Most line editor commands are also available with function keys,
see the section \fIfunction keys\fR below.
.SH VARIABLES
.LP
Utree knows about and uses the following variables which may be
set or unset at startup in the startupfile \fB$HOME/.utree\fR,
with some commandline options (see \fIoptions\fR above),
or the variables command (\fB=\fR) on the \fIvariables screen\fR:
.sp
.TP 20
\fBBELL\fR
or \fBBL\fR: Allow ringing of the bell if set
.TP
\fBCLOCK\fR
or \fBCL\fR: Show and update clock every second if set
.TP
\fBGRAPHCHARS\fR
or \fBGC\fR: Use the graphical character set if set.
Not all terminal database termcap or terminfo
definitions of the graphical character set are
correct
.TP
\fBTERMSCROLL\fR
or \fBTS\fR: Use hardware terminal scrolling if set.
On some terminals (i.e. on the X terminal emulator xterm)
redrawing the screen may be faster than scrolling
.TP
\fBSCANTREE\fR
or \fBST\fR: Allow scanning the tree for changes if set.
Many commands scan the directory tree after execution
what may take some time.
Prohibiting tree scanning
therefore may speed up \fButree\fR a little bit
.TP
\fBWARNDIRS\fR
or \fBWD\fR: Allow warnings and requests about unreadable directories if set
.TP
\fBLEXSORT\fR
or \fBLS\fR: Sort filenames in lexical order if set,
in order of modification times if not set.
.TP
\fBAUTOSAVE\fR
or \fBAS\fR: Save changes in variables or filetype commands definitions,
key bindings and history list
to appropriate files in the home directory
.TP
\fBTREEINDENT\fR
or \fBTI\fR: Set the tree level indention column (3 .. 9) if possible.
Normally the tree level indention column is calculated dependent
on the number of screen columns and the maximal filesystem depth automatically
.TP
\fBVIDEOMODE\fR
or \fBVM\fR: Set using of video attributes.
2 means use all possible attributes and their combinations.
1 means use the attributes reverse and underline only.
0 means don't use any video attribute.
.TP
\fBFILELINES\fR
or \fBFL\fR: Number of lines of the file window on the \fItree screen\fR
(1 .. 9, default 3)
.TP
\fBHISTSIZE\fR
or \fBHS\fR: Maximal number of shell commands which are hold
in the shell commands history list (6 .. 99, default: 22)
.TP
\fBEDITOR\fR
or \fBED\fR: Program for editing files.
When redefining the editor variable don't forget to
check and set or unset the editopts variable for
editor options
.TP
\fBEDITOPTS\fR
or \fBEO\fR: File editor options
.TP
\fBPAGER\fR
or \fBPG\fR: Program for viewing files.
When redefining the pager variable don't forget to
check and set or unset the pageopts variable
for pager options
.TP
\fBPAGEOPTS\fR
or \fBPO\fR: File pager options
.TP
\fBXDUMPER\fR
or \fBXD\fR: Program for hexdumping files.
When redefining the hexdumper variable don't forget to
check and set or unset the xdumpopts variable
for hexdumper options
.TP
\fBXDUMPOPTS\fR
or \fBXO\fR: File hexdumper options
.TP
\fBLPRINTER\fR
or \fBLP\fR: Program for printing files or sending files to the
printer spooling system.
When redefining the lineprinter variable don't forget to
check and set or unset the lprintopts variable
for lineprinter options
.TP
\fBLPRINTOPTS\fR
or \fBLO\fR: Printer options
.TP
\fBBACKUP\fR
or \fBBK\fR: Program or shell script for backing up a directory or tree
.TP
\fBBACKUPOPTS\fR
or \fBBO\fR: Backup options
.TP
\fBSHELL\fR
or \fBSH\fR: Interactive shell for shell escape
.TP
\fBTREECMD1..9\fR
or \fBT1..9\fR: User defined \fItree screen\fR commands 1 .. 9
.TP
\fBFILECMD1..9\fR
or \fBF1..9\fR: User defined \fIfile screen\fR commands 1 .. 9
.PP
Variables are set with a line '\fBvariable=value\fR'
or '\fBshorthand=value\fR'
(i.e. '\fBtreecmd1=ps -ef\fR' or '\fBt1=ps -ef\fR')
and unset with a line '\fBvariable=\fR' or '\fBshorthand=\fR'
(i.e. '\fBt1=\fR').
When defining user tree or file commands some sprintf
like format characters lead in by a percent sign (\fB%\fR) have a
special meaning and are expanded before the command is executed.
For further information about the command line format see the
section \fIline formats\fR below.
.br
The last sharp sign (\fB#\fR) in a variable definition is used as leadin
for a menu item of the defined user file or tree command.
Example: the variable definition '\fBfc1=wc -l %F #Count\fR' for the user
defined file command 1 is expanded to '\fBwc -l filename\fR'
and in the user command file menu '\fBCount\fR' is displayed
behind menu item 1.
.SH FILETYPE COMMANDS
.LP
On \fIfile screen\fR you can execute a file or a command on it
with the \fButree\fR execute command (\fBx\fR).
You are requested for parameters if the current file is executable,
for a command to execute on the
current file if it is not executable.
For a type of file you can define so called \fIfiletype commands\fR
which are called if the current file matches a given file pattern.
.br
Filetype commands can be set and unset at startup in
the startupfile \fB$HOME/.utree\fR,
with the commandline option \fB-d\fR
or the filetype command (\fB:\fR) similar
to setting and unsetting variables.
Filetype commands are set with a line like '\fBfiletype:command\fR'
(i.e. '\fB*.c:cc -c -O\fR').
The command (i.e. '\fBcc -c -O\fR') is then executed
if the current file matches the given file pattern
(i.e. '\fB*.c\fR' for a C source file ending with '\fB.c\fR').
Filetype commands are unset with a line '\fBfiletype:\fR'
(i.e. '\fB*.c:\fR').
.br
When defining filetype commands some sprintf like format characters
lead in by a percent sign (\fB%\fR) have a special meaning and are
expanded before the command is executed. For further information
about file patterns and the format line
characters and her meaning
see the sections \fIfile patterns\fR
and \fIline formats\fR below.
.SH LINE FORMATS
.LP
When defining a user tree or file command
or a filetype command some sprintf like format line
characters are known and expanded before the command is executed.
These format line characters and their meaning are:
.sp
.TP 20
\fB%B\fR or \fB%b\fR
is expanded to the basename (filename without extension)
of the current file or directory
.TP
\fB%D\fR or \fB%d\fR
is expanded to the full pathname of the current directory
.TP
\fB%F\fR or \fB%f\fR
is expanded to the filename of the current file or directory
.TP
\fB%H\fR or \fB%h\fR
is expanded to the pathname of your home directory
.TP
\fB%P\fR or \fB%p\fR
is expanded to the full pathname of the current file or directory
.TP
\fB%R\fR or \fB%r\fR
is expanded to the pathname of the root directory
from where \fButree\fR was started
.TP
\fB%S\fR or \fB%s\fR
is expanded to additional parameter(s) for a command which are requested
before the command is executed
.PP
The command line '\fBcommand %s %f >%b.out\fR' i.e. is expanded before execution
to '\fBcommand parameters filename >basename.out\fR'
with filename of the current file or directory (\fB%f\fR),
basename.out of the current file or directory (\fB%b.out\fR)
and additional parameters (\fB%s\fR)
which are requested before command execution.
.br
For further information about tree, file and filetype commands
see the sections \fIvariables\fR and \fIfiletype commands\fR.
.SH FILE PATTERNS
.LP
Some commands (list, find, grep, tag or untag) require
file patterns for matching files using some
special (or meta) characters.
.br
Shell like filename pattern matching interprets the
following meta characters:
.sp
.TP 20
\fB*\fR
matches all characters in a filename
.TP
\fB?\fR
matches one character in a filename
.TP
\fB[class]\fR
matches one character from a character class.
A character class includes all characters enclosed between
the opening and closing brackets (\fB[\fR and \fB]\fR).
If in a class definition a minus sign (\fB-\fR) is found
between two other characters
this means the range from the character before and the
character behind the minus sign.
If the first character of a class definition is a exclamation mark
(\fB!\fR) this means matching of all characters excluded those
defined in the class, i.e.
.TP
[abc]
matches the characters 'a', 'b' and 'c'
.TP
[a-z_]
matches the characters from 'a' to  'z' and '_'
.TP
[!a-z_]
matches all characters except 'a' to 'z' and '_'
.PP
File size pattern matching interprets the following
meta characters:
.sp
.TP 20
\fB=size\fR
matches all files of size \fBsize\fR
.TP
\fB!size\fR
matches all files not of size \fBsize\fR
.TP
\fB>size\fR
matches all files larger than \fBsize\fR
.TP
\fB<size\fR
matches all files smaller than \fBsize\fR
.PP
Size may be specified in bytes (\fBb\fR, default),
kilo bytes (\fBk\fR) or mega bytes (\fBm\fR),
i.e. '\fB>2k\fR' matches all files larger than
2 kilo bytes or 2048 bytes.
.sp
.PP
The additional file time pattern matching interprets the
following meta characters:
.sp
.TP 20
\fB)time\fR
matches all files modified within \fBtime\fR
.TP
\fB(time\fR
matches all files not modified within \fBtime\fR
.PP
Time may be specified in minutes (\fBm\fR),
hours (\fBh\fR, default), days (\fBd\fR) or weeks (\fBw\fR),
i.e. '\fB)2d\fR' matches all files modified within last 2 days.
.sp
To combine shell like filename patterns and/or
additional file size and modification time patterns use
.sp
.TP 20
\fB&\fR
for \fBAND\fRing of patterns
.TP
\fB|\fR
for \fBOR\fRing of patterns
.PP
If a character is preceeded by a backslash or enclosed in quotes
his interpretation is suppressed and he is used as he is.
.SH KEY BINDINGS
.LP
All defaults key bindings are listed in the section
\fIkey naming conventions and default key bindings\fR above.
All supported function keys if defined in the termcap or
terminfo terminal database and their default bindings
are listed in the next section.
Rebindings of default keys or additional bindings
of other keys may be done in terminal dependent
startupfiles \fButree-TERM\fR in a global directory
containing \fButree\fR startupfiles or in
files \fB.utree-TERM\fR in your home directory where \fBTERM\fR denotes
the terminal type as defined in the environment variable \fBTERM\fR.
These startup files are built from lines
like \fB'key_sequence=utree_key'\fR
or \fB'key_sequence="string"'\fR.
Key_sequence describes the function key string,
utree_key the \fButree\fR key or function name,
a string enclosed in braces a string to insert
into the input buffer.
A comment lead in by a sharp sign (#) should
contain the name of the bound key.
For defining key sequences of function keys
control keys are defined with a
leading caret (^, i.e. ^x, ^? means DEL)
and some other special characters
may be defined lead in by a backslash (\e).
These escaped characters and their meaning are:
b backspace (^h),  f formfeed (^l),
n newline (^j), r return (^m), t tab (^i),
e or E escape (^[ or ESC) and s space.
If a backslash is followed by up to three
digits this defines an octal given character.
I.e. the definition '\ee^O\e003=END' binds the
keystring <ESCAPE> <CONTROL-O> <ASCII-3> to the
\fButree\fR key or function END.
.PP
The simplest way to define keys is to switch
to the \fIbindings screen\fR and there to do all bindings.
You have only to hit the key to bind and terminate
the key sequence with CR or NL.
Therefore CR or NL or key sequences containing CR or NL
cannot be bound on the \fIbindings screen\fR.
Then you have to enter the \fButree\fR name where the key
is to bind to.
You can use C-n and C-p to select the wanted name.
At last you should give a short comment to the
bound key(i.e. the key name) for documentation.
If you want to bind a key for insertion of a string
(and so to bind function keys to simple letter commands)
you have to enter the string enclosed in double quotes.
I.e. the binding \fB'\eeh="h"'\fR binds the
key <ESCAPE> <h> to the string <h> and the
so called help.
Any bindings you have done on the \fIbindings screen\fR
are saved to a file \fB.utree-TERM\fR into your home directory
if the variable AUTOSAVE is set.
.SH FUNCTION KEYS
.LP
The following function keys are supported by \fButree\fR
and pre-bound at startup to appropriate functions
if they are defined in your system's termcap or terminfo database:
.sp
.TP 20
\fBCursorRight\fR
Move forward (FORWARD, \fBC-f\fR)
.TP
\fBCursorLeft\fR
Move backward (BACKWARD, \fBC-b\fR)
.TP
\fBCursorUp\fR
Move up (PREVIOUS, \fBC-p\fR)
.TP
\fBCursorDown\fR
Move down (NEXT, \fBC-n\fR)
.TP
\fBHome/Begin\fR
Move to beginning (BEGIN, \fBC-a\fR)
.TP
\fBEnd\fR
Move to end (END, \fBC-e\fR)
.TP
\fBNextPage/PageDown\fR
Move one page down (NEXTPAGE, \fBC-v\fR)
.TP
\fBPrevPage/PageUp\fR
Move one page up (PREVPAGE, \fBC-w\fR)
.TP
\fBScrollUp\fR
Scroll one line up (UP, \fBC-u\fR)
.TP
\fBScrollDown\fR
Scroll one line down or delecte character (DOWN, \fBC-d\fR)
.TP
\fBInsert\fR
Change to directory (\fB>\fR) or
switch insert/overwrite mode (INSERT, \fBC-o\fR)
.TP
\fBDelete\fR
Change to parent directory (\fB<\fR)
or delete character under cursor like (DELETE, \fBC-h\fR)
.TP
\fBClear\fR
Refresh screen or input line (REFRESH, \fBC-l\fR)
.TP
\fBHelp\fR
Call the help menu (HELP, \fBC-r\fR)
.TP
\fBSelect\fR
Select and/or accept (SELECT, \fBCR,NL\fR)
.TP
\fBDo/Command\fR
Select and/or accept (SELECT, \fBCR,NL\fR)
.TP
\fBMark\fR
Set a mark (SETMARK, \fBC-@\fR)
.TP
\fBEnter\fR
Select and/or accept (SELECT, \fBCR,NL\fR)
.PP
This function keys may be rebound
or other function keys may be bound
on the \fIbindings screen\fR.
For more information about key bindings
see the section \fBkey bindings\fR above.
.SH ENVIRONMENT
.LP
.TP 20
\fBUTREE\fR
Some boolean and numerical settings for \fButree\fR
.TP
\fBUTLIB\fR
Directory for \fButree\fR help pages and startup files
.TP
\fBHOME\fR
User's home directory
.TP
\fBTERM\fR
Terminal type
.TP
\fBEDITOR\fR
File editor
.TP
\fBPAGER\fR
File pager/viewer
.TP
\fBSHELL\fR
Interactive shell for shell escape
.PP
.SH FILES
.LP
.TP 20
\fBHOME/.utreelist\fR
Tree list file of user's filesystem
.TP
\fBHOME/.utreehist\fR
History list of shell commands
.TP
\fBHOME/.utree\fR
User's \fButree\fR startup file
containing variables and filetype commands definitions
.TP
\fBUTLIB/utree\fR
Global \fButree\fR startup file
containing variables and filetype commands definitions
.TP
\fBUTLIB/utree-TERM\fR
Global key bindings for terminal type TERM
.TP
\fBHOME/.utree-TERM\fR
User's key bindings for terminal type TERM
.TP
\fBUTLIB/utree.help\fR
\fBUtree\fR help pages
.TP
\fBBIN/utree.backup\fR
Backup shell script or program
.TP
\fBBIN/utree.mklist\fR
Create directory tree list shell script
.TP
\fBBIN/utree.prlist\fR
Display or print a formatted tree list file
.PP
\fBUTLIB\fR can be a system default directory containing
library files (i.e. /usr/local/lib)
defined at compile time or a directory defined in the
environment variable \fBUTLIB\fR.
\fBBIN\fR is a public directory containing executable
files (i.e. /usr/local/bin) and should be
included in the command search path environment variable \fBPATH\fR.
.SH SEE ALSO
.LP
utree.prlist(1L)
.br
cp(1) grep(1) ls(1) mv(1) rm(1)
.br
du(1) mkdir(1) rmdir(1)
.br
find(1) sh(1)
.SH BUGS
.LP
Changes in filesystem after shell escape or editor session
are not always detected.
.br
Directory tree depth >32 may be confusing.
.br
Screen sizes smaller than 80x24 may be confusing.
.br
Symbolic links to directories may be confusing.
.br
On most BSD systems changing owner and/or group of files
for normal users is not allowed.
.SH AUTHOR
.LP
Peter Klingebiel
.SH COPYRIGHT
.LP
\(co 1991/1992 Peter Klingebiel & UNIX Magazin Munich
.sp
Permission is granted to copy and distribute \fButree\fR in modified
or unmodified form, for noncommercial use, provided (a) this copyright
notice is preserved, (b) no attempt is made to restrict redistribution
of this file, and (c) this file is not distributed as part of any
collection whose redistribution is restricted by a compilation copyright.
