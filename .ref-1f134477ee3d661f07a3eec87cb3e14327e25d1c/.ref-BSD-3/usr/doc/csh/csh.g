.SH
Glossary
.PP
This glossary lists the most important terms introduced in the
introduction to the
shell and gives references to sections of the shell
document for further information about them.
References of the form
`pr (1)'
indicate that the command
.I pr
is in the \s-2UNIX\s0 programmers manual in section 1.
You can get an online copy of its manual page by doing
.DS
man 1 pr
.DE
References of the form (2.5)
indicate that more information can be found in section 2.5 of this
manual.
.IP \&\fB.\fR 15n
Your current directory has the name `.' as well as the name printed
by the command
.I pwd.
The current directory `.' is usually the first component of the search
path contained in the variable
.I path,
thus commands which are in `.' are found first (2.2).
The character `.' is also used in separating components of filenames
(1.6).
The character `.' at the beginning of a component of a pathname is
treated specially and not matched by the filename expansion
metacharacters `?', `*', and `[' `]' pairs (1.6).
.IP \&\fB..\fR
Each directory has a file `..' in it which is a reference to its
.I parent
directory.
After changing into the directory with
.I chdir,
i.e.
.DS
chdir paper
.DE
you can return to the parent directory by doing
.DS
chdir ..
.DE
The current directory is printed by
.I pwd
(2.6).
.IP a.out
Compilers which create executable images create them, by default, in the
file `a.out', for historical reasons (2.3).
.IP alias
An
.I alias
specifies a shorter or different name for a \s-2UNIX\s0
command, or a transformation on a command to be performed in
the shell.
The shell has a
command
.I alias
which establishes aliases and can print their current values.
The command
.I unalias
is used to remove aliases (2.6).
.IP argument
Commands in \s-2UNIX\s0 receive a list of argument words.
Thus the command
.DS
echo a b c
.DE
consists of a command name `echo' and three argument words `a', `b' and `c' (1.1).
.IP argv
The list of arguments to a command written in the shell language
(a shell script or shell procedure) is stored in a variable called
.I argv
within the shell.
This name is taken from the conventional name in the
C programming language (3.4).
.IP background
Commands started without waiting for them to complete are called
.I background
commands (1.5).
.IP bin
A directory containing binaries of programs and shell scripts to be
executed is typically called a `bin' directory.
The standard system `bin' directories are `/bin' containing the most
heavily used commands and `/usr/bin' which contains most other user
programs.
Programs developed at UC Berkeley live in `/usr/ucb', while locally
written programs live in `/usr/local'.  Games are kept in the directory
`/usr/games'.
You can place binaries in any directory.
If you wish to execute them often, the name of the directories
should be a component of the variable
.I path.
.IP break
.I Break
is a built-in command used to exit from loops within the control
structure of the shell (3.6).
.IP builtin
A command executed directly by the shell is called a
.I builtin
command.
Most commands in \s-2UNIX\s0 are not built into the shell,
but rather exist as files in `bin' directories.
These commands are accessible because the directories in which
they reside are named in the
.I path
variable.
.IP case
A
.I case
command is used as a label in a
.I switch
statement in the shells control structure, similar to that of the
language C.
Details are given in the shells documentation `csh(1)' (3.7).
.IP cat
The
.I cat
program catenates a list of specified files on the standard output.
It is usually used to look at the contents of a single file on the terminal,
to `cat a file' (1.8, 2.3). 
.IP cd
The
.I cd
command is used to change the working directory.
With no arguments,
.I cd
changes your working directory to be your
.I home
directory (2.3) (2.6).
.IP chdir
The
.I chdir
command is a synonym for
.I cd.
.I Cd
is usually used because it is easier to type.
.IP chsh
The
.I chsh
command is used to change the shell which you use on \s-2UNIX\s0.
By default, you use an different version of the shell
which resides in `/bin/sh'.
You can change your shell to `/bin/csh' by doing
.DS
chsh your-login-name /bin/csh
.DE
Thus I would do
.DS
chsh bill /bin/csh
.DE
It is only necessary to do this once.
The next time you log in to \s-2UNIX\s0 after doing this command,
you will be using
.I csh
rather than the shell in `/bin/sh' (1.9).
.IP cmp
.I Cmp
is a program which compares files.
It is usually used on binary files, or to see if two files are identical (3.6).
For comparing text files the program
.I diff,
described in `diff (1)' is used.
.IP command
A function performed by the system, either by the shell
(a builtin command) or by a program residing in a file in
a directory within the \s-2UNIX\s0 system is called a
.I command
(1.1).
.IP "command substitution"
.br
The replacement of a command enclosed in `\`' characters
by the text output by that command 
is called
.I "command substitution"
(3.6, 4.3).
.IP component
A part of a
.I pathname
between `/' characters is called a
.I component
of that pathname.
A
.I variable
which has multiple strings as value is said to have
several
.I components,
each string is a
.I component
of the variable.
.IP continue
A builtin command which causes execution of the enclosing
.I foreach
or
.I while
loop to cycle prematurely.
Similar to the
.I continue
command in the programming language C (3.6).
.IP "core\ dump"
When a program terminates abnormally, the system places an image
of its current state in a file named `core'.
This `core dump' can be examined with the system debuggers `adb(1)'
or `sdb(1)' in order to determine what went wrong with the program (1.8).
If the shell produces a message of the form:
.DS
commandname: Illegal instruction \-\- Core dumped
.DE
(where `Illegal instruction' is only one of several possible
messages) you should report this to the author of the program
or a system administrator, 
saving the `core' file.
.IP cp
The
.I cp
(copy) program is used to copy the contents of one file into another
file.
It is one of the most commonly used \s-2UNIX\s0 commands (2.6).
.IP \&.cshrc
The file
.I \&.cshrc
in your
.I home
directory is read by each shell as it begins execution.
It is usually used to change the setting of the variable
.I path
and to set
.I alias
parameters which are to take effect globally (2.1).
.IP date
The
.I date
command prints the current date and time (1.3).
.IP debugging
.I Debugging
is the process of correcting mistakes in programs and shell scripts.
The shell has several options and variables which may be used
to aid in shell debugging (4.4).
.IP default
The label
.I default:
is used within shell
.I switch
statements, as it is in the C language
to label the code to be executed if none of the
.I case
labels matches the value switched on (3.7).
.IP \s-2DELETE\s0
The
\s-2DELETE\s0
or
\s-2RUBOUT\s0
key on the terminal is used to generate an
\s-2INTERRUPT\s0
signal in \s0UNIX\s0
which stops the execution of most programs (2.6).
.IP detached
A command run without waiting for it to complete is said to be detached
(2.5).
.IP diagnostic
An error message produced by a program is often referred to as a
.I diagnostic.
Most error messages are not written to the standard output,
since that is often directed away from the terminal (1.3, 1.5).
Error messsages are instead written to the
.I "diagnostic output"
which may be directed away from the terminal, but usually is not.
Thus diagnostics will usually appear on the terminal (2.5).
.IP directory
A structure which contains files.
At any time you are in one particular directory whose
names can be printed by the command `pwd'.
The
.I chdir
command will change you to another directory, and make the files
in that directory visible.
The directory in which you are when you first login is your
.I home
directory (1.1, 1.6).
.IP echo
The
.I echo
command prints its arguments (1.6, 2.6, 3.6, 3.10).
.IP else
The
.I else
command is part of the `if-then-else-endif' control
command construct (3.6).
.IP \s-2EOF\s0
An
.I "end-of-file"
is generated by the terminal by a control-d,
and whenever a command reads to the end of a file which
it has been given as input.
Commands receiving input from a
.I pipe
receive an end-of-file when the command sending them
input completes.
Most commands terminate when they receive an end-of-file.
The shell has an option to ignore end-of-file from a terminal
input which may help you keep from logging out accidentally
by typing too many control-d's (1.1, 1.8, 3.8).
.IP escape
A character \e used to prevent the special meaning of a metacharacter
is said to
.I escape
the character from its special meaning.
Thus
.DS
echo \e*
.DE
will echo the character `*' while just
.DS
echo *
.DE
will echo the names of the file in the current directory.
In this example, \e
.I escapes
`*' (1.7).
There is also a non-printing character called
.I escape, 
usually labelled
\s-2ESC\s0
or
\s-2ALTMODE\s0
on terminal keyboards.
Some older \s-2UNIX\s0 systems use this character to indicate that
output is to be suspended. 
Most systems use control-s to stop the output and control-q to start it.
.IP /etc/passwd
This file contains information about the accounts currently on the
system.
If consists of a line for each account with fields separated by
`:' characters (2.3).
You can look at this file by saying
.DS
cat /etc/passwd
.DE
The commands
.I finger
and
.I grep
are often used to search for information in this file.
See `finger(1)', `passwd(5)' and `grep(1)' for more details.
.IP exit
The
.I exit
command is used to force termination of a shell script,
and is built into the shell (3.9).
.IP "exit\ status"
A command which discovers a problem may reflect this back to the command
(such as a shell) which invoked (executed) it.
It does this by returning a non-zero number as its
.I "exit status,"
a status of zero being considered
`normal termination'.
The
.I exit
command can be used to force a shell command script to give a non-zero
exit status (3.5).
.IP expansion
The replacement of strings in the shell input which contain metacharacters
by other strings is referred to as the process of
.I expansion.
Thus the replacement of the word `*' by a sorted list of files
in the current directory is a `filename expansion'.
Similarly the replacement of the characters `!!' by the text of
the last command is a `history expansion'.
Expansions are also referred to as
.I substitutions
(1.6, 3.4, 4.2).
.IP expressions
Expressions are used in the shell
to control the conditional structures used in the writing of shell
scripts and in calculating values for these scripts.
The operators available in shell expressions are those of the language
C (3.5).
.IP extension
Filenames often consist of a
.I root
name and an
.I extension
separated by the character `.'.
By convention, groups of related files often share the same root name.
Thus if `prog.c' were a C program, then the object file for this
program would be stored in `prog.o'.
Similarly a paper written with the
`\-me'
nroff macro package might be stored in
`paper.me'
while a formatted version of this paper might be kept in
`paper.out' and a list of spelling errors in
`paper.errs' (1.6).
.IP filename
Each file in \s-2UNIX\s0 has a name consisting of up to 14 characters
and not including the character `/' which is used in
.I pathname
building.
Most file names do not begin with the character `.', and contain
only letters and digits with perhaps a `.' separating the root
portion of the filename from an extension (1.6).
.IP "filename expansion"
.br
Filename expansion uses the metacharacters `*', `?' and `[' and `]'
to provide a convenient mechanism for naming files.
Using filename expansion it is easy to name all the files in
the current directory, or all files which have a common root name.
Other filename expansion mechanisms use the metacharacter `~' and allow
files in other users directories to be named easily (1.6, 4.2).
.IP flag
Many \s-2UNIX\s0 commands accept arguments which are not the names
of files or other users but are used to modify the action of the commands.
These are referred to as
.I flag
options, and by convention consists of one or more letters preceded by
the character `\-' (1.2).
Thus the
.I ls
list file commands has an option
`\-s' to list the sizes of files.
This is specified
.DS
ls \-s
.DE
.IP foreach
The
.I foreach
command is used in shell scripts and at the terminal to specify
repitition of a sequence of commands while the value of a certain
shell variable ranges through a specified list (3.6, 4.1).
.IP goto
The shell has a command
.I goto
used in shell scripts to transfer control to a given label (3.7).
.IP grep
The
.I grep
command searches through a list of argument files for a specified string.
Thus
.DS
grep bill /etc/passwd
.DE
will print each line in the file
`/etc/passwd'
which contains the string `bill'.
Actually,
.I grep
scans for
.I "regular expressions"
in the sense of the editors
`ed(1)' and `ex(1)' (2.3).
.I Grep
stands for
`globally find regular expression and print.'
.IP head
The
.I head
command prints the first few lines of one or more files.
If you have a bunch of files containing text which you are wondering
about it is sometimes is useful to run
.I head
with these files as arguments.
This will usually show enough of what is in these files to let you decide
which you are interested in (1.5).
.IP history
The
.I history
mechanism of the shell allows previous commands to be repeated,
possibly after modification to correct typing mistakes or to change
the meaning of the command.
The shell has a
.I "history list"
where these commands are kept, and a
.I history
variable which controls how large this list is (1.7, 2.6).
.IP "home\ directory"
Each user has a home directory, which is given in your entry
in the password file,
.I /etc/passwd.
This is the directory which you are placed in when you first log in.
The
.I cd
or
.I chdir
command with no arguments takes you back to this directory, whose
name is recorded in the shell variable
.I home.
You can also access the home directories of other users in forming
filenames using a file expansion notation and the character `~' (1.6).
.IP if
A conditional command within the shell, the
.I if
command is used in shell command scripts to make decisions
about what course of action to take next (3.6).
.IP ignoreeof
Normally, your shell will exit, printing
`logout'
if you type a control-d at a prompt of `% '.
This is the way you usually log off the system.
You can
.I set
the
.I ignoreeof
variable if you wish in your
.I \&.login
file and then use the command
.I logout
to logout.
This is useful if you sometimes accidentally type too many control-d
characters, logging yourself off
(2.2, 2.6).
.IP input
Many commands on \s-2UNIX\s0 take information from the terminal or from
files which they then act on.
This information is called
.I input.
Commands normally read for input from their
.I "standard input"
which is, by default, the terminal.
This standard input can be redirected from a file using a shell metanotation
with the character `<'.
Many commands will also read from a file specified as argument.
Commands placed in pipelines will read from the output of the previous
command in the pipeline.
The leftmost command in a pipeline reads from the terminal if
you neither redirect its input nor give it a file name to use as
standard input.
Special mechanisms exist for suppling input to commands in shell
scripts (1.2, 1.6, 3.8).
.IP interrupt
An
.I interrupt
is a signal to a program that is generated by hitting the
\s-2RUBOUT\s0 or \s-2DELETE\s0 key.
It causes most programs to stop execution.
Certain programs such as the shell and the editors
handle an interrupt in special ways, usually by stopping what they
are doing and prompting for another command.
While the shell is executing another command and waiting for it
to finish, the shell does not listen to interrupts.
The shell often wakes up when you hit interrupt because many commands
die when they receive an interrupt (1.8, 2.6, 3.9).
.IP kill
A program which terminates processes run without waiting for them to
complete. (2.6)
.IP \&.login
The file
.I \&.login
in your
.I home
directory is read by the shell each time you log in to \s-2UNIX\s0
and the commands there are executed.
There are a number of commands which are usefully placed here
especially
.I tset
commands and
.I set
commands to the shell itself (2.1).
.IP logout
The
.I logout
command causes a login shell to exit.
Normally, a login shell will exit when you hit control-d
generating an end-of-file, but if you have set
.I ignoreeof
in you
.I \&.login
file then this will not work and you must use
.I logout
to log off the \s-2UNIX\s0 system (2.2).
.IP \&.logout
When you log off of \s-2UNIX\s0 the shell will execute commands from
the file
.I \&.logout
in your
.I home
directory after it prints `logout'.
.IP lpr
The command
.I lpr
is the line printer daemon.
The standard input of
.I lpr
is spooled and printed on the \s-2UNIX\s0 line printer.
You can also give
.I lpr
a list of filenames as arguments to be printed.
It is most common to use
.I lpr
as the last component of a
.I pipeline
(2.3).
.IP ls
The
.I ls
list files command is one of the most commonly used \s-2UNIX\s0
commands.
With no argument filenames it prints the names of the files in the
current directory.
It has a number of useful
.I flag
arguments, and can also be given the names of directories
as arguments, in which case it lists the names of the files in these
directories (1.2).
.IP mail
The
.I mail
program is used to send and receive messages from other \s-2UNIX\s0
users (1.1, 2.2).
.IP make
The
.I make
command is used to maintain one or more related files and to
organize functions to be performed on these files. 
In many ways
.I make
is easier to use, and more helpful
than
shell command scripts (3.2).
.IP makefile
The file containing commands for
.I make
is called
`makefile' (3.2).
.IP manual
The `manual' often referred to is the
`\s-2UNIX\s0 programmers manual.'
It contains a number of sections and a description of each \s-2UNIX\s0
program.
An online version of the manual is accessible through the
.I man
command.
Its documentation can be obtained online via
.DS
man man
.DE
.IP metacharacter
Many characters which are neither letters nor digits have special meaning
either to the shell or to \s-2UNIX\s0.
These characters are called
.I metacharacters.
If it is necessary to place these characters in arguments to commands
without them having their special meaning then they must be
.I quoted.
An example of a metacharacter is the character `>' which is used
to indicate placement of output into a file.
For the purposes of the
.I history
mechanism,
most unquoted metacharacters form separate words (1.4).
The appendix to this user's manual lists the metacharacters
in groups by their function.
.IP mkdir
The
.I mkdir
command is used to create a new directory (2.6).
.IP modifier
Substitutions with the history mechanism, keyed by the character `!'
or of variables using the metacharacter `$' are often subjected
to modifications, indicated by placing the character `:' after the
substitution and following this with the modifier itself.
The
.I "command substitution"
mechanism can also be used to perform modification in a similar way,
but this notation is less clear (3.6).
.IP noclobber
The shell has a variable
.I noclobber
which may be set in the file
.I \&.login
to prevent accidental destruction of files by the `>' output redirection
metasyntax of the shell (2.2, 2.5).
.IP nroff
The standard text formatter on \s-2UNIX\s0 is the program
.I nroff.
Using
.I nroff
and one of the available
.I macro
packages for it, it is possible to have documents automatically
formatted and to prepare them for phototypesetting using the
typesetter program
.I troff
(3.2).
.IP onintr
The
.I onintr
command is built into the shell and is used to control the action
of a shell command script when an interrupt signal is received (3.9).
.IP output
Many commands in \s-2UNIX\s0 result in some lines of text which are
called their
.I output.
This output is usually placed on what is known as the
.I "standard output"
which is normally connected to the users terminal.
The shell has a syntax using the metacharacter `>' for redirecting
the standard output of a command to a file (1.3).
Using the
.I pipe
mechanism and the metacharacter `|' it is also possible for
the standard output of one command to become the standard input of another
command (1.5).
Certain commands such as the line printer daemon
.I lpr
do not place their results on the standard output but rather in more
useful places such as on the line printer (2.3).
Similarly the
.I write
command places its output on another users terminal rather than its
standard output (2.3).
Commands also have a
.I "diagnostic output"
where they write their error messages.
Normally these go to the terminal even if the standard output has
been sent to a file or another command, but it is possible
to direct error diagnostics along with standard output using
a special metanotation (2.5).
.IP path
The shell has a variable
.I path
which gives the names of the directories in which it searches for
the commands which it is given.
It always checks first to see if the command it is given is
built into the shell.
If it is, then it need not search for the command as it can do it internally.
If the command is not builtin, then the shell searches for a file
with the name given in each of the directories in the
.I path
variable, left to right.
Since the normal definition of the
.I path
variable is
.DS
path	(. /usr/ucb /bin /usr/bin)
.DE
the shell normally looks in the current directory, and then in
the standard system directories `/usr/ucb', `/bin' and `/usr/bin' for the named
command (2.2).
If the command cannot be found the shell will print an error diagnostic.
Scripts of shell commands will be executed using another shell to interpret
them if they have `execute' bits set.
This is normally true because a command of the form
.DS
chmod 755 script
.DE
was executed to turn these execute bits on (3.3).
If you add new commands to a directory in the path, you should issue
the command `rehash' (2.2).
.IP pathname
A list of names, separated by `/' characters forms a
.I pathname.
Each
.I component,
between successive `/' characters, names a directory
in which the next component file resides.
Pathnames which begin with the character `/' are interpreted relative
to the
.I root
directory in the filesystem.
Other pathnames are interpreted relative to the current directory
as reported by
.I pwd.
The last component of a pathname may name a directory, but
usually names a file.
.IP pipeline
A group of commands which are connected together, the standard
output of each connected to the standard input of the next
is called a
.I pipeline.
The
.I pipe
mechanism used to connect these commands is indicated by
the shell metacharacter `|' (1.5, 2.3).
.IP pr
The
.I pr
command is used to prepare listings of the contents of files
with headers giving the name of the file and the date and
time at which the file was last modified (2.3).
.IP printenv
The
.I printenv
command is used on version 7 \s-2UNIX\s0 systems
to print the current setting of variables in the environment
(2.6).
.IP process
A instance of a running program is called a process (2.6).
The
numbers used by
.I kill
and printed by 
.I wait
are unique numbers generated for these processes by \s-2UNIX\s0.
They are useful in
.I kill
commands which can be used to stop background processes. (2.6)
.IP program
Usually synonymous with
.I command;
a binary file or shell command script
which performs a useful function is often
called a program.
.IP "programmers manual"
.br
Also referred to as the
.I manual.
See the glossary entry for `manual'.
.IP prompt
Many programs will print a prompt on the terminal when they expect
input.
Thus the editor
`ex(1)' will print a `:' when it expects input.
The shell prompts for input with `% ' and occasionally with `? ' when
reading commands from the terminal (1.1).
The shell has a variable
.I prompt
which may be set to a different value to change the shells main prompt.
This is mostly used when debugging the shell (2.6).
.IP ps
The
.I ps
command is used to show the processes you are currently running.
Each process is shown with its unique process number,
an indication of the terminal name it is attached to,
and the amount of \s-2CPU\s0 time it has used so far.
The command is identified by printing some of the words used
when it was invoked (2.6).
Shells, such as the
.I csh
you use to run the `ps' command are not normally shown in the output.
.IP pwd
The
.I pwd
command prints the full pathname of the current (working)
directory.
.IP quit
The
.I quit
signal,
generated by a control-\e
is used to terminate programs which are behaving unreasonably.
It normally produces a core image file (1.8).
.IP quotation
The process by which metacharacters are prevented their special
meaning, usually by using the character `\' in pairs, or by
using the character `\e' is referred to as
.I quotation
(1.4).
.IP redirection
The routing of input or output from or to a file is known
as
.I redirection
of input or output (1.3).
.IP repeat
The
.I repeat
command iterates another command a specified number of times (2.6).
.IP \s-2RUBOUT\s0
The \s-2RUBOUT\s0 or \s-2DELETE\s0
key generates an interrupt signal which is used to stop programs
or to cause them to return and prompt for more input (2.6).
.IP scratch file
Files whose names begin with a `#' are referred to as scratch files,
since they are automatically removed by the system after a couple of
days of non-use, or more frequently if disk space becomes tight (1.3).
.IP script
Sequences of shell commands placed in a file are called shell command 
scripts.
It is often possible to perform simple tasks using these scripts without
writing a program in a language such as C, by
using the shell to selectively run other programs (3.2, 3.3, 3.10).
.IP set
The builtin
.I set
command is used to assign new values to shell variables
and to show the values of the current variables.
Many shell variables have special meaning to the shell itself.
Thus by using the set command the behavior of the shell can be affected (2.1).
.IP setenv
On version 7 systems variables in the environment `environ(5)'
can be changed by using the
.I setenv
builtin command (2.6).
The
.I printenv
command can be used to print the value of the variables in the environment.
.IP shell
A shell is a command language interpreter.
It is possible to write and run your own shell,
as shells are no different than any other programs as far as the
system is concerned.
This manual deals with the details of one particular shell,
called
.I csh.
.IP "shell script"
See
.I script
(3.2, 3.3, 3.10).
.IP sort
The
.I sort
program sorts a sequence of lines in ways that can be controlled
by argument flags (1.5).
.IP source
The
.I source
command causes the shell to read commands from a specified file.
It is most useful for reading files such as
.I \&.cshrc
after changing them (2.6).
.IP "special character"
.br
See
.I metacharacters
and the
appendix to this manual.
.IP standard
We refer often to the
.I "standard input"
and
.I "standard output"
of commands.
See
.I input
and
.I output
(1.3, 3.8).
.IP status
A command normally returns a
.I status
when it finishes.
By convention a
.I status
of zero indicates that the command succeeded.
Commands may return non-zero status to indicate that some abnormal
event has occurred.
The shell variable
.I status
is set to the status returned by the last command.
It is most useful in shell commmand scripts (3.5, 3.6).
.IP substitution
The shell implements a number of
.I substitutions
where sequences indicated by metacharacters are replaced by other sequences.
Notable examples of this are history substitution keyed by the
metacharacter `!' and variable substitution indicated by `$'.
We also refer to substitutions as
.I expansions
(3.4).
.IP switch
The
.I switch
command of the shell allows the shell
to select one of a number of sequences of commands based on an
argument string.
It is similar to the
.I switch
statement in the language C (3.7).
.IP termination
When a command which is being executed finished we say it undergoes
.I termination
or
.I terminates.
Commands normally terminate when they read an end-of-file
from their standard input.
It is also possible to terminate commands by sending them
an
.I interrupt
or
.I quit
signal (1.8).
The
.I kill
program terminates specified command whose numbers are given (2.6).
.IP then
The
.I then
command is part of the shells
`if-then-else-endif' control construct used in command scripts (3.6).
.IP time
The
.I time
command can be used to measure the amount of \s-2CPU\s0
and real time consumed by a specified command (2.1, 2.6).
.IP troff
The
.I troff
program is used to typeset documents.
See also
.I nroff
(3.2).
.IP tset
The
.I tset
program is used to set standard erase and kill characters
and to tell the system what kind of terminal you are using.
It is often invoked in a
.I \&.login
file (2.1).
.IP unalias
The
.I unalias
command removes aliases (2.6).
.IP \s-2UNIX\s0
\s-2UNIX\s0 is an operating system on which
.I csh
runs.
\s-2UNIX\s0 provides facilities which allow
.I csh
to invoke other programs such as editors and text formatters which
you may wish to use.
.IP unset
The
.I unset
command removes the definitions of shell variables (2.2, 2.6).
.IP "variable expansion"
.br
See
.I variables
and
.I expansion
(2.2, 3.4).
.IP variables
Variables in
.I csh
hold one or more strings as value.
The most common use of variables is in controlling the behavior
of the shell.
See
.I path,
.I noclobber,
and
.I ignoreeof
for examples.
Variables such as
.I argv
are also used in writing shell programs (shell command scripts)
(2.2).
.IP verbose
The
.I verbose
shell variable can be set to cause commands to be echoed
after they are history expanded.
This is often useful in debugging shell scripts.
The
.I verbose
variable is set by the shells
.I \-v
command line option (3.10).
.IP wait
The builtin command
.I wait
causes the shell to pause, and not prompt,
until all commands run in the background have terminated (2.6).
.IP while
The
.I while
builtin control construct is used in shell command scripts (3.7).
.IP word
A sequence of characters which forms an argument to a command is called
a
.I word.
Many characters which are neither letters, digits, `\-', `.' or `/'
form words all by themselves even if they are not surrounded
by blanks.
Any sequence of character may be made into a word by surrounding it
with `\'' characters
except for the characters `\'' and `!' which require special treatment
(1.1, 1.6).
This process of placing special characters
in words without their special meaning is called
.I quoting.
.IP "working directory"
.br
At an given time you are in one particular directory, called
your working directory.
This directories name is printed by the
.I pwd
command and the files listed by
.I ls
are the ones in this directory.
You can change working directories using
.I chdir.
.IP write
The
.I write
command is used to communicate with other users who are logged in to
\s-2UNIX\s0 (2.3).
