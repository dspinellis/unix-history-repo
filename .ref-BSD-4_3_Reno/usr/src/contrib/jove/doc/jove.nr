.hy 0
.TH JOVE 1 "12 February 1986"
.ad
.SH NAME
jove - an interactive display-oriented text editor
.SH SYNOPSIS
.nf
jove [-d directory] [-w] [-t tag] [+[n] file] [-p file] [files]
jove -r
.fi
.SH DESCRIPTION
JOVE is Jonathan's Own Version of Emacs.  It is based on the original EMACS
editor written at MIT by Richard Stallman.  Although JOVE is meant to be
compatible with EMACS, there are some major differences between the two
editors and you shouldn't rely on their behaving identically.
.LP
JOVE works on any reasonable display terminal that is described in the
.I termcap
file (see TERMCAP(5) for more details).  When you start up JOVE, it checks
to see whether you have your
.I TERM
environment variable set.  On most systems that will automatically be set up
for you, but if it's not JOVE will ask you what kind of terminal you are
using.  To avoid having to type this every time you run JOVE you can set your
.I TERM
environment variable yourself.  How you do this depends on which shell you
are running.  If you are running the C Shell, as most of you are, you type
.sp 1
     % setenv TERM
.I type
.sp 1
and with the Bourne Shell, you type
.sp 1
     $ TERM=
.I type
; export TERM
.sp 1
where
.I type
is the name of the kind of terminal you are using (e.g., vt100).  If
neither of these works get somebody to help you.
.SH INVOKING JOVE
If you run JOVE with no arguments you will be placed in an empty buffer,
called
.I Main.
Otherwise, any arguments you supply are considered file names and each is
"given" its own buffer.  Only the first file is actually read in--reading
other files is deferred until you actually try to use the buffers they are
attached to.  This is for efficiency's sake: most of the time, when you run
JOVE on a big list of files, you end up editing only a few of them.
.LP
The names of all of the files specified on the command line are saved in a
buffer, called
.I *minibuf*.
The mini-buffer is a special JOVE buffer that is used when JOVE is prompting
for some input to many commands (for example, when JOVE is prompting for a
file name).  When you are being prompted for a file name, you can type C-N
(that's Control-N) and C-P to cycle through the list of files that were
specified on the command line.  The file name will be inserted where you are
typing and then you can edit it as if you typed it in yourself.
.LP
JOVE recognizes the following switches:
.TP
.I -d
The following argument is taken to be the name of the current directory.
This is for systems that don't have a version of C shell that automatically
maintains the
.I CWD
environment variable.  If
.I -d
is not specified on a system without a modified C shell, JOVE will have to
figure out the current directory itself, and that can be VERY slow.  You
can simulate the modified C shell by putting the following lines in your
C shell initialization file (.cshrc):
.nf
.sp 1
	alias cd        'cd \e!*; setenv CWD $cwd'
	alias popd      'popd \e!*; setenv CWD $cwd'
	alias pushd     'pushd \e!*; setenv CWD $cwd'
.fi
.TP
.I +n
Reads the file, designated by the following argument, and positions point at
the
.I n'th
line instead of the (default) 1'st line.  This can be specified more than
once but it doesn't make sense to use it twice on the same file; in that
case the second one wins. If no numeric argument is given after the +,
the point is positioned at the end of the file.
.TP
.I -p
Parses the error messages in the file designated by the following argument.
The error messages are assumed to be in a format similar to the C compiler,
LINT, or GREP output.
.TP
.I -t
Runs the
.I find-tag 
command on the string of characters immediately following 
the -t if there is one (as in -tTagname), or on
the following argument (as in -t Tagname) otherwise (see ctags(1)).
.TP
.I -w
Divides the window in two.  When this happens, either the same file is
displayed in both windows, or the second file in the list is read in and
displayed in its window.
.SH "RECOVERING BUFFERS AFTER A CRASH"
The
.I -r
option of jove runs the JOVE recover program.  Use this when the system
crashes, or JOVE crashes, or you accidently get logged out while in JOVE.
If there are any buffers to be recovered, this will find them.
.LP
Recover looks for JOVE buffers that are left around and are
owned by you.  (You cannot recover other peoples' buffers, obviously.)
If there were no buffers that were modified at the time of the
crash or there were but recover can't get its hands on them, you will be
informed with the message, "There is nothing to recover."  Otherwise,
recover prints the date and time of the version of the buffers it has,
and then waits for you type a command.
.LP
To get a list of the buffers recover knows about, use the
.I list
command.  This will list all the buffers and the files and the number of
lines associated with them.  Next to each buffer is a number.  When you want
to recover a buffer, use the
.I get
command.  The syntax is
.I get buffer filename
where
.I buffer
is either the buffer's name or the number at the beginning of the line.  If
you don't type the buffer name or the filename, recover will prompt you
for them.
.LP
If there are a lot of buffers and you want to recover all of them, use the
.I recover
command.  This will recover each buffer to the name of the buffer with ".#"
prepended to the name (so that the original isn't over-written).  It asks
for each file and if you want to restore that buffer to that name you type
"yes".  If you want to recover the file but to a different name, just type
the new name in.  If you type "no" recover will skip that file and go on
to the next one.
.LP
If you want to look at a buffer before deciding to recover it, use the
.I print
command.  The syntax for this is
.I print buffer
where
.I buffer
again is either its name or the number.  You can type ^C if you want to
abort printing the file to the terminal, and recover will respond with
an appropriate message.
.LP
When you're done and have all the buffers you want, type the
.I quit
command to leave.  You will then be asked whether it's okay to delete the
tmp files.  Most of the time that's okay and you should type "yes".  When
you say that, JOVE removes all traces of those buffers and you won't be able
to look at them again.  (If you recovered some buffers they will still be
around, so don't worry.)  So, if you're not sure whether you've gotten all
the buffers, you should answer "no" so that you'll be able to run
recover again at a later time (presumably after you've figured out
which ones you want to save).
.LP
If you type ^C at any time other than when you're printing a file to the
terminal, recover will exit without a word.  If you do this but wish you
hadn't, just type "jove -r" to the shell again, and you will be put back
with no loss.
.SH GETTING HELP
Once in JOVE, there are several commands available to get help.  To execute
any JOVE command, you type "<ESC> X command-name" followed by <Return>.  To
get a list of all the JOVE commands you type "<ESC> X" followed by "?".  The
.I describe-bindings
command can be used to get a list containing each key, and its associated
command (that is, the command that gets executed when you type that key).
If you want to save the list of bindings, you can set the jove variable
.I send-typeout-to-buffer
to ON (using the 
.I set
command), and then execute the
.I describe-bindings
command.  This will create a buffer and put in it the bindings list it
normally would have printed on the screen.  Then you can save that buffer to
a file and print it to use as a quick reference card.  (See VARIABLES below.)
.LP
Once you know the name of a command, you can find out what it does with the
.I describe-command
command, which you can invoke quickly by typing "ESC ?".  The
.I apropos
command will give you a list of all the command with a specific string in
their names.  For example, if you want to know the names of all the
commands that are concerned with windows, you can run "apropos" with the
keyword
.I window.
.LP
If you're not familar with the EMACS command set, it would be worth your
while to use run TEACHJOVE.  Do do that, just type "teachjove" to your shell
and you will be placed in JOVE in a file which contains directions.  I highly
recommend this for beginners; you may save yourself a lot of time and
headaches.
.SH KEY BINDINGS and VARIABLES
You can alter the key bindings in JOVE to fit your personal tastes.  That
is, you can change what a key does every time you strike it.  For example,
by default the C-N key is bound to the command
.I next-line
and so when you type it you move down a line.  If you want to change a
binding or add a new one, you use the
.I bind-to-key
command.  The syntax is "bind-to-key <command> key".
.LP
You can also change the way JOVE behaves in little ways by changing the
value of some variables with the
.I set
command.  The syntax is "set <variable> value", where value is a number or a
string, or "on" or "off", depending on the context.  For example, if you
want JOVE to make backup files, you set the "make-backup-files" variable to
"on".  To see the value of a variable, use the "print <variable>" command.
.SH INITIALIZATION
JOVE automatically reads commands from an initialization file in your HOME
directory, called ".joverc".  In this file you can place commands that you
would normally type in JOVE.  If you like to rearrange the key bindings and
set some variables every time you get into JOVE, you should put them in your
initialization file.  Here are a few lines from mine:
.nf
	set match-regular-expressions on
	auto-execute-command auto-fill /tmp/Re\e|.*drft
	bind-to-key i-search-forward ^\e
	bind-to-key i-search-reverse ^R
	bind-to-key find-tag-at-point ^[^T
	bind-to-key scroll-down ^C
	bind-to-key grow-window ^Xg
	bind-to-key shrink-window ^Xs
.fi
(Note that the Control Characters can be either two character sequences
(e.g. ^ and C together as ^C) or the actual control character.  If you want
to use an ^ by itself you must BackSlash it (e.g., bind-to-key grow-window
^X\e^ binds grow-window to "^X^").
.SH SOME MINOR DETAILS
You should type C-\e instead of C-S in many instances.  For example, the way
to search for a string is documented as being "C-S" but in reality you
should type "C-\e".  This is because C-S is the XOFF character (what gets
sent when you type the NO SCROLL key), and clearly that won't work.  The XON
character is "C-Q" (what gets sent when you type NO SCROLL again) which is
documented as the way to do a quoted-insert.  The alternate key for this is
"C-^" (typed as "C-`" on vt100's and its look-alikes).  If you want to
enable C-S and C-Q and you know what you are doing, you can put the line:
.nf
	set allow-^S-and-^Q on
.fi
in your initialization file.
.LP
If your terminal has a metakey, JOVE will use it if you turn on the
"meta-key" variable.  JOVE will automatically turn on "meta-key" if the
METAKEY environment variable exists.  This is useful for if you have
different terminals (e.g., one at home and one at work) and one has a
metakey and the other doesn't.
.SH FILES
LIBDIR/.joverc - system wide initialization file
.sp 0
~/.joverc - personal initialization file
.sp 0
TMPDIR - where temporary files are stored
.sp 0
LIBDIR/teach-jove - the interactive tutorial
.sp 0
LIBDIR/portsrv - for running shells in windows (pdp11 only)
.SH SEE ALSO
.nf
ed(1) - for a description of regular expressions
.sp 0
teachjove(1) - for an interactive JOVE tutorial.
.fi
.SH DIAGNOSTICS
JOVE diagnostics are meant to be self-explanatory, but you are advised
to seek help whenever you are confused.  You can easily lose a lot of
work if you don't know EXACTLY what you are doing.
.SH BUGS
Lines can't be more than 1024 characters long.
.sp 1
Searches can't cross line boundaries.
.SH AUTHOR
Jonathan Payne
