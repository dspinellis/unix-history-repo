.TH JOVE_RECOVER 1 "12 February 1986"
.ad
.SH NAME
jove_recover - recover JOVE buffers after a system/editor crash
.SH SYNOPSIS
jove_recover [-syscrash] [-d directory]
jove -r
.SH DESCRIPTION
JOVE_RECOVER lets you recover your work in the JOVE editor in the event of a
system or JOVE crash.  It is designed to put invoked through JOVE with the
"-r" switch.  JOVE_RECOVER looks for JOVE buffers that are left around and
are owned by you.  (You cannot recover other peoples' buffers, obviously.)
When the system is rebooted after a crash, you type "jove -r" after you've
logged in.  If there were no buffers that were modified at the time of the
crash or there were but JOVE_RECOVER can't get its hands on them, you will be
informed with the message, There is nothing here for you.  Otherwise,
JOVE_RECOVER prints the date and time of the version of the buffers it has,
and then waits for you type a command.
.LP
To get a list of the buffers JOVE_RECOVER knows about, use the
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
you don't type the buffer name or the filename, JOVE_RECOVER will prompt you
for them.
.LP
If there are a lot of buffers and you want to recover all of them, use the
.I recover
command.  This will recover each buffer to the name of the buffer with ".#"
prepended to the name (so that the original isn't over-written).  It asks
for each file and if you want to restore that buffer to that name you type
"yes".  If you want to recover the file but to a different name, just type
that name in.  If you type "no" JOVE_RECOVER will skip that file and go on
to the next one.
.LP
If you want to look at a buffer before deciding to recover it, use the
.I print
command.  The syntax for this is
.I print buffer
where
.I buffer
again is either its name or the number.  You can type ^C if you want to
abort printing the file to the terminal, and JOVE_RECOVER will respond with
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
JOVE_RECOVER again at a later time (presumably after you've figured out
which ones you want to save).
.LP
If you type ^C at any time other than when you're printing a file to the
terminal, JOVE_RECOVER will exit without a word.  If you do this but wish you
hadn't, just type "jove -r" to the shell again, and you will be put back
with no loss.
.SH A SAMPLE SESSION
.sp 1
.nf
% jove -r
Found 2 buffers (last updated: Sun Apr 14 14:13:38 1985).
(Type '?' for options): list
1) buffer recover.1  "/u/staff/jpay/doc/recover.1" (120 lines)
2) buffer recover.c  "/u/staff/jpay/jove/recover.c" (635 lines)
(Type '?' for options): get recover.1 recover.save
"recover.save" 53 lines, 1821 characters.
(Type '?' for options): quit
Should I delete the tmp files? yes
%
.fi
.LP
Here I "got" the buffer
.I recover.1
and restored it to the temporary file
.I recover.save.
It's generally a good idea to recover buffers to temporary files and then
compare them to the original or at least look them over before putting them
back in their real name.  This is just in case you were restoring what you
thought you were restoring.
.fi
.SH FILES
TMPDIR - where temporary files are stored.
.SH SEE ALSO
JOVE(1) - for this to make any sense to you.
.SH DIAGNOSTICS
JOVE_RECOVER diagnostics are meant to be self-explanitory.
.SH BUGS
It works well enough...
.SH AUTHOR
Jonathan Payne
