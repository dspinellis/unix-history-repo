.sh 1 "Command line options"
.pp
This section describes the alternate usages of Mail from the
shell.
.pp
As you continue to receive system mail, you will most likely
accumulate a large collection of messages in the file
.i mbox.
In order to help you deal with this, Mail allows you to edit
files of messages by using the
.rb -f
flag.  Specifically,
.(l
Mail \-f filename
.)l
causes Mail to edit \*(lqfilename\*(rq and
.(l
Mail \-f
.)l
causes Mail to read \*(lqmbox\*(rq in your home directory.  All of
the Mail commands except
.b preserve
are available to edit the messages.  When you type the
.b quit
command, Mail will write the updated file back.
.pp
Since you will usually have a large number of messages stored in
.i mbox,
Mail will only print out the first 18 message headers when editing
more than 18 messages.  To display the other message headers, the
.b headers
command takes as an optional argument either + or \- to move forward
or back to the next or previous 18 message group.
.pp
If you send mail over a noisy phone line, you will notice that many
of the garbage characters turn out to be the \s-2RUBOUT\s0 character,
which causes Mail to abort the message.  To deal with this annoyance,
you can invoke Mail with the
.rb -i
option to causes these garbage characters to be ignored.  Unfortunately,
as you are typing in a line of text to a program, the little gnome which
gathers up the characters is instructed to throw them all away when
a \s-2RUBOUT\s0 is seen.  For this reason, Mail indicates that a
\s-2RUBOUT\s0 has been received by echoing an @ to tell you that everything
you had typed on that line has been thrown away.
