.sh 1 "Additional commands"
.pp
This section describes additional Mail commands available when
receiving mail.
.pp
The
.b next
command goes to the next message and types it.  If given a message list,
.b next
goes to the first such message and types it.  Thus,
.(l
type root
.)l
goes to the next message sent by \*(lqroot\*(rq and types it.
The
.b next
command can be abbreviated to simply a newline, which means that one
can go to and type a message by simply giving its message number or
one of the magic characters
.q "\(ua"
.q "."
or
.q "$".
Thus,
.(l
\&.
.)l
prints the current message and
.(l
4
.)l
prints message 4.
.pp
The
.rb \-
command goes to the previous message and prints it.  The
.rb \-
command may be given a decimal number
.i n
as an argument, in which case the
.i n th
previous message is gone to and printed.
.pp
The
.b save
command allows you to save messages received from others on a file
other than
.i mbox.
Its syntax varies somewhat from the other commands which accept
a message list in that the final word on the command line is taken to be
the file on which to save the messages.  The named messages are
appended to the file (which is created if it did not already exist)
and are marked as saved.  Saved messages are not automatically saved in
.i mbox
at quit time, nor are they selected by the
.b next
command described above, unless explicitly specified.  The
.b save
command provides a facility for saving messages pertaining to a particular
subject or from a particular person in a special place.
.pp
The
.b undelete
command causes a message which had been deleted previously to regain
its initial status.  Only messages which are already deleted may be
undeleted.  This command may be abbreviated to
.b u .
.pp
The
.b preserve
command takes a message list and marks each message therein so that it
will be saved in your system mailbox instead of being deleted or
saved in
.i mbox
when you quit.  This is useful for saving messages of importance that
you want to see again, or messages not intended for you if you are sharing
a login name.
.pp
Often, one wants to deal with a message by responding to its author right
then and there.  The
.b reply
command is useful for this purpose:  it takes a message list and sends mail
to the authors of those messages.  The message is collected in the usual
fashion by reading up to an EOT.  All of the tilde escapes described in section
three will work in
.b reply .
Additionally, if there are header fields
in the message being replied to, this information is copied into the
new message.  The
.b reply
command can be abbreviated to
.b r .
.pp
In order to simply mail to a user inside of Mail, the
.b mail
command is provided.  This sends mail in the manner described for the
.b reply
command above, except that the user supplies a list of recipient login
names and distribution groups.  All of the tilde escapes described in
section three will work in
.b mail .
The
.b mail
command may be abbreviated to
.b m .
.pp
In order to edit individual messages using the text editor, the
.b edit
command is provided.  The
.b edit
command takes a list of message as described under the
.b type
command and processes each by writing it into the file
Message\c
.i x
where
.i x
is the message number being edited and executing the text editor on it.
When you have edited the message to your satisfaction, write the message
out and quit, upon which Mail will read the message back and remove the file.
.b Edit
may be abbreviated to
.b e .
.pp
It is often useful to be able to invoke one of two editors,
based on the type of terminal one is using.  To invoke
a display oriented editor, you can use the
.b visual
command.  The operation of the
.b visual
command is otherwise identical to that of the
.b edit
command.
.pp
When Mail is invoked to receive mail, it prints out the message header
for each message.  In order to reprint the headers for remaining messages
(those which haven't been deleted), you may type the
.b headers
command.  Deleted messages do not appear in the listing, saved messages are
flagged with a
.q "*"
and preserved messages are flagged with a
\*(lqP.\*(rq
.pp
The
.b from
command takes a list of messages and prints out the header lines for each one;
hence
.(l
from joe
.)l
is the easy way to display all the message headers from \*(lqjoe.\*(rq
.pp
The
.b top
command takes a message list and prints the first five lines
of each addressed message.  It may be abbreviated to
.b to .
.pp
The
.b dt
command deletes the current message and prints the next message.
It is useful for quickly reading and disposing of mail.
