.de SF		\" Give short form of a command (from $1)
[\\$1]
..
.sh 1 "Summary of commands, escapes, and options"
.pp
This sections describes tersely all of the Mail commands,
escapes, and options.
For each command,
its most abbreviated form (in brackets) and a
short description of the command is given below.
.pp
First, message lists are computed by determining the set M which
consists of all message referenced explicitly or through ranges.
Then, the set U is computed, which consists of all messages sent by
.i any
of the user names specified.  Finally, the message list is calculated
by finding the intersection of sets M and U.
.pp
Each Mail command is typed on a line by itself, and may take arguments
following the command word.  The command need not be typed in its
entirety \*- the first command which matches the typed prefix is used.
If the argument begins with a digit or special character, then
no space is required following the command letter, but otherwise the space
is required.  If a Mail command does not take arguments, they may be
specified, even though they are ignored.  For the commands which take
message lists as arguments, if no message list is given, then the
next message forward which satisfies the command's requirements is
used.  If there are no messages forward of the current message,
the search proceeds backwards, and if there are no good messages
at all, Mail types \*(lqNo applicable messages\*(rq and aborts the
command.
.nr ii 12n
.ip \-
.SF \-
Goes to the previous message and prints it out.  If  given
a numeric argument
.i n ,
goes to the
.i n th
previous message and prints it.  If there is no previous
message, it prints
.q "Nonzero address required."
.ip =
.SF =
Prints out the current message number.  Takes no arguments.
.ip ?
.SF ?
Prints out the file /usr/lib/Mail.help, which contains a brief summary
of the commands.  Takes no arguments.
.ip !
.SF !
Executes the \s-2UNIX\s0 Shell command which follows.  Unlike other commands,
there does not need to be a space after the exclamation point.
.ip alias
.SF a
With no arguments, prints out all currently-defined aliases.  With one
argument, prints out that alias.  With more than one argument, adds the
users named in the second and later arguments to the alias named in
the first argument.
.ip chdir
.SF c
Changes the user's working directory to that specified, if given.  If
no directory is given, then changes to the user's login directory.
.ip delete
.SF d
Takes a list of messages as argument and marks them all as deleted.
Deleted messages will not be saved in
.i mbox ,
nor will they be available for most other commands.  Default messages may
not be deleted already.
.ip dp
.SF dp
Deletes the current message and prints the next message.  If there is no
next message, types out
.q "At EOF."
.ip dt
.SF dt
Same as
.b dp .
.ip edit
.SF e
Takes a list of messages and points the text editor at each one in
turn.  On return from the editor, the message is read back in.  The default
message for
.b edit
may not be saved or deleted.
.ip exit
.SF ex
Effects an immediate return to the Shell without modifying
the user's system mailbox, his
.i mbox
file, or his edit file in
.b \-f .
.ip from
.SF f
Takes a list of messages and prints their message headers.  The default
message is neither saved nor deleted.
.ip headers
.SF h
Lists the current range of headers, which is an 18 message group.  If
the 
.q +
argument is given, then the next 18 message group is printed, and
if the 
.q \-
argument is given, the previous 18 message group is printed.
.ip help
.SF hel
A synonym for ?
.ip hold
.SF ho
Takes a message list and marks each message therein to be saved in the
user's system mailbox instead of in
.i mbox.
Does not override the
.b delete
command.  The default message must not be deleted.
.ip list
.SF l
The
.b list
command lists all of the available user commands in the order
that the command processor sees them.  It takes no arguments.
.ip mail
.SF m
Takes as argument login names and distribution group names
and sends mail to those people.  Tilde escapes work in
.b mail.
.ip next
.SF n
Goes to the next message in sequence and types it.  If a message
list is given, then
.b next
goes to the first message in the message list.
.ip preserve
.SF pre
A synonym for
.b hold.
.ip print
.SF p
Takes a message list and types out each message on the user's terminal.
The default message must not be deleted.
.ip quit
.SF q
Terminates the Mail session, saving all undeleted, unsaved messages
in the user's
.i mbox
file in his login directory, preserving all messages marked with
.b hold
or
.b preserve
in his system mailbox, and removing all other messages from his
system mailbox.  If mail has arrived during the Mail session,
the message \*(lqYou have new mail\*(rq is typed.  If executing
while editing a mailbox file with the
.b \-f
flag, then the edit file is rewritten.  A return to the Shell is
effected, unless the rewrite of edit file fails, in which case
the user can escape with the
.b exit
command.
.ip reply
.SF r
Takes a message list and sends mail to each message author
just like the
.b mail
command.  The default message must not be deleted.
.ip respond
.SF r
A synonym for
.b reply .
.ip save
.SF s
Takes a message list and a filename and appends each message in turn to
the end of the file.  The filename in quotes, followed by the line count
and character count is echoed on the user's terminal.  The default message
for
.b save
may not be saved or deleted.
.ip set
.SF se
With no arguments, prints all variable values.  Otherwise, sets option.
Arguments are of the form
.q "option=value"
or
.q option.
.ip shell
.SF sh
Invokes an interactive version of the shell.
.ip size
.SF si
Takes a message list and prints out the size in characters of
each message.  The default message for
.b size
must not be deleted.
.ip top
.SF to
Takes a message list and prints the top so many lines.  The number of lines
printed is controlled by the variable
.q toplines
and defaults to five.
.ip type
.SF t
A synonym for
.b print .
.ip unalias
.SF una
Takes a list of names defined by
.b alias
commands and discards the remembered groups of users.  The group names
no longer have any significance.
.ip undelete
.SF u
Takes a message list and marks each one as
.i not
being deleted.  Each message
in the list must already be deleted.  The default message must be deleted.
.ip unset
.SF uns
Takes a list of option names and discards their remembered values;
opposite of
.b set .
.ip visual
.SF v
Takes a message list and invokes the display editor on each one.
.ip write
.SF w
A synonym for
.b save .
.ip xit
.SF x
A synonym for
.b exit .
.pp
Recall that tilde escapes are used when composing messages to perform
special functions.  Tilde escapes are only recognized at the beginning
of lines.  The name
.q tilde\ escape
is somewhat of a misnomer since the actual escape character can be set
by the option
.q escape.
.pp
Here is a summary of the tilde escapes:
.nr ii 16n
.ip ~!command
Execute the indicated shell command, then return to the message.
.ip "~c name ..."
Add the given names to the list of carbon copy recipients.
.ip ~d
Read the file
.q dead.letter
from your home directory into the message.
.ip ~e
Invoke the text editor on the message collected so far.  After the
editing session is finished, you may continue appending text to the
message.
.ip ~h
Edit the message header fields by typing each one in turn and allowing the
user to append text to the end or modify the field by using the current
terminal erase and kill characters.
.ip "~m messages"
Read the named messages into the message being sent, shifted right one
tab.  If no messages are specified, read the current message.
.ip ~p
Print out the message collected so far, prefaced by the message header
fields.
.ip ~q
Abort the message being sent, copying the message to
.q "dead.letter"
in your home directory if
.q save
is set.
.ip "~r filename"
Read the named file into the message.
.ip "~s string"
Cause the named string to become the current subject field.
.ip "~t name ..."
Add the given names to the direct recipient list.
.ip "~v"
Invoke an alternate editor (defined by the VISUAL option)
on the message collected so far.  Usually, the alternate editor will be a
visual editor.  After you quit the editor, you may resume appending
text to the end of your message.
.ip "~w filename"
Write the message onto the named file.
.ip "~|command"
Pipe the message through the command as a filter.  If the command
gives no output or terminates abnormally, retain the original text
of the message.
.ip "~~string"
Insert the string of text in the message prefaced by a single ~.
If you have changed the escape character, then you should double
.i that
character in order to send it.
.pp
Options are controlled via the
.b set
and
.b unset
commands.  Options may be either binary, in which case it is only
significant to see whether they are set or not, or string, in which
case it's actual value is of interest.
.pp
The binary options include the following:
.ip append
Causes messages saved in
.i mbox
to be appended to the end rather than prepended.
.ip ask
Causes Mail to prompt you for the subject of each message you send.
If you respond with simply a newline, no subject field will be sent.
.ip askcc
Causes you to be prompted for additional carbon copy recipients at the
end of each message.  Responding with a newline indicates your
satisfaction with the current list.
.ip autoprint
Causes the
.b delete
command to behave like
.b dp
\*- thus, after deleting a message, the next one will be typed
automatically.
.ip ignore
Causes interrupt signals from your terminal to be ignored and echoed
as @'s.
.ip metoo
Usually, when a group is expanded that contains the sender,
the sender is removed from the expansion.  Setting this option causes
the sender to be included in the group.
.ip quiet
Suppresses the printing of the version when Mail is first invoked.
.ip save
Causes the message collected prior to a \s-2RUBOUT\s0 to be saved
on the file
.q dead.letter
in your home directory on receipt of the \s-2RUBOUT\s0.  Also causes
the message to be so saved in the same fashion for ~q.
.pp
The following options have string values:
.ip EDITOR
Pathname of the text editor to use in the
.b edit
command and ~e escape.  If not defined, then a default editor is used.
.ip SHELL
Pathname of the shell to use in the
.rb !
command and the ~! escape.  A default shell is used if this option
is not defined.
.ip VISUAL
Pathname of the text editor to use in the
.b visual
command
and ~v escape.
.ip escape
If defined, the first character of this option gives the character to
use in the place of ~ to denote escapes.
.ip record
If defined, gives the pathname of the file used to record all outgoing
mail.  If not defined, then outgoing mail is not so saved.
.ip toplines
If defined, gives the number of lines of a message to be printed out
with the
.b top
command; normally, the first five lines are printed.
