.sh 1 "Message lists"
.pp
The
.b type
and
.b delete
commands described in section two take a list of messages as argument,
as do many of the commands described in section six.  This section
describes the construction of message lists in general.
.pp
A
.i "message list"
consists of a list of message numbers, ranges, and names,
separated by spaces or tabs.  Message numbers may be either
decimal numbers, which directly specify messages, or one of the
special characters
.q \(ua
.q "."
or
.q "$"
to specify the first relevant, current, or last
relevant message, respectively.
.i Relevant
here means, for most commands
.q "not deleted"
and
.q "deleted"
for the
.b undelete
command.
.pp
A range of messages consists of two message numbers (of the form
described in the previous paragraph) separated by a dash.
Thus, to print the first four messages, use
.(l
type 1\-4
.)l
and to print all the messages from the current message to the last
message, use
.(l
type .\-$
.)l
.pp
A
.i name
is a user name.  All of the user names given in the message list are
collected together and each message selected by other means
is checked to make sure it was sent by one of the named users.
If the message consists entirely of user names, then every
message sent by one those users which is
.i relevant
(in the sense described earlier)
is selected.  Thus, to print every message sent to you by
.q root,
do
.(l
type root
.)l
.pp
As a shorthand notation, you can specify simply
.q *
to get every
.i relevant
(same sense)
message.  Thus,
.(l
type *
.)l
prints all undeleted messages,
.(l
delete *
.)l
deletes all undeleted messages, and
.(l
undelete *
.)l
undeletes all deleted messages.
