.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)mail6.nr	6.1 (Berkeley) 5/23/86
.\"
.bp
.sh 1 "Command line options"
.pp
This section describes command line options for
.i Mail
and what they are used for.
.ip \-N
Suppress the initial printing of headers.
.ip \-d
Turn on debugging information.  Not of general interest.
.ip "\-f file"
Show the messages in
.i file
instead of your system mailbox.  If
.i file
is omitted,
.i Mail
reads
.i mbox
in your home directory.
.ip \-i
Ignore tty interrupt signals.  Useful on noisy phone lines, which
generate spurious RUBOUT or DELETE characters.  It's usually
more effective to change your interrupt character to control\-c,
for which see the
.i stty
shell command.
.ip \-n
Inhibit reading of /usr/lib/Mail.rc.  Not generally useful, since
/usr/lib/Mail.rc is usually empty.
.ip "\-s string"
Used for sending mail.
.i String
is used as the subject of the message being composed.  If
.i string
contains blanks, you must surround it with quote marks.
.ip "\-u name"
Read
.i names's
mail instead of your own.  Unwitting others often neglect to protect
their mailboxes, but discretion is advised. Essentially,
.b "\-u user"
is a shorthand way of doing
.b "\-f /usr/spool/user".
.ip "\-v"
Use the
.b \-v
flag when invoking sendmail. This feature may also be enabled
by setting the the option "verbose".
.pp
The following command line flags are also recognized, but are
intended for use by programs invoking
.i Mail
and not for people.
.ip "\-T file"
Arrange to print on
.i file
the contents of the
.i article-id
fields of all messages that were either read or deleted.
.b \-T
is for the
.i readnews
program and should NOT be used for reading your mail.
.ip "\-h number"
Pass on hop count information.
.i Mail
will take the number, increment it, and pass it with
.b \-h
to the mail delivery system.
.b \-h
only has effect when sending mail and is used for network mail
forwarding.
.ip "\-r name"
Used for network mail forwarding:  interpret
.i name
as the sender of the message.  The
.i name
and
.b \-r
are simply sent along to the mail delivery system.  Also,
.i Mail
will wait for the message to be sent and return the exit status.
Also restricts formatting of message.
.pp
Note that
.b \-h
and
.b \-r ,
which are for network mail forwarding, are not used in practice
since mail forwarding is now handled separately.  They may
disappear soon.
