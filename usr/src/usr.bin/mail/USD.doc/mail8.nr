.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)mail8.nr	6.1 (Berkeley) %G%
.\"
.bp
.sh 1 "Glossary"
.pp
This section contains the definitions of a few phrases
peculiar to
.i Mail .
.ip "\fIalias\fP"
An alternative name for a person or list of people.
.ip "\fIflag\fP"
An option, given on the command line of
.i Mail ,
prefaced with a \-.  For example,
.b \-f
is a flag.
.ip "\fIheader field\fP"
At the beginning of a message, a line which contains information
that is part of the structure of the message.  Popular header fields
include
.i to ,
.i cc ,
and
.i subject .
.ip "\fImail\ \ \fP"
A collection of messages.  Often used in the phrase,
.q "Have you read your mail?"
.ip "\fImailbox\fP"
The place where your mail is stored, typically in the directory
/usr/spool/mail.
.ip "\fImessage\fP"
A single letter from someone, initially stored in your
.i mailbox .
.ip "\fImessage list\fP"
A string used in
.i Mail
command mode to describe a sequence of messages.
.ip "\fIoption\fP"
A piece of special purpose information used to tailor
.i Mail
to your taste.
Options are specified with the
.b set
command.
