.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)mail1.nr	6.1 (Berkeley) 5/23/86
.\"
.sh 1 Introduction
.pp
.i Mail
provides a simple and friendly environment for sending and receiving mail.
It divides incoming mail into
its constituent messages and allows the user to deal with them
in any order.  In addition, it provides a set of
.i ed -\c
like commands for manipulating messages and sending mail.
.i Mail
offers the user simple editing capabilities to ease the composition
of outgoing messages, as well as providing the ability to define and send
to names which address groups of users.  Finally,
.i Mail
is able to send and receive messages across such networks as the
ARPANET, UUCP, and Berkeley network.
.pp
This document describes how to use the
.i Mail
program to send and receive messages.  The reader is not assumed to
be familiar with other message handling systems, but should be
familiar with the \s-2UNIX\s0\**
.(f
\** \s-1UNIX\s0 is a trademark of Bell Laboratories.
.)f
shell, the text editor, and some of the common \s-2UNIX\s0 commands.
.q "The \s-2UNIX\s0 Programmer's Manual,"
.q "An Introduction to Csh,"
and
.q "Text Editing with Ex and Vi"
can be consulted for more information on these topics.
.pp
Here is how messages are handled:
the mail system accepts incoming
.i messages
for you from other people
and collects them in a file, called your
.i "system mailbox" .
When you login, the system notifies you if there are any messages
waiting in your system mailbox.  If you are a
.i csh
user, you will be notified when new mail arrives if you inform
the shell of the location of your mailbox.  On version 7 systems,
your system mailbox is located in the directory /usr/spool/mail
in a file with your login name.  If your login name is
.q sam,
then you can make
.i csh
notify you of new mail by including the following line in your .cshrc
file:
.(l
set mail=/usr/spool/mail/sam
.)l
When you read your mail using
.i Mail ,
it reads your system mailbox and separates that file into the
individual messages that have been sent to you.  You can then
read, reply to, delete, or save these messages.
Each message is marked with its author and the date they sent it.
