.sh 1 "Common usage"
.pp
The Mail command has two distinct usages, according to whether one
wants to send or receive mail.  Sending mail is simple:  to send a
message to a user whose login name is, say,
\*(lqroot,\*(rq
use the Shell
command:
.(l
% Mail root
.)l
then type your message.  When you reach the end of the message, type
an EOT (control\-d) at the beginning of a line, which will cause Mail
to echo \*(lqEOT\*(rq and return you to the Shell.  When the user you sent mail
to next logs in, he will receive the message:
.(l
You have mail.
.)l
to alert him to the existence of your message.  Incidentally, once you have
sent mail to someone, there is no way to undo the act, so be
careful.  The message your recipient reads will consist of the message you
typed, preceded by a line telling who sent the message (your login name),
the teletype from which the message was sent, and the date and time it
was sent.
.pp
If you want to send the same message to several other people, you can list
all of their login names on the command line.
Thus,
.(l
% Mail sam bob john
Tuition fees are due next Friday.  Don't forget!!
<Control\-d>
EOT
%
.)l
will send the reminder to sam, bob, and john.
.pp
If, when you log in, you see the message,
.(l
You have mail.
.)l
you can read the mail by typing simply:
.(l
% Mail
.)l
Mail will respond by typing its version number and date and then listing
the messages you have waiting.  Then it will type an underscore and await
your command.  The messages are assigned numbers starting with 1 \*- you
can refer to the messages with these numbers.
.pp
To look at a specific message, use the
.b type
command, which may be abbreviated to simply
.b t .
For example, if you had the following messages:
.(l
   1 root     Wed Sep 21 09:21  "Tuition fees"
   2 sam      Tue Sep 20 22:55
.)l
you could examine the first message by giving the command:
.(l
type 1
.)l
which might cause Mail to respond with, for example:
.(l
Message  1:
From root  tty8  Wed Sep 21 09:21:45 1978
Subj: Tuition fees

Tuition fees are due next Wednesday.  Don't forget!!

.)l
.pp
Normally, each message you receive is saved in the file
.i mbox
in your login directory at the time you leave Mail.  Often,
however, you will not want to save a particular message you
have received because it is only of passing interest.  To avoid
saving a message in
.i mbox
you can delete it using the
.b delete
command.  In our example,
.(l
delete 1
.)l
will prevent Mail from saving message 1 (from root) in
.i mbox .
In addition to not saving deleted messages, Mail will not let
you type them, either.  The effect is to make the message disappear
altogether, along with its number.  The
.b delete
command can be abbreviated to simply
.b d .
.pp
When you have perused all of the messages of interest, you can leave
Mail with the
.b quit
command, which saves all of the messages you have typed but not
deleted in the file
.i mbox
in your login directory.  Deleted messages are discarded irretrievably,
and messages left untouched are preserved in your system mailbox so
that you will see them the next time you type:
.(l
% Mail
.)l
The
.b quit
command can be abbreviated to simply
.b q .
.pp
If you wish for some reason to leave
Mail quickly without altering either your system mailbox or
.i mbox ,
you can type the
.b x
command (short for
.b exit ),
which will immediately return you to the Shell without changing anything.
.pp
If, instead, you want to execute a Shell command without leaving Mail, you
can type the command preceded by an exclamation point, just as in the
text editor.  Thus, for instance:
.(l
!date
.)l
will print the current date without leaving Mail.
.pp
Finally, the
.b help
command is available to print out a brief summary of the Mail
commands, using only the single character command abbreviations.
