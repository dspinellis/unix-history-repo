@Section(mhe -- a mail management system based on MH)
@Index(Mail, sending and receiving)
Mhe is an Emacs-based system that is used as a visual front end to the MH
mail system. MH is the Rand Mail Handler, which is available under license
from the Rand Corporation. Mhe is used as a mail program to send, receive,
classify, move, archive, search, and edit mail using the basic MH programs
as the underlying mechanism. While mhe can certainly be loaded from any
instance of @value(Emacs), the customary usage is to use mhe for a login
shell, or else to execute it immediately after login, and then to sit in it
all day, using it as both an editor and a mail reader.

When initially run, mhe presents you with a buffer containing a listing of
the headers of the mail messages in your current mail folder; you can then
peruse this buffer with all of the usual @value(Emacs) motion and search
commands. To delete a message, you position the cursor on the line
corresponding to that message and type "D"; to reply to a message, you
position the cursor on the line corresponding to it and type "R". All of the
basic mail-handling commands in mhe are single-character commands, as
follows:
@begin(description,spread 0,spacing 1,leftMargin 1 inch,Indent -0.8 inches)
n@\move cursor to next line

p@\move cursor to previous line

t@\type this message (the message represented by the current line). Pops up
a window and shows the message in it. Mhe key bindings are still in effect
while the cursor is in that window.

d@\delete this message. Marks it with a "D", and arranges for it to be
deleted when the mhe session is terminated.

^@\move this message to another folder. Prompts for its name. Marks it with
a "^" and arranges for it to be moved with the mhe session is terminated.

!@\repeat previous ^ (move) command. Uses same destination folder as
previous command, so no prompting is done.

u@\undelete/unmove: cancel delete or move command for this message. Since
the deleting and moving are not performed until mhe exits, those commands
can be undone.

m@\mail a message. Pops up a window whose contents are an empty mail
message; you fill in the "To:", "Subject:", and "Cc:" fields as you wish.
You can add "Fcc:" fields for file copies, "Bcc:" fields for blind copies,
and any other fields that you wish (such as "Reply-to:", etc.). Your
standard @value(Emacs) key bindings will be used in this window. When you
exit from the recursive edit with @b[^X^C], you will be asked for
instructions on handling the message, e.g. quit, send it, go back and edit
it some more.

r@\reply to the current message. Splits the screen, showing the message text
in one window and the reply in the other. Quite similar to the "mail"
command, except that the "Subject:",  "To:", and "Cc:" fields are filled in
for you. You can change them if you want, of course. When you send the
reply, the original message will be annotated with a "Replied:" field and
the date, and the letter "R" will appear in the header listing.

f@\forward the current message. Pops up a message composition window, just
like the "m" command, except that its initial contents are the contents of
the current message. When you send the message, the original that you
forwarded will be marked with an annotation showing that it has been
forwarded to someone, and the letter "R" will appear in the header listing.

e@\edit the current message. This command works just like the "type" command
described above, except that the keyboard has its "edit" key bindings, so
that you can change the message if you want.

i@\incorporate new mail. If the banner line shows that you have received new
mail, you can fetch it with this command. If you are currently working in
some folder besides +inbox, and if there is mail, then mhe will switch to
folder +inbox before incorporating the mail.

g@\get a new mail folder. Prompts you for the name of a new folder, and then
creates a new header buffer in the name of that folder. The old header
buffer is not destroyed, so that you can switch back and forth between them
as you see fit.

b@\get a bboard (bulletin board, otherwise known as newsgroup) folder. Mhe
lets you read newsgroup directories just as if they were mail in a mail
folder.

^X^C@\Exit from Mhe.

?@\Pop up a help window. Its topmost few lines give a command summary, and
if you scroll it down, various further instructions are given.
@end(Description)
Whenever the cursor is positioned in a header buffer, the above-mentioned
key bindings are in effect. In addition, all of the ^X-prefix key bindings
from your profile are left untouched, as are various other standard
@value(Emacs) key bindings like @b[ESC-], @b[^S], and so forth.
