.nr Qn 0 1
.de QQ
.sp
.IP \fB\\n+(Qn) 0.3i
..
.de AA
.IP \fR 0.75i
..
.Go 13 "QUESTIONS & ANSWERS"
.QQ
How can I make elvis run faster under DOS?
.AA
There are several things you can do.
The first thing to do is get a good screen driver such as NANSI.SYS.
This can speed up screen redrawing by as much as a factor of eight!
The DOS-specific part of section 12 tells you how to do this.
.AA
You might also consider reducing the size of the blocks that elvis uses.
You'll need to recompile \*E to do this.
The default BLKSIZE is 1024 byte for the DOS version of \*E, which means
that for each keystroke that you insert, elvis must shift an average of
about 500 bytes.
That's a lot to ask from a little old 5MHz 8088.
A BLKSIZE of 512 bytes might be more appropriate.
.AA
If you're \fIreally\fR desperate for more speed, you might want to make
\*E store its temporary files on a RAM disk.
However, this limits the size of the file you can edit, and it eliminates any
chance you may have had to recover your work after a power failure
or system crash, but it might be worth it; you decide.
To do this, add ":set dir=R:\\" (or whatever your RAM disk's name is)
to the \fIelvis.rc\fP file.
.AA
Next, consider turning off the "sync" option.
When the sync option is turned on, \*E will close the temporary file
and reopen it after every change, in order to force DOS to update
the file's directory entry.
If you put ":set nosync" into the \fIelvis.rc\fP file, then elvis will
only close the file when you start editing a different text file, or
when you're exiting \*E.
Consequently, there is no chance that you'll be able to recover your
changes after a power failure... so if you're going to this, then you
might as well store the temp files on the RAM disk, too.
.QQ
Where's the <Esc> key on a DEC keyboard?
.AA
I don't know.  Maybe the <F11> key?
You could always use ":map!" to make some other key act like the <Esc> key.
If all else fails, use <Control><[>.
.QQ
Is there a way to show which keys do what?
.AA
Yes.  The command ":map" will show what each key does in command mode,
and ":map!" (with an exclamation mark) shows what each key does in
input mode.
.AA
The table is divided into three columns: the key's label, the characters
that it sends, and the characters that \*E pretends you typed.
.QQ
How can I make \*E display long lines like the real vi?
.AA
You can't yet.
The next version of \*E shouldsupport this, though.
.QQ
I can't recover my text [under MS-DOS or Atari TOS].
According to the directory listing, the temporary file is 0 bytes long.
What went wrong?
.AA
MS-DOS and TOS only update a file's directory entry when the file is closed.
If the system crashes while the file is still open, then the file's length
is stored as 0 bytes.
The ":set sync" option is supposed to prevent this;
you probably turned it off in the interest of speed, right?
.AA
Under MS-DOS [I don't know about TOS], you should delete the empty
temporary file, and then run CHKDSK/F.
This \fImight\fP find the data that belonged in the empty file,
and place it in a new file with a name like "000001.CHK" -- something like that.
You can then try to extract the text from that temporary file by giving the
command "elvprsv -R 000001.chk >goodnews.txt".
If you're lucky, then your text might be in GOODNEWS.TXT.
.QQ
What is the most current version of \*E?
.AA
Each version of \*E that is released to the public has a version number
of the form "number point number".
As I write this, the most current version of elvis is 1.5.
.AA
The intermediate steps between one release and the next are labeled with
the \fInext\fP version number, with a letter appended.
For example, after 1.4 was released, I started working on 1.5a.
I am currently working on 2.0a.
When \*E reaches a stable state, I'll call it 2.0 and release it.
.AA
Sometimes a beta-test version of elvis will be available via anonymous FTP
from m2xenix.psg.com, in the directory "pub/elvis/beta".
.QQ
I only got executables, but now I want the source code.
Where can I get it?
.AA
If you have access to the Internet, then you should be able to fetch it
from one of the public archives such as \fBplains.nodak.edu\fP.
It is accessible via anonymous FTP, or via an email server named
"archive-server@plains.nodak.edu".
Elvis is located in the directory "/pub/Minix/all.contrib".
.AA
I will also offer it to the C Users' Group.
They sell C source code for us$8 per diskette
(or slightly more outside  North  America).
Their phone number is (913) 841-1631,
and their address is:
.ID
The C Users' Group
PO Box 3127
Lawrence KS 66046-0127
.DE
.QQ
Is this shareware, or public domain, or what?
.AA
It is not public domain; it is copyrighted by me, Steve Kirkendall.
However, this particular version is freely redistributable, in either
source form or executable form.
(I would prefer that you give copies away for free, complete with the
full source code... but I'm not going to force you.)
.AA
It is not shareware; you aren't expected to send me anything.
You can use it without guilt.
.AA
It is not "copylefted."
I hold a copyright, but currently I have not added any of the usual restrictions
that you would find on copylefted software.
If people start doing really obnoxious things to \*E, then I will start
adding restrictions to subsequent versions, but earlier versions won't
be affected.
(So far, everybody has been pretty good about this so no restrictions
have been necessary.)
.QQ
Can I reuse parts of your source code?
.AA
Yes.  Please be careful, though, to make sure that the code really is mine.
Some of the code was contributed by other people, and I don't have the
authority to give you permission to use it.
The author's name can be found near the top of each source file.
If it says "Steve Kirkendall" then you may use it;
otherwise, you'd better contact the author first.
.AA
Please don't remove my name from the source code.
If you modify the source, please make a note of that fact in a comment
near the top of the source code.
And, finally, please mention my name in your documentation.
.QQ
Can \*E work with non-ASCII files?
.AA
\*E can't edit binary files because it can't handle the NUL character,
and because of line-length limitations.
However, it is 8-bit clean so you should be able to edit any European
extended ASCII file without any surprises.
.AA
\*E has also been modified to work with 16-bit character sets.
Yongguang Zhang (ygz@cs.purdue.edu) has created a Chinese version of \*E
that uses 16-bit characters and runs under cxterm (Chinese X-term)
on X-windows systems.
Junichiro Itoh (itojun@foretune.co.jp) has modified \*E to edit Japanese
text under MS-DOS.
