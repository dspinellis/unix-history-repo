.Go 10 "TERMCAP"
.PP
\*E uses fairly standard termcap fields for most things.
I invented the cursor shape names
but other than that there should be few surprises.
.SH
Required numeric fields
.if n .ul 0
.ID
:co#:	number of columns on the screen (chars per line)
:li#:	number of lines on the screen
.DE
.SH
Required string fields
.ID
.if n .ul 0
:ce=:	clear to end-of-line
:cl=:	home the cursor & clear the screen
:cm=:	move the cursor to a given row/column
:up=:	move the cursor up one line
.DE
.SH
Boolean fields
.if n .ul 0
.ID
:am:	auto margins - wrap when char is written in last column?
:xn:	brain-damaged auto margins - newline ignored after wrap
:pt:	physical tabs?
.DE
.SH
Optional string fields
.if n .ul 0
.ID
:al=:	insert a blank row on the screen
:dl=:	delete a row from the screen
:cd=:	clear to end of display
:ei=:	end insert mode
:ic=:	insert a blank character
:im=:	start insert mode
:dc=:	delete a character
:sr=:	scroll reverse (insert row at top of screen)
:vb=:	visible bell
:ti=:	terminal initialization string, to start full-screen mode
:te=:	terminal termination, to end full-screen mode
:ks=:	enables the cursor keypad
:ke=:	disables the cursor keypad
.DE
.SH
Optional strings received from the keyboard
.if n .ul 0
.ID
:kd=:	sequence sent by the <down arrow> key
:kl=:	sequence sent by the <left arrow> key
:kr=:	sequence sent by the <right arrow> key
:ku=:	sequence sent by the <up arrow> key
:kP=:	sequence sent by the <PgUp> key
:kN=:	sequence sent by the <PgDn> key
:kh=:	sequence sent by the <Home> key
:kH=:	sequence sent by the <End> key
:kI=:	sequence sent by the <Insert> key
.DE
.PP
Originally, termcap didn't have any names for the <PgUp>, <PgDn>, <Home>,
and <End> keys.
Although the capability names shown in the table above are the most common,
they are \fInot\fR universal.
SCO Xenix uses :PU=:PD=:HM=:EN=: for those keys.
Also, if the four arrow keys happen to be part of a 3x3 keypad,
then the five non-arrow keys may be named :K1=: through :K5=:,
so an IBM PC keyboard may be described using those names instead.
\*E can find any of these names.
.SH
Optional strings sent by function keys
.if n .ul 0
.ID
:k1=:...:k9=:k0=:	codes sent by <F1> through <F10> keys
:s1=:...:s9=:s0=:	codes sent by <Shift F1> ... <Shift F10>
:c1=:...:c9=:c0=:	codes sent by <Ctrl F1> ... <Ctrl F10>
:a1=:...:a9=:a0=:	codes sent by <Alt F1> ... <Alt F10>
.DE
.PP
Note that :k0=: is used to describe the <F10> key.
Some termcap documents recommend :ka=: or even :k;=: for describing
the <F10> key, but \*E doesn't support that.
.PP
Also, the :s1=:..., :c1=:..., and :a1=:... codes are very non-standard.
The terminfo library doesn't support them.
.SH
Optional fields that describe character attributes
.if n .ul 0
.ID
:so=:se=:	start/end standout mode (We don't care about :sg#:)
:us=:ue=:	start/end underlined mode
:md=:me=:	start/end boldface mode
:as=:ae=:	start/end alternate character set (italics)
:ug#:		visible gap left by :us=:ue=:md=:me=:as=:ae=:
.DE
.SH
Optional fields that affect the cursor's shape
.PP
The :cQ=: string is used by \*E immediately before exiting to undo
the effects of the other cursor shape strings.
If :cQ=: is not given, then all other cursor shape strings are ignored.
.ID
:cQ=:	normal cursor
:cX=:	cursor used for reading EX command
:cV=:	cursor used for reading VI commands
:cI=:	cursor used during VI input mode
:cR=:	cursor used during VI replace mode
.DE
.PP
If the capabilities above aren't given, then \*E will try to use the
following values instead.
.ID
:ve=:	normal cursor, used as :cQ=:cX=:cI=:cR=:
:vs=:	gaudy cursor, used as :cV=:
.DE
.SH
An example
.PP
Here's the termcap entry I use on my Minix-ST system.
Some of the fields in it have nothing to do with \*E.
Some can only work on my system;
I have modified my kernel's screen driver.
.sp
.LD
.ne 14
mx|minix|minixst|ansi:\\
	:is=\\E[0~:co#80:li#25:bs:pt:\\
	:cm=\\E[%i%d;%dH:up=\\E[A:do=^J:nd=\\E[C:sr=\\EM:\\
	:cd=\\E[J:ce=\\E[K:cl=\\E[H\\E[J:\\
	:al=\\E[L:dl=\\E[M:ic=\\E[@:dc=\\E[P:im=:ei=:\\
	:so=\\E[7m:se=\\E[m:us=\\E[4m:ue=\\E[m:\\
	:md=\\E[1m:me=\\E[m:as=\\E[1;3m:ae=\\E[m:\\
	:ku=\\E[A:kd=\\E[B:kr=\\E[C:kl=\\E[D:\\
	:k1=\\E[1~:k2=\\E[2~:k3=\\E[3~:k4=\\E[4~:k5=\\E[5~:\\
	:k6=\\E[6~:k7=\\E[17~:k8=\\E[18~:k9=\\E[19~:k0=\\E[20~:\\
	:kU=\\E[36~:kQ=\\E[32~:kH=\\E[28~:\\
	:GV=3:GH=D:G1=?:G2=Z:G3=@:G4=Y:GC=E:GL=4:GR=C:GU=A:GD=B:\\
	:cQ=\\E[k:cX=\\E[2;0k:cV=\\E[16;0k:cI=\\E[k:cR=\\E[16;20k:
.DE
