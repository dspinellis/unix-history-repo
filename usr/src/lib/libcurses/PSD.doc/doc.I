.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)doc.I	5.1 (Berkeley) %G%
.\"
.Ds
.Fd addch ch \*m
char	ch;
.sp .5
.Fd waddch win\*,ch
WINDOW	*win;
char	ch;
.De
Add the character
.Vn ch
on the window
at the current \*y.
If the character is a newline
(\'\en\')
the line will be cleared to the end,
and the current \*y will be changed to the
beginning off the next line
if newline mapping is on,
or to the next line at the same x co-ordinate
if it is off.
A return
(\'\er\')
will move to the beginning of the line on the window.
Tabs
(\'\et\')
will be expanded into spaces
in the normal tabstop positions of
every eight characters.
\*(Es
.Ds
.Fd addstr str \*m
char	*str;
.sp .5
.Fd waddstr win\*,str
WINDOW	*win;
char	*str;
.De
Add the string pointed to by
.Vn str
on the window at the current \*y.
\*(Es
In this case, it will put on as much as it can.
.Ds
.Fd box win\*,vert\*,hor
WINDOW	*win;
char	vert\*,hor;
.De
.Pp
Draws a box around the window using
.Vn vert
as the character for drawing the vertical sides, and
.Vn hor
for drawing the horizontal lines.
If scrolling is not allowed,
and the window encompasses the lower right-hand corner of the terminal,
the corners are left blank to avoid a scroll.
.Ds
.Fd clear "" \*m
.sp .5
.Fd wclear win
WINDOW	*win;
.De
Resets the entire window to blanks.
If
.Vn win
is a screen,
this sets the clear flag,
which will cause a clear-screen sequence to be sent
on the next
.Fn refresh
call.
This also moves the current \*y
to (0\*,0).
.Ds
.Fd clearok scr\*,boolf \*m
WINDOW	*scr;
bool	boolf;
.De
Sets the clear flag for the screen
.Vn scr .
If
.Vn boolf
is TRUE,
this will force a clear-screen to be printed on the next
.Fn refresh ,
or stop it from doing so if
.Vn boolf
is FALSE.
This only works on screens,
and,
unlike
.Fn clear ,
does not alter the contents of the screen.
If
.Vn scr
is
.Vn curscr ,
the next
.Fn refresh
call will cause a clear-screen,
even if the window passed to
.Fn refresh
is not a screen.
.Ds
.Fd clrtobot "" \*m
.sp .5
.Fd wclrtobot win
WINDOW	*win;
.De
Wipes the window clear from the current \*y to the bottom.
This does not force a clear-screen sequence on the next refresh
under any circumstances.
\*(Nm
.Ds
.Fd clrtoeol "" \*m
.sp .5
.Fd wclrtoeol win
WINDOW	*win;
.De
Wipes the window clear from the current \*y to the end of the line.
\*(Nm
.Ds
.Fd delch
.sp .5
.Fd wdelch win
WINDOW	*win;
.De
Delete the character at the current \*y.
Each character after it on the line shifts to the left,
and the last character becomes blank.
.Ds
.Fd deleteln
.sp .5
.Fd wdeleteln win
WINDOW	*win;
.De
Delete the current line.
Every line below the current one will move up,
and the bottom line will become blank.
The current \*y will remain unchanged.
.Ds
.Fd erase "" \*m
.sp .5
.Fd werase win
WINDOW	*win;
.De
Erases the window to blanks without setting the clear flag.
This is analagous to
.Fn clear ,
except that it never causes a clear-screen sequence to be generated
on a
.Fn refresh .
\*(Nm
.Ds
.Fd insch c
char	c;
.sp .5
.Fd winsch win\*,c
WINDOW	*win;
char	c;
.De
Insert
.Vn c
at the current \*y
Each character after it shifts to the right,
and the last character disappears.
\*(Es
.Ds
.Fd insertln
.sp .5
.Fd winsertln win
WINDOW	*win;
.De
Insert a line above the current one.
Every line below the current line
will be shifted down,
and the bottom line will disappear.
The current line will become blank,
and the current \*y will remain unchanged.
\*(Es
.Ds
.Fd move y\*,x \*m
int	y\*,x;
.sp .5
.Fd wmove win\*,y\*,x
WINDOW	*win;
int	y\*,x;
.De
Change the current \*y of the window to
.Vn y\*,x ). (
\*(Es
.Ds
.Fd overlay win1\*,win2
WINDOW	*win1\*,*win2;
.De
Overlay
.Vn win1
on
.Vn win2 .
The contents of
.Vn win1 ,
insofar as they fit,
are placed on
.Vn win2
at their starting \*y.
This is done non-destructively,
i.e., blanks on
.Vn win1
leave the contents of the space on
.Vn win2
untouched.
.Ds
.Fd overwrite win1\*,win2
WINDOW	*win1\*,*win2;
.De
Overwrite
.Vn win1
on
.Vn win2 .
The contents of
.Vn win1 ,
insofar as they fit,
are placed on
.Vn win2
at their starting \*y.
This is done destructively,
i.e., blanks on
.Vn win1
become blank on
.Vn win2 .
.Ds
.Fd printw fmt\*,arg1\*,arg2\*,...
char	*fmt;
.sp .5
.Fd wprintw win\*,fmt\*,arg1\*,arg2\*,...
WINDOW	*win;
char	*fmt;
.De
Performs a
.Fn printf
on the window starting at the current \*y.
It uses
.Fn addstr
to add the string on the window.
It is often advisable to use the field width options of
.Fn printf
to avoid leaving things on the window from earlier calls.
\*(Es
.Ds
.Fd refresh "" \*m
.sp .5
.Fd wrefresh win
WINDOW	*win;
.De
Synchronize the terminal screen with the desired window.
If the window is not a screen,
only that part covered by it is updated.
\*(Es
In this case, it will update whatever it can
without causing the scroll.
.Ds
.Fd standout "" \*m
.sp .5
.Fd wstandout win
WINDOW	*win;
.sp .5
.Fd standend "" \*m
.sp .5
.Fd wstandend win
WINDOW	*win;
.De
Start and stop putting characters onto
.i win
in standout mode.
.Fn standout
causes any characters added to the window
to be put in standout mode on the terminal
(if it has that capability).
.Fn standend
stops this.
The sequences
.Vn SO
and
.Vn SE
(or
.Vn US
and
.Vn UE
if they are not defined)
are used (see Appendix A).
