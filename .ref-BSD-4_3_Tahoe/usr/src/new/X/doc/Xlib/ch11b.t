.NH 2
User Preference Subroutines
.PP
.IN "Keyboard" "Bell Volume"
.IN "Keyboard" "Keyclick Volume"
.IN "Mouse" "Programming"
In this category are the user preference options of bell, keyclick,
mouse behavior, etc.
The default values for many of these functions are determined by
.IN "File" "/etc/ttys"
command line arguments to the X server, typically set in \fI/etc/ttys\fP.
Not all implementations will actually be able to control all of these
parameters.
.FD
.IN "Definitions" "XMouseControl"
.IN "XMouseControl"
.IN "Mouse" "Behavior"
XMouseControl (acceleration, threshold)
	int acceleration, threshold;
.FN
Defines how the mouse moves.
The \fIacceleration\fP is a multiplier for movement.
For example,
specifying 3 means the cursor moves three times as fast as the mouse.
Acceleration only takes effect if the mouse moves more than
threshold pixels at once,
and only applies to the amount beyond the \fIthreshold\fP.
.FD
.IN "Definitions" "XFeepControl"
.IN "XFeepControl"
XFeepControl (volume)
	int volume;	/* 0 to 7 */
.FN
Defines the base volume for \fIXFeep\fP request.
The volume is in the range
0 to 7, with 7 the loudest.
.FD
.IN "Definitions" "XFeep"
.IN "XFeep"
XFeep (volume)
	int volume;	/* -7 to 7 */
.FN	
\fIXFeep\fP rings a bell on the display. 
The
sound \fIvolume\fP is in the range -7 to 7
and is added to the base \fIvolume\fP defined by \fIXFeepControl\fP.
.FD
.IN "Definitions" "XKeyClickControl"
.IN "XKeyClickControl"
XKeyClickControl (volume)
	int volume;	/* 0 to 8 */
.FN
\fIXKeyClick\fP controls the volume of the `click' that the keyboard makes
when a key is pressed. 
0 is `off', 8 is the loudest \fIvolume\fP.
.FD
.IN "Keyboard" "Auto-Repeat"
.IN "Definitions" "XAutoRepeatOn"
.IN "Definitions" "XAutoRepeatOff"
.IN "XAutoRepeatOn"
.IN "XAutoRepeatOff"
XAutoRepeatOn ()

XAutoRepeatOff ()
.FN
\fIXAutoRepeatOn\fP turns the keyboard's auto-repeat on.
\fIXAutoRepeatOff\fP turns the keyboard's auto-repeat off.
.FD
.IN "Keyboard" "Lock Key"
.IN "Definitions" "XLockUpDown"
.IN "XLockUpDown"
.IN "Definitions" "XLockToggle"
.IN "XLockToggle"
XLockUpDown ()

XLockToggle ()
.FN
Some people hate the behavior of the shift lock key.
These subroutines change the mode of the Shift LOCK key on the keyboard.
When the keyboard is in \fILockUpDown\fP mode,
KeyPressed and KeyReleased events are sent as for any other key, and the
ShiftLockMask sent in events gives the current state of the key.
.IN "KeyPressed Events"
In \fILockToggle\fP mode, 
\fIKeyPressed\fP and \fIKeyReleased\fP events are never sent
for the Lock key,
and the state of the \fIShiftLockMask\fP sent in events is toggled on every
press of the LOCK key.
.FD
.IN "Screen Saver"
.IN "Definitions" "XScreenSaver"
.IN "XScreenSaver"
XScreenSaver (savetimeout, patterntimeout, video)
	int savetimeout, patterntimeout;	/* in minutes */
	int video;
.FN
If the server remains idle for the specified number of minutes,
the server will blank the screen, usually with a pattern that changes at the
specified rate.
The screen state will typically be restored when the next request or input
event occurs, unless the server was out of memory, in which case exposure
events to all mapped windows are generated.
If \fIvideo\fP is non-zero, the server will attempt to disable the video to
implement the screen saver, rather than change to the background
grey.
.FD
.IN "XGetDefault"
.IN "Definitions" "XGetDefault"
.IN "Default Options"
char *XGetDefault(program, name)
	char *program;	/* usually name of program */
	char *name;
.FN
The number of options that a program may need can be very large in the X
environment.
(Fonts of various sorts, colors of characters, mouse, background, text,
cursor, etc.)
If only command line options could be used to specify each of these,
things quickly become unmanageable as your tastes in windows will often
be drastically different from someone else's.
.PP
\fIXGetDefault\fP
makes it easy to find out what the user wants for his favorite
font, colors, etc.
Its use is highly encouraged.
.PP
\fIXGetDefault\fP will return NULL if no option of the specified name exists for
the program.
Defaults are read out of a file called
.IN "File" "$HOME/.Xdefaults"
\fI~/.Xdefaults\fP in the user's home directory.
See the X manual page
for details of its format.
.PP
The strings returned by \fIXGetDefault\fP are owned by Xlib and should not
be modified or freed by the client.
.FD
.IN "XParseGeometry"
.IN "Definitions" "XParseGeometry"
.IN "Window" "Initial Location"
int XParseGeometry (string, x, y, width, height)
	char *string;
	int *x, *y, *width, *height;	/* RETURN */
.FN
By convention,
X applications use a standard string to indicate window size and placement.
This subroutine makes it easy to conform to this standard.
It is not normally used by user programs, which typically use
.IN "XCreate"
.IN "XCreateTerm"
the \fIXCreate\fP or \fIXCreateTerm\fP subroutines to create the window.
This subroutine is used to parse strings of form 
\fI=<width>x<height>{+-}<xoffset>{+-}<yoffset>\fP where
width, height, xoffset and yoffset are returned in the \fIwidth\fP, 
\fIheight\fP, \fIx\fP and \fIy\fP arguments.
It returns a bitmask that indicates which of the four values were
actually found in the string, and whether the x and y values
are negative (remember, -0 is not equal +0 in this system!).
For each value found, the corresponding argument is updated;
for each value not found, the argument is left unchanged.
The bits are \fIXValue\fP, \fIYValue\fP, \fIWidthValue\fP, \fIHeightValue\fP,
\fIXNegative\fP, \fIYNegative\fP, and are defined in \fI<X/Xlib.h>\fP.
.IN "File" "<X/Xlib.h>
They will be set whenever one of the values are defined or signs
are set.
.FD
.IN "XGeometry"
.IN "Definitions" "Geometry"
.IN "Window" "Initial Location"
.IN "Window" "Computing Placement"
int XGeometry (position, default, bwidth, fwidth, fheight, xadd, yadd, x, y, width, height)
	char *position, default;	/* geometry specs */
	int bwidth;	/* border width */
	int fwidth, fheight;	/* size of units of width and height spec */
	int xadd, yadd;	/* any additional interior space */
	int *x, *y, *width, *height;	/* RETURN */
.FN
This routine does all the work required to determine the placement of
a window using the current format to position windows.
Given a fully qualified default geometry specification and a (possibly)
incompletely specified geometry specification, it will return a bitmask
value as defined above in the \fIXParseGeometry\fP call.
It is not normally used by user programs, which typically use
.IN "XCreate"
.IN "XCreateTerm"
the \fIXCreate\fP or \fIXCreateTerm\fP subroutines to create the window.
The position the window should be placed will be returned in the
x, y, width, and height arguments.
If either the function returns the \fIXValue\fP or \fIYValue\fP flag,
you should
place the window at the requested position.
The border width (\fIbwidth\fP),
size of the increments \fIfwidth\fP and \fIfheight\fP
(typically font width and height),
and any additional interior space (\fIxadd\fP and \fIyadd\fP)
are passed in to make it easy to compute the resulting size.
.FD
.IN "XReadBitmapFile"
.IN "Definitions" "Bitmap file format"
.IN "Definitions" "XReadBitmapFile"
Status XReadBitmapFile (filename, width, height, data, x_hot, y_hot)
	char *filename;
	int *width, *height;  /* RETURN; must be non-NULL */
	short **data;	      /* RETURN */
	int *x_hot, *y_hot;   /* RETURN; may be NULL */
.FN
\fIXReadBitmapFile\fP reads a file produced by the
.I bitmap(1)
program.   The format of that file is described in the man page for 
that program.
.PP
If the cannot be opened, \fIXReadBitmapFile\fP returns a Status of 0.  If the 
file can be opened but is not syntactically valid, the procedure returns
a negative Status.  If the file is readable and valid, it returns a Status of 1.
.PP
\fIXReadBitmapFile\fP assigns the bitmap's \fIheight\fP and \fIwidth\fP,
as read from the file,
to the caller's variables 
\fIwidth\fP
and 
\fIheight\fP.
It then allocates an appropriate
amount of storage, reads the bitmap data from the file, and assigns to the
caller's variable 
.I data.
The caller must free 
.I data
when he is done with it.
.PP
If
.I x_hot
and 
.I y_hot
are non-NULL, then \fIXReadBitmapFile\fP will set 
.I *x_hot
and 
.I *y_hot
to the value of the hot spot as defined in the file.  If no hot spot is
defined, \fIXReadBitmapFile\fP will set
.I *x_hot
and 
.I *y_hot
to -1.

