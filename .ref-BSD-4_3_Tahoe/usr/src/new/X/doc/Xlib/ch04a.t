.NH
Opening and Closing the Display
.XS
Initializing the Window System
.XE
.PP
The window system is a network service.
You must connect to the X server before you can perform any operations.
There is one X server for each hardware display.
This library interface presumes you are working with a single display at
one time, and
therefore has an idea of the current display you are using.
.FD
.IN "Definitions" "XOpenDisplay"
.IN "XOpenDisplay"
Display *XOpenDisplay (name)
	char *name;
.FN	
This function takes the \fIname\fP of the server, and opens a connection to the
server for the display hardware.
.IN "DISPLAY Environment Variable"
.IN "Environment" "DISPLAY"
If the display name string is NULL, it uses the environment variable DISPLAY
to determine which display and which communications domain to use.
The
display string or DISPLAY environment variable should be in the format
``hostname:number'', where ``hostname'' is the name of a machine, and
``number'' is the number of the display on that machine.
 For example, ``mit-athena:3'' would be display 3 on the machine
``mit-athena''.
.IN "Protocol" "TCP"
.IN "Protocol" "DECnet"
XOpenDisplay connects through TCP, Unix or DECnet streams to the server.
If the ``hostname'' is ``unix'', Unix domain IPC is used.
If there is only a single ``:'' between hostname and display number,
TCP streams are used.
If hostname and display number are seperated by a ``::'', DECnet streams
will be used.
Note that you must build all software for DECnet if you want to use DECnet.
A single X server will accept both TCP and DECnet connections if it has been
built for DECnet.
.PP
.IN "Definitions" "Display"
.IN "Data Structures" "Display"
If the call is successful, it returns a pointer to a  Display structure,
.IN "File" "<X/Xlib.h>"
which is defined in \fI<X/Xlib.h>\fP.
.PP
The procedure returns NULL on failure.
.PP
There are a number of useful macros and functions
which return information out of the
Display structure.
.IN "Unix System Call" "select"
Some programs may find it useful to perform select(2) on the file descriptor
.IN "Macro" "dpyno()"
returned using the \fIdpyno()\fP macro,
which returns the file descriptor of the connection.
This comes up most often in applications which drive more than one
display at a time.
The length of the input queue on the display connection is
.IN "Macro" "QLength()"
returned using the \fIQLength()\fP macro.
This may be useful to make sure you have processed all events
already read in before performing a select again.
.IN "Macro" "DisplayType()"
.IN "Macro" "DisplayPlanes()"
.IN "Macro" "DisplayCells()"
The \fIDisplayType()\fP, \fIDisplayPlanes()\fP, \fIDisplayCells()\fP and
.IN "Macro" "ProtocolVersion()"
\fIProtocolVersion()\fP macros
return the appropriate information out of the current display structure.
.IN "Window" "RootWindow"
.IN "RootWindow"
.IN "Macro" "RootWindow"
.PP
The \fIRootWindow\fP variable (actually a reference to the current display
structure) is often useful in other calls, as many subroutines take a
parent window as an argument.
.PP
.IN "Macro" "DisplayType()"
There may be different performance tradeoffs a client program
may wish to make depending on the display type.
The \fIDisplayType()\fP macro returns the type of the device, as
.IN "File" "<X/X.h>"
defined in <X/X.h>.
Its use is discouraged; programs should be prepared to work on different
displays of various sizes and characteristics.
.PP
.IN "Macro" "DisplayName()"
The \fIDisplayName()\fP macro returns the string that was passed to 
\fIXOpenDisplay\fP
when the current display was opened (or, if that was NULL, the value of
the DISPLAY environment variable when the current display was opened).
.IN "Unix System Call" "fork"
This is especially useful to applications which \fIfork()\fP, and want to open
a new connection to the same display from the child process.
.PP
.IN "BlackPixmap"
.IN "WhitePixmap"
.IN "Pixmap"
.IN "Macro" "BlackPixmap"
.IN "Macro" "WhitePixmap"
\fIXOpenDisplay\fP
automatically creates a solid black and a solid white pixmap,
suitable for use as a background or border tile.
You can refer to these
by using the \fIBlackPixmap\fP and \fIWhitePixmap\fP macros,
which refer to the
current display structure.
.PP
.IN "DisplayWidth"
.IN "DisplayHeight"
.IN "Definitions" "DisplayWidth"
.IN "Definitions" "DisplayHeight"
The size of the current display's root window can be determined with the
\fIDisplayWidth()\fP and \fIDisplayHeight()\fP functions,
which return integers describing the size of the screen in pixels.
.PP
.IN "Data Structures" "Display"
Other elements of the \fIDisplay\fP structure are private to
the X library and must not be used.
.FD
.IN "XSetDisplay"
.IN "Definitions" "XSetDisplay"
XSetDisplay(display)
	Display *display;
.FN
.PP
This procedure sets the current \fIdisplay\fP
connection which you are talking to.
It is used to switch between displays.
.FD
.IN "XCloseDisplay"
.IN "Definitions" "XCloseDisplay"
XCloseDisplay (display)
	Display *display;
.FN
.LP
\fIXCloseDisplay\fP closes the connection associated with the specified
\fIdisplay\fP.
All windows or other resources that the caller has created on this display
server are destroyed; they should never be referenced again.
Any output events
that have been buffered but not yet sent are discarded.
.PP
The effect of \fIXCloseDisplay\fP is automatically achieved if a process exits.
For this reason, most clients
will not need to call \fIXCloseDisplay\fP.
.PP
.IN "Resource ID"
If a client has created Window, Font, Bitmap, Pixmap, or Cursor
resource ID's with this display server, they must not be used after calling
.IN "XCloseDisplay"
XCloseDisplay.
.PP
.IN "Unix System Call" "fork"
A special note on \fIfork()\fP:  if a program has a connection to a display
server open and then
calls \fIfork()\fP you must take typical 
UNIX care when dealing with the
connection to the window system.
Do not forget to flush the output buffer before waiting on the child
process, and do not forget to make sure that you have processed all input
events before forking or exiting in the child, or your programs
may perform the operations twice.
