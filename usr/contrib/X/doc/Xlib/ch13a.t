.NH
Error Handling
.XS
Error Handling
.XE
.PP
.IN "Error Handlers"
There are two default error handlers in the library, one to handle
typically fatal conditions (for example, the connection to a display
server
dying due to machine crash), and one to handle error events from X.
These error handlers can be changed to user supplied routines if you
prefer your own error handling, and can be changed as often as you like.
If either of these routines are passed a NULL pointer, it will
reinvoke the default handler.
The default action of the  supplied routine is to exit.
.PP
.FD
.IN "Definitions" "XIOErrorHandler"
.IN "XIOErrorHandler"
XIOErrorHandler( handler )
	int handler(Display *);
.FN
.PP
The program's
supplied error handler will be called by Xlib if any sort of system call error
occurs, e.g. the connection to the server was lost.
This is assumed to
be a fatal condition, i.e., the called routine should not return.
If the IO error handler
does return, the client process will exit.
.PP
.FD
.IN "Definitions" "XErrorHandler"
.IN "XErrorHandler"
XErrorHandler( handler )
	int handler(Display *, XErrorEvent *)
.FN
The program's
supplied error hander will be called by Xlib whenever an \fIXError\fP event is
received.
This is not assumed to be a fatal condition, i.e., it is
acceptable for this procedure to return.
However, the error handler should NOT
perform any operations (directly or indirectly) on the Display.
The
fields of the \fIXErrorEvent\fP passed to \fIXError\fP should be interpreted as follows:
.sp
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _XErrorEvent {
	long pad;
	long serial;	/* serial number of failed request */
	char error_code;	/* error code of failed request */
	char request_code;	/* request code of failed request */
	char func;	/* function field of failed request */
	char pad_b7;
	Window window;	/* Window of failed request */
	long pad_l3;
	long pad_l4;
} XErrorEvent;
.DE
.IN "Data Structures" "XErrorEvent"
.IN "Serial Number"
.sp
The serial number is the number of requests sent over the network connection
since it was opened, starting from one; it is the number that was the
value of \fIdpy->request\fP immediately after the failing call was made.  The
request code is a protocol representation of the name of the procedure
.IN "File" "<X/X.h>"
that failed; these are defined in \fI<X/X.h>\fP.  The error code is one of
the following, also defined in \fI<X/X.h>\fP:
.IN "Error Numbers"
.LP
.TS
center,box;
lcl.
BadRequest	1	bad request code
BadValue	2	int parameter out of range
BadWindow	3	parameter not a Window
BadPixmap	4	parameter not a Pixmap
BadBitmap	5	parameter not a Bitmap
BadCursor	6	parameter not a Cursor
BadFont	7	parameter not a Font
BadMatch	8	parameter mismatch
BadTile	9	Pixmap shape invalid for tiling
BadGrab	10	mouse/button already grabbed
BadAccess	11	access control violation
BadAlloc	12	insufficient resources
BadColor	13	no such color
.TE
It is recommended that \fIXError\fP use the following procedure for obtaining
textual descriptions of errors:
.FD
.IN "Definitions" "XErrDescrip"
.IN "XErrDescrip"
char *XErrDescrip (code)
	int code;
.FN
Returns a null-terminated string describing the specified error code.
The string is static in Xlib and should not be modified or freed.
