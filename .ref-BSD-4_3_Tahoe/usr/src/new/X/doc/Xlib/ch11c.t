.NH 2
Cut Buffer Operations
.PP
.IN "Cut Buffers"
.IN "Paste Buffers"
X provides areas of memory in which bytes can be stored for implementing
cut and paste between windows.
It is up to applications to agree what the representation of the
data in the buffers are.
The data is most often ascii text.
Eight such buffers are provided, which can be accessed as a ring,
or as explicit buffers, numbered 0 through 7.
.FD
.IN "Definitions" "XStoreBytes"
.IN "XStoreBytes"
XStoreBytes (bytes, length)
	char *bytes;	/* NOT necessarily ascii or null-terminated */
	int length;	/* number of bytes */
.FN
\fIXStoreBytes\fP stores an arbitrary string of \fIbytes\fP
in cut buffer number 0.
The cut buffer's contents may be retrieved later by
.IN "XFetchBytes"
any client calling \fIXFetchBytes\fP.
.PP
The number of bytes to be stored is given in the parameter \fIlength\fP.
Note that the cut buffer's contents need not be text, so null bytes 
are not special.
.FD
.IN "Definitions" "XFetchBytes"
.IN "XFetchBytes"
char *XFetchBytes (nbytes)
	int *nbytes;	/* RETURN */
.FN
\fIXFetchBytes\fP retrieves the contents of cut buffer 0.
If the buffer contains data,
it returns the number of bytes in the \fInbytes\fP argument,
otherwise it returns NULL and sets \fInbytes\fP to 0.
The appropriate amount of storage is ``malloc'ed'' and the pointer returned;
the client must free this storage when finished with it.
.PP
Note that the cut buffer does not necessarily contain text, so it may
contain embedded null bytes and may not terminate with a null byte.
.FD
.IN "Definitions" "XRotateBuffers"
.IN "XRotateBuffers"
XRotateBuffers (n)
	int n;
.FN
Rotates the cut buffers by n.
Buffer 0 becomes buffer n, buffer 1 becomes n+1 mod 8, and so on.
This cut buffer numbering is global to the display.
.FD
.IN "Definitions" "XStoreBuffer"
.IN "XStoreBuffer"
XStoreBuffer (bytes, nbytes, buffer)
	char *bytes;	/* NOT necessarily ascii or null-terminated */
	int nbytes;	/* number of bytes */
	int buffer;
.FN
\fIXStoreBuffer\fP is just like \fIXStoreBytes\fP,
except that it stores in the
specified \fIbuffer\fP instead of in buffer 0.
.FD
.IN "Definitions" "XFetchBuffer"
.IN "XFetchBuffer"
char *XFetchBuffer (nbytes, buffer)
	int *nbytes;	/* RETURN */
	int buffer;
.FN
\fIXFetchBuffer\fP is just like \fIXFetchBytes\fP,
except that it fetches the contents
of the specified \fIbuffer\fP instead of buffer 0. 
