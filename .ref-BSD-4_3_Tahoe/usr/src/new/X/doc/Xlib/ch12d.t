.NH 2
Keyboard Operations
.PP
X does not predefine the keyboard to be ascii characters.
It is often useful to know that the ``a'' key just went down,
or possibly that it just went back up.
Client programs are sent keyboard events,
which contain a ``keycode''
which assigns a number to each physical key on the
keyboard.
(For historical reasons, the keycodes are a superset of the DEC LK201 keyboard
keycodes.)
Client programs which want to deal with
ascii text will then have to convert the keycode into ascii
explicitly.
The transformation of keycode to ascii is arbitrary,
and facilities are provided to aid people in customizing the keyboard
layout to whatever they want.
Remember that keyboards often differ dramatically,
so writing code that presumes the existance of a particular key
not on the main part of the keyboard is fraught with portability
problems.
It may also be difficult to get key up events on certain X implementations
due to hardware or software restrictions.
.PP
Keyboard events are normally sent to the smallest enclosing window
which is interested in that type of event underneath the
mouse's position.
It is also possible to assign the keyboard `input focus' to a specific
window.
When the input focus is attached to a window,
keyboard events will go to  the client which has selected
input on that window rather than the window under the mouse.
.PP
WARNING:
Some implementations cannot support up events.
You should think seriously before designing software that takes
advantage of up events if you are concerned about wide portability,
though there are some applications that can exploit up events
to provide superior user interfaces.
You should also be VERY careful when selecting which keys may be used
in such applications.
It may be impossible
to guarantee the existance of a set of keys on all keyboard with the probable
exception of a-z, spacebar, and carriage return.
.FD
.IN "XFocusKeyboard"
.IN "Definitions" "XFocusKeyboard"
XFocusKeyboard (w)
	Window w;
.FN
.IN "Input Focus"
\fIXFocusKeyboard\fP designates a window as the `input focus' window.  If
the window that would normally receive a \fIKeyPressed\fP or \fIKeyReleased\fP
event is not the focus window or one of its descendents, the event will
be sent to the focus window instead.  
The events will go to whatever
client has selected input on the focus window;  in general, this may be
.IN "XFocusKeyboard"
a client other than the one which has called \fIXFocusKeyboard\fP.  For
instance, a window manager may allow the user to designate an arbitrary
window as the keyboard focus.
.LP
The root window is the default focus window.
If the focus window is
closed, the closest existing ancestor inherits the input focus.
.FD
.IN "Definitions" "XLookupMapping"
.IN "XLookupMapping"
.IN "Keycode to Ascii Conversion"
.IN "Keyboard" "Customization"
char *XLookupMapping(event, nbytes)
	XKeyPressedEvent *event;
	int *nbytes;
.FN
.PP
This function is very useful for mapping down events to counted
character strings (an array of characters and the length; the null
character is legitimate in this use.)
It returns a pointer to a static counted character string which must not be
touched by a client, and the number of bytes in the string.
.PP
This mapping is normally stored in the user's home directory in the
.IN "File" "$HOME/.Xkeymap"
.IN "File" "/usr/lib/Xkeymap.txt"
file ".Xkeymap".
If this file is not present, \fIXLookupMapping\fP
falls back to a built in table.
If no text is defined for that key, \fInbytes\fP will be zero.
The \fI~/.Xkeymap\fP file is produced by the \fIkeycomp(1)\fP program, 
which reads
a text file of keyboard mappings.  The file \fI/usr/lib/Xkeymap.txt\fP contains
a set of standard keyboard mappings.
The function performs normal interpretation of `shift' bits (meta, shift,
shift lock, control).
The user should strncpy the result if needed to his own storage if the
data must be modified.
.IN "XUseKeymap"
If a different keymap file is desired, it may be set using
\fIXUseKeymap\fP.

.FD
.IN "Definitions" "XRebindCode"
.IN "XRebindCode"
.IN "Keyboard" "Customization"
XRebindCode(keycode, shiftbits, str, nbytes)
	unsigned int keycode;
	unsigned int shiftbits;
	char *str;
	int nbytes;
.FN
.PP
If you wish to rebind the keyboard, you can use this routine to change
(on a non-permanent basis) the binding of the keyboard.
Given a keycode,
the meta bits (or the \fIX*Masks\fP together to specify the bits
you wish to set), a string and the number of bytes in the string,
.IN "XLookupMapping"
subsequent calls to \fIXLookupMapping\fP will return the supplied string.
The string should be stored in static storage;
an automatic string may have been deallocated by the time it is needed.
.FD
.IN "Definitions" "XUseKeymap"
.IN "XUseKeymap"
.IN "Keyboard" "Customization"
Status XUseKeymap(keymap_file);
	char *keymap_file;
.FN
.PP
If you wish to use an alternate keymap file, you can use this
routine to change the file used.  Like XRebindKey, this only affects
the keymap within the current process.

The procedure returns a zero value if it fails (if it could not find
the keymap file named by \fIkeymap_file\fP, or if the file contains a
bad magic number), and a non-zero value if it succeeds.  If it fails,
the existing keymap is untouched.




