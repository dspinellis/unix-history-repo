.NH
Text Operations
.XS
Text Operations
.XE
.PP
.IN "Definitions" "Font"
.IN "Fonts"
All of the subroutines in this section make use of fonts.
A font is a
graphical description of a set of characters, used to increase efficiency
whenever a set of small, similar-sized patterns are repeatedly used.
.PP
While most fonts contain descriptions of alphanumeric characters, fonts
and the subroutines in this section are not restricted to that purpose
.IN "XText"
.IN "XTextWidth"
(despite suggestive subroutine names such as \fIXText\fP and \fIXTextWidth\fP).
.FD
.IN "Definitions" "XText"
.IN "XText"
.IN "XTextPad"
.IN "Definitions" "XTextPad"
XText (w, x, y, str, len, font, foreground, background)

XTextPad (w, x, y, str, len, font, charpad, spacepad, foreground, background, func, planes)
	Window w;
	int x, y;
	int len;	/* string length */
	char *str;	/* NOT necessarily null-terminated */
	Font font;
	int charpad;
	int spacepad;
	int foreground, background;
	int func;	/* display function */
	int planes;
.FN
\fIXTextPad\fP draws text into a window,
using the specified \fIfont\fP and display function
\fIfunc\fP.
It modifies the specified planes of the display memory,
painting in the foreground
color.
The number of characters to be drawn must be specified in the
\fIlen\fP parameter;
\fIXText\fP does NOT assume that \fIstr\fP is null-terminated.
.PP
The \fIx\fP and \fIy\fP
coordinates represent the upper left corner of the first
character.
.PP
For each character drawn, a rectangular bitmap is transferred onto the
display.
\fIBackground\fP and \fIforeground\fP pixels will be set to the specified
pixel values (colors). 
.PP
.IN "Character Padding"
.IN "Space Padding"
.IN "Padding" "Space"
.IN "Padding" "Character"
The character padding \fIcharpad\fP defines how much space will be
left between each character.
The space padding \fIspacepad\fP defines how much additional padding
will occur when a space character is painted.
Padded pixels are NOT modified.
All pixels in a character cell are modified to either the \fIforeground\fP or
\fIbackground\fP pixel value.
.PP
.IN "XText"
\fIXText\fP defaults the function to \fIGXcopy\fP, modifies all planes,
and does no padding.
.FD
.IN "Definitions" "XTextMask"
.IN "XTextMask"
.IN "Definitions" "XTextMaskPad"
.IN "XTextMaskPad"
XTextMask (w, x, y, str, len, font, foreground )

XTextMaskPad (w, x, y, str, len, font, charpad, spacepad, foreground, func, planes)
	Window w;
	int x, y;
	int len;	/* string length */
	char *str;	/* NOT necessarily null-terminated */
	Font font;
	int charpad, spacepad;
	int foreground;
	int func;	/* display function */
	int planes;	/* plane masks */
.FN	
\fIXTextMaskPad\fP draws text into a window,
using the specified \fIfont\fP and display function
\fIfunc\fP.
It modifies the specified \fIplanes\fP of the display,
only modifying bits specified by the font.
.IN "Mask Font"
(The font bits are used as a ``mask'', so only bits set to one in the font
cause pixels to be modified on the display).
The number of characters to be drawn must be specified in the
\fIlen\fP parameter;
these calls do NOT assume that \fIstr\fP is null-terminated.
.PP
The \fIx\fP and \fIy\fP coordinates represent the upper left corner of the first
character.
.PP
\fICharpad\fP and \fIspacepad\fP can be used for intercharacter and space padding.
Padded pixels are NOT modified.
.PP 
.IN XTextMask
\fIXTextMask\fP defaults the function to \fIGXcopy\fP, modifies all planes,
and does not do padding.
