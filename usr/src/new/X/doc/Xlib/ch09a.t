.NH
Fonts and Information about Fonts
.XS
Fonts and Information about Fonts
.XE
.PP
.IN "Font Loading"
WARNING:  This section WILL change in major ways soon,
to provide a more sophisticated view of fonts,
and lift limitations in the current font format.
Please plan accordingly.
.PP
The X server loads fonts whenever a program requests a new font.
Fonts are unloaded when the last program using the font exits or
closes it.
There is never more than one copy of a font stored in the server at
one time.
.PP
There are several levels one can deal with fonts.
.IN "XOpenFont"
.IN "XCloseFont"
You can use the general font routines \fIXOpenFont\fP and \fIXCloseFont\fP,
or only get the information you want explicitly.
.IN "FontInfo"
.FD
.IN "Definitions" "XOpenFont"
.IN "XOpenFont"
FontInfo *XOpenFont(name)
	char *name;
.FN
.IN "XGetFont"
.IN "XQueryFont"
.IN "XFontWidths"
This function does a \fIXGetFont\fP, 
\fIXQueryFont\fP and \fIXFontWidths\fP in one operation,
creating an instance of the font structure.
It allocates the memory to store the font information in.
It returns NULL if it could not succeed.
.FD
.IN "Definitions" "XCloseFont"
.IN "XCloseFont"
XCloseFont(info)
	FontInfo *info;
.FN
This function closes off any use of a font, and deallocates the storage
associated with the \fIFontInfo\fP structure.
.IN "XOpenFont"
WARNING: it is a gross error to close a font not opened with \fIXOpenFont\fP,
since you may corrupt the memory pool.
.FD
.IN "Definitions" "XGetFont"
.IN "XGetFont"
Font XGetFont (name)
	char *name;	/* null-terminated string */
.FN
\fIXGetFont\fP loads a font of the specified \fIname\fP.
A font id is returned, or 0 if it could not succeed.
.IN "XFreeFont"
The client should call \fIXFreeFont\fP when it is no longer needed.
.FD
.IN "Definitions" "XQueryFont"
.IN "XQueryFont"
Status XQueryFont (font, info)
	Font font;
	FontInfo *info;	/* RETURN */
.FN
\fIXQueryFont\fP gets various facts about a font.  It fills in the
client-passed \fIFontInfo\fP, which is defined as follows:
.sp
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _FontInfo {
	Font id;	/* font id for this font */
	short height;	/* constant for all characters in font */
	short width;	/* "average" width of characters in font */
	short baseline;	/* baseline of characters */
	short fixedwidth;	/* 0 or 1 */
	unsigned char firstchar, lastchar;	/* first & last characters in font */
	short *widths;	/* pointer to font width array */
} FontInfo;
.DE
.IN "Data Structures" "FontInfo"
.IN "FontInfo"
The \fIbaseline\fP specifies where in pixels from the bottom of the font the
characters without descenders begin.
.PP
A font is fixed width if all characters in the range of legal characters
are the same width.
.PP
It does NOT get the width array, but the space is in the
data structure for a pointer to the array.
.FD
.IN "Definitions" "XFreeFont"
.IN "XFreeFont"
XFreeFont (font)
	Font font;
.FN
\fIXFreeFont\fP tells the server that this font is no longer needed. 
The font may be unloaded on the server if this is the last
reference to the font.
In any case, the
font should never again be referenced.
.FD
.IN "Definitions" "XCharWidths"
.IN "XCharWidths"
Status XCharWidths (chars, len, font, widths)
	char *chars;	/* NOT necessarily null-terminated */
	int len;	/* number of characters */
	Font font;
	short *widths;	/* RETURN widths [0..len-1] */
.FN
\fIXCharWidths\fP determines the width, in the specified font, of each
character in a string.
For each element of the character array, the width of
that character is stored in the corresponding element of the \fIwidths\fP
array (i.e. widths[i] is set to the width of the character chars[i].)
.FD
.IN "Definitions" "XFontWidths"
.IN "XFontWidths"
short *XFontWidths (font)
	Font font;
.FN
\fIXFontWidths\fP allocates and returns a pointer to an array containing the
width of every character defined in the font.   
It is only possible to get the widths of a variable width font. 
If \fIXFontWidths\fP returns NULL, an error has occurred and no
array is allocated.
The client must free this array when he no longer
needs it.
.PP
.IN "XFontWidths"
.IN "XQueryFont"
\fIXFontWidths\fP should be used in conjunction with \fIXQueryFont\fP,
which returns
(among other data) the font's \fIfirstchar\fP and \fIlastchar\fP.
The length of the array
.IN "XFontWidths"
returned by \fIXFontWidths\fP will always be equal to (lastchar-firstchar+1).
In the array, widths[i] will be set to the width of character (firstchar+i).
.FD
.IN "XQueryWidth"
.IN "Definitions" "XQueryFont"
int XQueryWidth(str, font)
	char *str;	/* null-terminated string */
	Font font;
.FN
\fIXQueryWidth\fP returns the width in pixels of a null-terminated string in
the specified font.
It queries the server for the width computation.
.FD
.IN "XQueryWidth"
.IN "Definitions" "XQueryFont"
int XStringWidth(string, info, charpad, spacepad)
	char *str;
	FontInfo *info;
	int charpad, spacepad;
.FN
This function computes the width of the string given a complete
.IN "FontInfo"
\fIFontInfo\fP structure.
\fICharpad\fP and \fIspacepad\fP are added to the width on each
character and space defined in the string.
It does not reference the window system server, as the information is
all available locally in this case.
