/* $Header: Xtext.h,v 10.4 86/04/23 12:04:49 jg Rel $ */
/* Include file for X text window package.  An application using this
   package modifies any of the fields at its own risk! */

#include <X/Xlib.h>

typedef struct _TextWindow {
	struct _TextWindow *next; /* for linked list of TextWindows */
	Window w;		/* Window to use */
	FontInfo *font;		/* Font to use for text */
	int fgpixel;		/* Pixel value of foreground */
	int bgpixel;		/* Pixel value of background */
	short num_lines;	/* Number of lines in the window */
	short num_chars;	/* The length of each line */
	short mapped;		/* Whether or not the window is mapped */
	short height;		/* Height of window in pixels */
	short width;		/* Width of window in pixels */
	short first_line;	/* The index of the first line */
	char **lines;		/* Ptr to array of text lines */
	short *line_length;	/* Ptr to array of line lengths (in pixels) */
	short *line_chars;	/* Ptr to array of line lengths in chars */
	short last_line;	/* Which line is the last */
	short last_char;	/* Length of the last line */
	short next_x;		/* X-coord for next character */
	short next_y;		/* Y-coord for next character */
	unsigned int eventmask;	/* List of events we're interested in */
	char *scroll_history;	/* Ptr to list of scroll amounts */
	short scroll_count;	/* Number of outstanding scrolls */
	short scroll_start;	/* Where in the history the history starts */
	short old_scrolls;	/* Number of ignorable outstanting scrolls */
	short fastscroll;	/* Whether or not to use fast scrolling */
} TextWindow; 

TextWindow *TextCreate();
