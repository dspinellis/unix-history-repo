/* $Header: Xtextlib.c,v 10.6 86/04/23 12:03:28 jg Rel $ */
/* Library of routines for creating a simple text output window.
 *
 * Routines in the library are:
 *
 *	TextCreate		Creates a new instance of a text window
 *	TextDestroy		Destroys the window
 *	TextClear		Clears a text window
 *	TextRedisplay		Redisplays one or all windows
 *	TextEvent		Handles exposure and unmapping events
 *	TextFlush		Flush and handle all outstanding events for
 *				one or all TextWindows.
 *	TextPutString		Displays a string in a text window
 *	TextPutChar		Displays a character in a text window
 *	TextPrintf		Does a printf in a text window
 *
 * Most of these routines pass around a pointer to a TextWindow data structure:
 *
 * typedef struct _TextWindow {
 *	Window w;		Window to use
 *	FontInfo *font;		Font to use for text
 *	short num_lines;	Number of lines in the window
 *	short num_chars;	The length of each line
 *	short mapped;		Whether or not the window is mapped
 *	short height;		Height of window in pixels
 *	short width;		Width of window in pixels
 *	short first_line;	The index of the first line
 *	char **lines;		Ptr to array of text lines
 *	short *line_length;	Ptr to array of line lengths (in pixels)
 *	short *line_chars;	Ptr to array of line lengths in chars
 *	short last_line;	Which line is the last
 *	short last_char;	Length of the last line
 *	short next_x;		X-coord for next character
 *	short next_y;		Y-coord for next character
 *	unsigned int eventmask;	List of events we're interested in
 *	char *scroll_history;	Ptr to list of scroll amounts
 *	short scroll_count;	Number of outstanding scrolls
 *	short scroll_start;	Where in the history the history starts
 *	short old_scrolls;	Number of ignorable outstanding scrolls
 *	short fastscroll;	Whether or not to use fast scrolling
 * } TextWindow; 
 *
 * Applications should not modify anything in this data structure, obviously!
 * They may, however, have reason to get information out of it. (Such as the
 * window id for mapping).
 *
 * Information about the first line of the window is stored in the array
 * entries subscripted by [first_line]; the arrays wrap back up at the end.
 * Last_char should always be the same as line_chars[last_line]. 
 * Similarly, next_x should always be the same as line_length[last_line];
 *
 * The only complicated thing about these procedures is the way they keep
 * track of scrolling.  When a scroll is done, X sends ExposeRegions for
 * every region that needs to be patched up and then an ExposeCopy event.
 * The ExposeCopy comes even if there were no regions.  The only problem
 * is that more scrolls may have been done in the meantime.  So we keep a
 * history of how much cumulative scrolling has been done in the
 * scroll_history list.  scroll_start tells which one to start with, and
 * scroll_count tells how many there are (they wrap around).  The list is
 * num_lines long since anything that's scrolled away longer ago than that
 * has scrolled off the screen.  The old_scrolls field gets set whenever the
 * screen is fully updated for some reason or other; it means that that
 * many ExposeCopy events can be completely ignored since the screen has
 * been fully updated.
 */

#include <stdio.h>
#include "Xtext.h"

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

/* Define the width of the left margin */

#define mar_width 2

char *calloc(), *malloc(), *realloc();

/* The following variable is sometimes set by TextPutString to temporarily
   disable screen updating. */

static int dont_update = FALSE;

/*
 * the following variable is the head pointer to a list of TextWindows
 */
static TextWindow *head=NULL;


/*
 * add a new TextWindow to the list of TextWindows created for this
 * process.
 */
static AddTextWindow(t)
	TextWindow *t;
{	
	TextWindow *p;

	t->next = NULL;
	if (head) {
		p = head;
		while (p->next) p = p->next;
		p->next = t;
	} else {
		head = t;
	}
}


/*
 * delete a TextWindow from the list of TextWindows created for this
 * process.
 */
static DelTextWindow(t)
	TextWindow *t;
{
	TextWindow *p=head;

	if (!p) return; 	/* shouldn't happen! no list. */
	if (p==t) {
		head = t->next;
	} else {
		while (p->next && (t != p->next)) p = p->next;
		if (t != p->next) return;  /* shouldn't happen! not found */
		p->next = t->next;
	}
}

/* TextCreate creates a new window which will use the
 * specified font.  The window is height lines high and width
 * characters wide.  Note that since a variable-width font may be
 * used, the width is calculated using the average width of the font.
 * Colors are used as specified.
 */

TextWindow *TextCreate (width, height, x, y, parent, fontname,
		bwidth, fgpixel, bgpixel, bordercolor, fastscroll)
	int height, width, x, y, bwidth, fastscroll;
	Window parent;
	char *fontname;
	int fgpixel, bgpixel;
	Pixmap bordercolor;
{
	register TextWindow *t;
	register int i;
	register FontInfo *f;
	Window XCreateWindow();
	Pixmap bgpixmap;

	if ((t = (TextWindow *) malloc(sizeof(TextWindow))) ==
		NULL) return NULL;

	AddTextWindow(t); 		/* add to list of TextWindows */

	if ((f = t->font = XOpenFont(fontname)) == NULL) {
	    TextDestroy(t);
	    return NULL;
	}

	t->fgpixel = fgpixel;
	t->bgpixel = bgpixel;

	if ((bgpixmap = XMakeTile(bgpixel)) == NULL) {
	    TextDestroy(t);
	    return NULL;
	}

	t->width = width * f->width + mar_width;
	t->height = height * f->height;

	t->w = XCreateWindow (parent, x, y, t->width, t->height,
		bwidth, bordercolor, bgpixmap);
	if (t->w == NULL) {
	    TextDestroy(t);
	    XFreePixmap(bgpixmap);
	    return NULL;
	}

	XFreePixmap(bgpixmap);

	t->eventmask = ExposeRegion | ExposeWindow | ExposeCopy | UnmapWindow;
	/* (ExposeRegion automatically selects ExposeWindow) */

	XSelectInput (t->w, t->eventmask);

	XSetResizeHint (t->w, mar_width, 0, f->width, f->height);
	t->fastscroll = fastscroll;
	t->mapped = FALSE;
	t->num_lines = height;
	t->num_chars = width;

	t->first_line = 0;

	if ((t->lines = (char **)
		calloc (height, sizeof (char *))) == NULL) {
	    TextDestroy(t);
	    return NULL;
	}

	if ((t->line_length = (short *)
		calloc (height, sizeof (short))) == NULL) {
	    TextDestroy(t);
	    return NULL;
	}

	if ((t->line_chars = (short *)
		calloc (height, sizeof (short))) == NULL) {
	    TextDestroy(t);
	    return NULL;
	}

	for (i = 0; i < height; i++) {
	    if ((t->lines[i] = (char *)
		    calloc (width+1, sizeof (char))) == NULL) {
		TextDestroy(t);
		return NULL;
	    }
	}

	if ((t->scroll_history = calloc(height, sizeof (char))) == NULL) {
	    TextDestroy(t);
	    return NULL;
	}

	t->scroll_count = t->scroll_start = t->old_scrolls = 0;
	TextClear(t);
	return t;
}

/* Free all the storage associated with a textwindow */

TextDestroy(t)
	register TextWindow *t;
{
	register int i;

	/* Free things in the order we allocated them.  If something doesn't
	   exist, don't free it!) */

	if (t->font) {
	    if (t->font->fixedwidth == 0) free((char *)t->font->widths);
	    free((char *)t->font);
	}

	if (t->w) XDestroyWindow(t->w);

	if (t->lines) {
	    for (i = 0; i < t->num_lines; i++) {
		if (t->lines[i]) free((char *)t->lines[i]);
	    }
	    free((char *)t->lines);
	}

	if (t->line_length) free ((char *)t->line_length);
	if (t->line_chars) free ((char *)t->line_chars);
	if (t->scroll_history) free (t->scroll_history);


	DelTextWindow(t);	/* remove from list of TextWindows */

	/* And finally the data structure itself! */
	free ((char *)t);
}

/* Clear out a text window and redisplay */

TextClear(t)
	register TextWindow *t;
{
	register int i;

	for (i = 0; i < t->num_lines; i++) {
	    t->lines[i][0] = '\0';
	    t->line_chars[i] = 0;
	    t->line_length[i] = mar_width;	/* Allow a left margin */
	}
	t->last_line = 0;
	t->last_char = 0;
	t->next_x = mar_width;		/* Allow a left margin */
	t->next_y = 0;
	t->first_line = 0;

	TextRedisplay(t);
}

/* Redisplays a text window */

TextRedisplay (t)
	register TextWindow *t;
{
	if (t==NULL) {
		for (t = head; t!=NULL; t = t->next) {
			TextRedisplay(t);
		}
		return;
	}
	if (!t->mapped) return;

	/* Clear the border area */

	XPixSet(t->w, 0, 0, mar_width, t->height, t->bgpixel);

	Redisplay_lines(t, 0, t->num_lines - 1);

	/* Any outstanding copies from scrolls can now be ignored */

	t->old_scrolls = t->scroll_count;
	t->scroll_count = t->scroll_start = 0;
}

Redisplay_lines(t, start, finish)
	register TextWindow *t;
	int start, finish;
{
	register int i, j, y, height = t->font->height, x, width;

	if (finish < 0) return;
	if (start < 0) start = 0;

	y = start * height;
	j = start + t->first_line;

	for (i = start; i <= finish; i++) {
	    if (j >= t->num_lines) j = 0;

	    if (t->line_chars[j]) {
		XText (t->w, mar_width, y, t->lines[j], t->line_chars[j],
			t->font->id, t->fgpixel, t->bgpixel);
	    }

	    x = t->line_length[j];
	    width = t->width - x;

	    if (width > 0) XPixSet(t->w, x, y, width, height, t->bgpixel);
	    y += height;
	    j++;
	}
}

/* Handles an event.  If it's not an event it knows how to deal with,
   returns TRUE, otherwise FALSE. */

int TextEvent(t, e)
	register TextWindow *t;
	XEvent *e;
{
	XExposeEvent *ee = (XExposeEvent *) e;
	int offset;

	switch (e->type) {
	    case ExposeWindow:
		if (ee->height != t->height || ee->width != t->width) {
		    Change_text_window_size(t, ee->height / t->font->height,
			    ee->width / t->font->width);
		}
		t->mapped = TRUE;
		TextRedisplay(t);
		break;

	    case ExposeRegion:
		/* If there have been more scrolls than there are lines,
		   this stuff has already scrolled off! */

		if (t->scroll_count > t->num_lines) return FALSE;

		/* If this is for an old scroll, ignore it */

		if (ee->detail == ExposeCopy && t->old_scrolls) return FALSE;

		if (t->scroll_count > 0) {
		    offset = t->scroll_history[t->scroll_start];
		} else offset = 0;
		Redisplay_lines(t, ee->y / t->font->height - offset,
			(ee->y + ee->height - 1) / t->font->height - offset);
		break;

	    case UnmapWindow:
		t->mapped = FALSE;
		break;

	    case ExposeCopy:	/* We've finished the events for one scroll */
		/* If there are old scrolls, just decrement the count and
		   return */

		if (t->old_scrolls) {
		    t->old_scrolls--;
		    return FALSE;
		}
		t->scroll_count--;
		if (t->scroll_count < t->num_lines) {
		    t->scroll_start++;
		    if (t->scroll_start >= t->num_lines) t->scroll_start = 0;
		}
		break;

	    default:
		return TRUE;
	}
	return FALSE;
}

/*
 * Flush all outstanding events for all existent TextWindows.
 */
TextFlush(t)
	TextWindow *t;
{
	XEvent e;

	if (t==NULL) {
		for (t=head; t!=NULL; t=t->next) {
			TextFlush(t);
		}
		return;
	}

	XSync(0);

	while (XCheckWindowEvent(t->w,t->eventmask|ExposeWindow,&e)) {
		if (TextEvent(t,&e)) {
			/* 
			 * We should never get here. Accordingly, we
			 * don't do anything if we do, somehow, get here.  
			 */
		}
	}

}

Change_text_window_size (t, new_h, new_w)
	register TextWindow *t;
	register int new_h, new_w;
{
	register int i;
	register char *curline;

	Normalize(t);		/* Rearrange lines so that first_line = 0 */

	/* First free up any now extraneous lines */

	for (i = new_h; i < t->num_lines; i++) free((char *)t->lines[i]);

	if ((t->lines = (char **)
		realloc((char *)t->lines, new_h * sizeof (char *))) == NULL) {
	    return;
	}

	if ((t->line_length = (short *)
		realloc((char *)t->line_length, new_h * sizeof (short))) == NULL) {
	    return;
	}

	if ((t->line_chars = (short *)
		realloc((char *)t->line_chars, new_h * sizeof (short))) == NULL) {
	    return;
	}

	if ((t->scroll_history = realloc(t->scroll_history, new_h)) == NULL) {
	    return;
	}

	for (i = 0; i < new_h; i++) {
	    if (i < t->num_lines) {
		if ((curline = t->lines[i] =
			realloc(t->lines[i], new_w + 1)) == NULL) {
		    return;
		}

		if (t->line_chars[i] > new_w) {
		    t->line_chars[i] = new_w;
		    curline[new_w] = '\0';	/* Truncate the line */
		    t->line_length[i] = mar_width +
			    XStringWidth (curline, t->font, 0, 0);
		}
	    } else {
		if ((t->lines[i] = malloc(new_w+1)) == NULL) {
		    return;
		}
		t->lines[i][0] = '\0';
		t->line_chars[i] = 0;
		t->line_length[i] = mar_width;
	    }
	}

	if (t->last_line >= new_h) {
	    t->last_line = new_h - 1;
	    t->last_char = t->line_chars[t->last_line];
	    t->next_x = t->line_length[t->last_line];
	    t->next_y = t->last_line * t->font->height;

	} else if (t->last_char > new_w) {
	    t->last_char = t->line_chars[t->last_line];
	    t->next_x = t->line_length[t->last_line];
	}

	t->num_lines = new_h;
	t->num_chars = new_w;
	t->height = new_h * t->font->height;
	t->width = new_w * t->font->width + mar_width;
}

/* Routine to re-arrange the lines in a window structure so that first_line
   is equal to 0. */

Normalize(t)
	register TextWindow *t;
{
	if (t->first_line == 0) return;

	t->last_line -= t->first_line;
	if (t->last_line < 0) t->last_line += t->num_lines;

	Spin_lines(t, 0, t->num_lines-1, t->first_line);

	t->first_line = 0;
}

/* Spin lines rotates the m through n lines of the arrays
   forward offset places.  For example, 012345 spun forward 2 is 234501.
   It's straightforward to spin the first part of the arrays; and we
   call Spin_lines recursively to do the last offset elements */

/* Actually, it's tail-recursive, so I just use a loop.  But I can
   pretend, can't I? */

Spin_lines(t, m, n, offset)
	register TextWindow *t;
	int m, n;
	register int offset;
{
	register int i;
	register int temp;		/* Temporaries */
	register char *tempc;

	while (1) {
	    if (offset == 0 || offset > n-m) return;

	    for (i = m; i <= n-offset; i++) {
		temp = t->line_length[i];
		t->line_length[i] = t->line_length[offset+i];
		t->line_length[offset+i] = temp;

		temp = t->line_chars[i];
		t->line_chars[i] = t->line_chars[offset+i];
		t->line_chars[offset+i] = temp;

		tempc = t->lines[i];
		t->lines[i] = t->lines[offset+i];
		t->lines[offset+i] = tempc;
	    }

/*	    Spin_lines(t, n-offset+1, n, offset - ((n-m+1) % offset)); */

	    temp = m;
	    m = n - offset + 1;
	    offset -= (n - temp + 1) % offset;
	}
}

/* Routine to put a string in a text window.  If fastscroll is
   set in the TextWindow structure, a single block scroll is done instead
   of scrolling at each newline. */

#define verybig 10000	/* Amount to scroll if we should refresh instead */

TextPutString (t, str)
	register TextWindow *t;
	register char *str;
{
	register char *ch = str;
	register char oldch;
	int jump = t->fastscroll;	/* Whether to do jump scrolling */
	int newlines, scroll;

	if (jump) jump = Count_lines (t, str, &newlines, &scroll);

	while (1) {
	    while (*ch != '\0' && *ch != '\n') ch++;
	    if (ch != str) {
		oldch = *ch;
		*ch = '\0';
		Do_text_string (t, str);
		*ch = oldch;
	    }
	    if (*ch == '\0') break;
	    if (jump && newlines == scroll) {
		Clear_lines (t, newlines);
		dont_update = TRUE;	/* Stop updating now */
	    }
	    newlines--;
	    TextPutChar (t, *ch);
	    str = ++ch;
	}
	if (t->mapped && jump) {
	    if (scroll != verybig) Scroll_text_window (t, scroll);
	    else TextRedisplay (t);
	}
	dont_update = FALSE;
}

/* Count the number of lines in str, calculate how much scrolling
   will be needed, and return whether this amount is positive */

int Count_lines (t, str, newlines, scroll)
	register TextWindow *t;
	register char *str;
	int *newlines, *scroll;
{
	register int num_lines = 0;
	register int lines_left, height = t->num_lines;

	*scroll = 0;

	while (*str) {
	    if (*str++ == '\n') num_lines++;
	}

	*newlines = num_lines;

	if (num_lines <= 1) return FALSE;    /* Don't bother jump scrolling */

	/* Would this fill the screen? */

	if (num_lines >= height) {
	    *scroll = verybig;
	    return TRUE;
	}

	/* Calculate the number of lines left in the window */

	lines_left = height - (t->last_line - t->first_line + 1);
	if (lines_left >= height) lines_left -= height;

	/* Figure out how many lines to scroll */

	num_lines -= lines_left;

	if (num_lines <= 0) return FALSE;	/* Enough room already */

	*scroll = num_lines;
	return TRUE;
}

/* Clear a number of lines in the window data structure */

Clear_lines (t, scroll)
	register TextWindow *t;
	register int scroll;
{
	register int i, start = t->first_line;
	register int height = t->num_lines;

	/* If this would fill the screen, clear it instead */

	if (scroll >= t->height	) {
	    TextClear (t);
	    return;
	}

	/* Shift the contents */

	t->first_line += scroll;
	if (t->first_line >= height) t->first_line -= height;

	/* Now clear the blank lines */

	for (i = 0; i < scroll; i++) {
	    t->lines[start][0] = '\0';
	    t->line_chars[start] = 0;
	    t->line_length[start] = mar_width;	/* Allow a left margin */
	    start++;
	    if (start >= height) start = 0;
	}
}

/* Store the characters of a string in the window and update the screen,
   but only if dont_update isn't set */

Do_text_string (t, str)
	register TextWindow *t;
	char *str;
{
	register char *ch = str;
	register char *curline = t->lines[t->last_line];
	register int curchar = t->last_char;
	register int x = t->next_x;
	register FontInfo *f = t->font;
	int start_x = t->next_x, start = curchar,
		minch = f->firstchar, maxch = f->lastchar;

	/* First store the characters in the line */

	while (*ch != '\0' && curchar < t->num_chars) {
	    curline[curchar] = *ch;
	    if (*ch >= minch && *ch <= maxch) {
		x += f->fixedwidth ? f->width : f->widths[*ch - minch];
	    }
	    curchar++;
	    ch++;
	}

	curline[curchar] = '\0';
	t->line_chars[t->last_line] = t->last_char = curchar;
	t->line_length[t->last_line] = t->next_x = x;

	if (dont_update || !t->mapped) return;

	/* And then update the screen */

	if (start < t->num_chars) {
	    XText (t->w, start_x, t->next_y, str, curchar-start,
		    f->id, t->fgpixel, t->bgpixel);
	}
}

/* Textputchar displays a character in the text window.  It
 * responds to \n as a special character and just displays anything else.
 */

TextPutChar (t, ch)
	register TextWindow *t;
	char ch;
{
	register int i, height = t->num_lines;
	register char *curline = t->lines[t->last_line];
	register FontInfo *f = t->font;
	
	switch (ch) {
	    case '\0':		/* NULL */
		break;

	    case '\n':		/* newline */
		if (t->last_line == t->first_line - 1 ||
			(t->last_line == height - 1 && t->first_line == 0)) {

		    /* The screen is full...clear out the first line */

		    t->lines[t->first_line][0] = '\0';
		    t->line_chars[t->first_line] = 0;
		    t->line_length[t->first_line] = mar_width;

		    t->first_line++;			/* And advance it */
		    if (t->first_line == height) t->first_line = 0;

		    if (!dont_update && t->mapped) Scroll_text_window (t, 1);

		} else if (!dont_update) t->next_y += f->height;

		t->last_line++;
		if (t->last_line == height) t->last_line = 0;

		t->last_char = 0;
		t->next_x = mar_width;
		break;

	    default:		/* Just insert the character */
		t->last_char++;
		t->line_chars[t->last_line]++;
		if (t->last_char > t->num_chars) break;

		curline[t->last_char] = ch;
		curline[t->last_char+1] = '\0';

		if (!dont_update && t->mapped) {
		    XText(t->w, t->next_x, t->next_y, &ch, 1,
			    f->id, t->fgpixel, t->bgpixel);
		}
		if (ch <= f->firstchar && ch >= f->lastchar) {
		    t->line_length[t->last_line] = t->next_x +=
			    (f->fixedwidth ? f->width :
					     f->widths[ch - f->lastchar]);
		}
		break;
	}
}

/* This procedure moves the contents of a text window up n lines.
 */

Scroll_text_window (t, n)
	register TextWindow *t;
	register int n;
{
	register int i, y, x, width, j;
	int height = t->font->height;
	int scrollsize = n * height;

	/* First shift up the contents */

	XMoveArea(t->w, 0, scrollsize, 0, 0, t->width, t->height-scrollsize);

	/* Now redisplay the bottom n lines */

	y = height * (t->num_lines - n);
	i = t->first_line - n;
	if (i < 0) i += t->num_lines;

	for (j = 0; j < n; j++) {
	    if (t->line_chars[i]) {
		XText (t->w, mar_width, y, t->lines[i], t->line_chars[i],
			t->font->id, t->fgpixel, t->bgpixel);
	    }
	    x = t->line_length[i];
	    width = t->width - x;

	    if (width > 0) XPixSet(t->w, x, y, width, height, t->bgpixel);
	    y += height;
	    i++;
	    if (i == t->num_lines) i = 0;
	}

	/* Add the current scroll to all values in the scroll history,
	   then add a new entry at the end (the history wraps!) */

	i = t->scroll_start;

	for (j = 0; j < t->scroll_count; j++) {
	    t->scroll_history[i] += n;
	    i++;
	    if (i >= t->num_lines) i = 0;
	}
	t->scroll_count++;
	t->scroll_history[i] = n;

	if (t->scroll_count > t->num_lines) t->scroll_start++; /* trash one */
}

#define TEXT_BUFSIZE 2048

TextPrintf(t, format, args)
	TextWindow *t;
	char *format;
{
	char buffer[TEXT_BUFSIZE+1];
	struct _iobuf _strbuf;

	_strbuf._flag = _IOWRT+_IOSTRG;
	_strbuf._ptr = buffer;
	_strbuf._cnt = TEXT_BUFSIZE;
	_doprnt(format, &args, &_strbuf);
	_strbuf._cnt++;	    /* Be sure there's room for the \0 */
	putc('\0', &_strbuf);
	TextPutString(t, buffer);
}
