/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#ifdef lint
#include "undo.h"
#endif /* lint */


#define SetPoint  point =

#define PointRight  point +=
#define  PointLeft  point -=

struct buffer_text
  {
    unsigned char *p1;		/* Address of first data char, minus 1 */
    unsigned char *p2;		/* p1 plus gap size */
    int size1;			/* # characters before gap */
    int size2;			/* # characters after gap */
    int gap;			/* gap size in chars */
    int modified;		/* tick at which contents last modified */
    int head_clip;		/* # of first char that's visible (origin 1) */
    int tail_clip;		/* # chars not visible at end of buffer */
    int pointloc;		/* # of char point is at (origin 1) */
  };

/* structure that defines a buffer */
struct buffer
  {
    struct buffer_text text;	/* This describes the buffer's text */

    Lisp_Object number;		/* buffer number, assigned when buffer made */
    Lisp_Object name;		/* the name of this buffer */
    Lisp_Object filename;	/* the name of the file associated
				   with this buffer */
    Lisp_Object directory;	/* Dir for expanding relative pathnames */
    int save_modified;		/* Value of text.modified when buffer last saved */
    Lisp_Object save_length;	/* Length of file when last read or saved. */
    int modtime;		/* Set to the modtime of the file when read */
				/* Really should be time_t */
    int backed_up;		/* true iff this buffer has been been backed
				   up (if you write to its associated file
				   and it hasn't been backed up, then a
				   backup will be made) */
    Lisp_Object auto_save_file_name;	/* file name used for auto-saving this
				   buffer */
    int auto_save_modified;	/* the value of text.modified at the last auto-save. */
    Lisp_Object read_only;      /* Non-nil if buffer read-only */

    Lisp_Object markers;	/* the markers that refer to this buffer.
				   This is actually a single marker ---
				   successive elements in its marker `chain'
				   are the other markers referring to this
				   buffer */
    Lisp_Object mark;		/* "The mark"; may be nil */

    Lisp_Object major_mode;	/* Symbol naming major mode (eg lisp-mode) */
    Lisp_Object mode_name;	/* Pretty name of major mode (eg "Lisp") */
    Lisp_Object mode_line_format; /* Format string for mode line */

    Lisp_Object keymap;		/* Keys that are bound local to this buffer
				   (stuff like $J) */
    struct Lisp_Vector *syntax_table_v;	/* the syntax table in use */
    Lisp_Object abbrev_table;	/* This buffer's local abbrev table */

    /* Values of several buffer-local variables */
    /* tab-width is buffer-local so that redisplay can find it
       in buffers that are not current */
    Lisp_Object case_fold_search;
    Lisp_Object tab_width;
    Lisp_Object fill_column;
    Lisp_Object left_margin;
    Lisp_Object auto_fill_hook;	/* Function to call when insert space past fiull column */
  /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER)
     for all per-buffer variables of this buffer.  */
    Lisp_Object local_var_alist;

    /* Position in buffer at which display started
       the last time this buffer was displayed */
    int last_window_start;
    /* Non-nil means do not display continuation lines */
    Lisp_Object truncate_lines;
    /* Non-nil means display ctl chars with uparrow */
    Lisp_Object ctl_arrow;
    /* Non-nil means do selective display;
       See doc string in syms_of_buffer (buffer.c) for details.  */
    Lisp_Object selective_display;
    /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer. */
    Lisp_Object minor_modes;
    /* Undo records for changes in this buffer. */
    struct UndoData *undodata;
    /* t if "self-insertion" should overwrite */
    Lisp_Object overwrite_mode;
    /* non-nil means abbrev mode is on.  Expand abbrevs automatically. */
    Lisp_Object abbrev_mode;
    /* Next buffer, in chain of all buffers that exist.  */
    struct buffer *next;
};

extern struct buffer *bf_cur;		/* the current buffer */

/* This structure contains data describing the text of the current buffer.
 Switching buffers swaps their text data in and out of here */

extern struct buffer_text bf_text;

#define bf_p1 bf_text.p1
#define bf_p2 bf_text.p2
#define bf_s1 bf_text.size1
#define bf_s2 bf_text.size2
#define bf_gap bf_text.gap
#define bf_modified bf_text.modified
#define bf_head_clip bf_text.head_clip
#define bf_tail_clip bf_text.tail_clip
#define point bf_text.pointloc

/* Lowest legal value of point for current buffer */
#define FirstCharacter bf_text.head_clip

/* Number of last visible character in current buffer */
/* The highest legal value for point is one greater than this */
#define NumCharacters (bf_text.size1+bf_text.size2-bf_text.tail_clip)

/* Return character at position n.  No range checking */
#define CharAt(n) *(((n)>bf_s1 ? bf_p2 : bf_p1) + (n))

extern void reset_buffer ();
