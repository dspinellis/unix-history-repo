/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#define SET_PT  PT =

/* Just for laughs, use the same names as in ITS TECO, around 1973.  */

/* Character position of beginning of buffer.  */ 
#define BEG (1)
/* Character position of beginning of accessible range of buffer.  */ 
#define BEGV (current_buffer->text.begv)
/* Character position of point in buffer.  */ 
#define PT (current_buffer->text.pt)
/* Character position of gap in buffer.  */ 
#define GPT (current_buffer->text.gpt)
/* Character position of end of accessible range of buffer.  */ 
#define ZV (current_buffer->text.zv)
/* Character position of end of buffer.  */ 
#define Z (current_buffer->text.z)
/* Modification count.  */
#define MODIFF (current_buffer->text.modiff)

/* Address of beginning of buffer.  */ 
#define BEG_ADDR (current_buffer->text.beg)
/* Address of beginning of accessible range of buffer.  */ 
#define BEGV_ADDR (&FETCH_CHAR (current_buffer->text.begv))
/* Address of point in buffer.  */ 
#define PT_ADDR (&FETCH_CHAR (current_buffer->text.pt))
/* Address of beginning of gap in buffer.  */ 
#define GPT_ADDR (current_buffer->text.beg + current_buffer->text.gpt - 1)
/* Address of end of gap in buffer.  */
#define GAP_END_ADDR (current_buffer->text.beg + current_buffer->text.gpt + current_buffer->text.gap_size - 1)
/* Address of end of accessible range of buffer.  */ 
#define ZV_ADDR (&FETCH_CHAR (current_buffer->text.zv))

/* Size of gap.  */
#define GAP_SIZE (current_buffer->text.gap_size)

/* Now similar macros for a specified buffer.
   Note that many of these evaluate the buffer argument more than once.  */

/* Character position of beginning of buffer.  */ 
#define BUF_BEG(buf) (1)
/* Character position of beginning of accessible range of buffer.  */ 
#define BUF_BEGV(buf) ((buf)->text.begv)
/* Character position of point in buffer.  */ 
#define BUF_PT(buf) ((buf)->text.pt)
/* Character position of gap in buffer.  */ 
#define BUF_GPT(buf) ((buf)->text.gpt)
/* Character position of end of accessible range of buffer.  */ 
#define BUF_ZV(buf) ((buf)->text.zv)
/* Character position of end of buffer.  */ 
#define BUF_Z(buf) ((buf)->text.z)
/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text.modiff)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text.beg)

/* Macro for setting the value of BUF_ZV (BUF) to VALUE,
   by varying the end of the accessible region.  */
#define SET_BUF_ZV(buf, value) ((buf)->text.zv = (value))
#define SET_BUF_PT(buf, value) ((buf)->text.pt = (value))

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text.gap_size)

/* Return the address of character at position POS in buffer BUF. 
   Note that both arguments can be computed more than once.  */
#define BUF_CHAR_ADDRESS(buf, pos) \
((buf)->text.beg + (pos) - 1		\
 + ((pos) >= (buf)->text.gpt ? (buf)->text.gap_size : 0))

/* Convert the address of a char in the buffer into a character position.  */
#define PTR_CHAR_POS(ptr) \
((ptr) - (current_buffer)->text.beg		\
 - (ptr - (current_buffer)->text.beg < (unsigned) GPT ? 0 : GAP_SIZE))

struct buffer_text
  {
    /* Actual address of buffer contents.  */
    unsigned char *beg;
    /* Values of BEGV ... Z in this buffer.  */
    int begv;
    int pt;
    int gpt;
    int zv;
    int z;
    /* Value of GAP_SIZE in this buffer.  */
    int gap_size;
    /* This counts buffer-modification events for this buffer.
       It is incremented for each such event, and never otherwise changed.  */
    int modiff;
  };

struct buffer
  {
    /* Everything before the `name' slot must be of a non-Lisp_Object type,
       and every slot after `name' must be a Lisp_Object
       This is known about by both mark_buffer (alloc.c) and
       Flist_buffer_local_variables (buffer.c)
     */
    /* This structure holds the coordinates of the buffer contents.  */
    struct buffer_text text;
    /* Next buffer, in chain of all buffers including killed buffers.
       This chain is used only for garbage collection, in order to
       collect killed buffers properly.  */
    struct buffer *next;
    /* Flags saying which DEFVAR_PER_BUFFER variables
       are local to this buffer.  */
    int local_var_flags;
    /* Value of text.modified as of when visited file was read or written.  */
    int save_modified;
    /* the value of text.modified at the last auto-save. */
    int auto_save_modified;
    /* Set to the modtime of the visited file when read or written.
       -1 means visited file was nonexistent.
       0 means visited file modtime unknown; in no case complain
       about any mismatch on next save attempt.  */
    int modtime;
    /* Position in buffer at which display started
       the last time this buffer was displayed */
    int last_window_start;

    /* This is a special exception -- as this slot should not be
       marked by gc_sweep, and as it is not lisp-accessible as
       a local variable -- so re regard it as not really being of type
       Lisp_Object */
    /* the markers that refer to this buffer.
       This is actually a single marker ---
       successive elements in its marker `chain'
       are the other markers referring to this
       buffer */
    Lisp_Object markers;


    /* Everything from here down must be a Lisp_Object */


    /* the name of this buffer */
    Lisp_Object name;
    /* the name of the file associated with this buffer */
    Lisp_Object filename;
    /* Dir for expanding relative pathnames */
    Lisp_Object directory;
    /* True iff this buffer has been been backed
       up (if you write to its associated file
       and it hasn't been backed up, then a
       backup will be made) */
    /* This isn't really used by the C code, so could be deleted.  */
    Lisp_Object backed_up;
    /* Length of file when last read or saved. */
    Lisp_Object save_length;
    /* file name used for auto-saving this buffer */
    Lisp_Object auto_save_file_name;
    /* Non-nil if buffer read-only */
    Lisp_Object read_only;
    /* "The mark"; no longer allowed to be nil */
    Lisp_Object mark;

    /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER)
       for all per-buffer variables of this buffer.  */
    Lisp_Object local_var_alist;


    /* Symbol naming major mode (eg lisp-mode) */
    Lisp_Object major_mode;
    /* Pretty name of major mode (eg "Lisp") */
    Lisp_Object mode_name;
    /* Format description for mode line */
    Lisp_Object mode_line_format;

    /* Keys that are bound local to this buffer */
    Lisp_Object keymap;
    /* This buffer's local abbrev table */
    Lisp_Object abbrev_table;
    /* This buffer's syntax table.  */
    Lisp_Object syntax_table;

    /* Values of several buffer-local variables */
    /* tab-width is buffer-local so that redisplay can find it
       in buffers that are not current */
    Lisp_Object case_fold_search;
    Lisp_Object tab_width;
    Lisp_Object fill_column;
    Lisp_Object left_margin;
    /* Function to call when insert space past fiull column */
    Lisp_Object auto_fill_hook;

    /* Non-nil means do not display continuation lines */
    Lisp_Object truncate_lines;
    /* Non-nil means display ctl chars with uparrow */
    Lisp_Object ctl_arrow;
    /* Non-nil means do selective display;
       See doc string in syms_of_buffer (buffer.c) for details.  */
    Lisp_Object selective_display;
    /* Non-nil means show ... at end of line followed by invisible lines.  */
    Lisp_Object selective_display_ellipses;
    /* t if "self-insertion" should overwrite */
    Lisp_Object overwrite_mode;
    /* non-nil means abbrev mode is on.  Expand abbrevs automatically. */
    Lisp_Object abbrev_mode;
    /* Changes in the buffer are recorded here for undo.
       t means don't record anything.  */
    Lisp_Object undo_list;
};

extern struct buffer *current_buffer;		/* points to the current buffer */

/* This structure holds the default values of the buffer-local variables
   defined with DefBufferLispVar, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

extern struct buffer buffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is zero, then even though there may
   be a DefBufferLispVar for the slot, there is no default valuefeor it;
   and the corresponding slot in buffer_defaults is not used.  */

extern struct buffer buffer_local_flags;

/* For each buffer slot, this points to the Lisp symbol name
   for that slot in the current buffer.  It is 0 for slots
   that don't have such names.  */

extern struct buffer buffer_local_symbols;

/* Some aliases for info about the current buffer.  */

#define point current_buffer->text.pt

/* Return character at position n.  No range checking */
#define FETCH_CHAR(n) *(((n)>= GPT ? GAP_SIZE : 0) + (n) + BEG_ADDR - 1)
  
/* BufferSafeCeiling (resp. BufferSafeFloor), when applied to n, return */
/* the max (resp. min) p such that &FETCH_CHAR (p) - &FETCH_CHAR (n) == p - n   */
#define BufferSafeCeiling(n) (((n) < GPT && GPT < ZV ? GPT : ZV) - 1)

#define BufferSafeFloor(n) (BEGV <= GPT && GPT <= (n) ? GPT : BEGV)

extern void reset_buffer ();
