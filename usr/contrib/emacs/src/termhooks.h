/* Hooks by which low level terminal operations
   can be made to call other routines.
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


extern int (*topos_hook) ();
extern int (*raw_topos_hook) ();

extern int (*clear_to_end_hook) ();
extern int (*clear_screen_hook) ();
extern int (*clear_end_of_line_hook) ();

extern int (*ins_del_lines_hook) ();

extern int (*change_line_highlight_hook) ();
extern int (*reassert_line_highlight_hook) ();

extern int (*insert_chars_hook) ();
extern int (*write_chars_hook) ();
extern int (*delete_chars_hook) ();

extern int (*ring_bell_hook) ();

extern int (*reset_terminal_modes_hook) ();
extern int (*set_terminal_modes_hook) ();
extern int (*update_begin_hook) ();
extern int (*update_end_hook) ();
extern int (*set_terminal_window_hook) ();

extern int (*read_socket_hook) ();
extern int (*fix_screen_hook) ();

/* If nonzero, send all terminal output characters to this stream also.  */

extern FILE *termscript;

