/* Definitions needed by most editing commands.
   Copyright (C) 1985 Free Software Foundation, Inc.

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


#define Ctl(c) ((c)&037)

/* Define the names of keymaps, just so people can refer to them in calls to defkey */

extern Lisp_Object Vglobal_map;

extern Lisp_Object Vesc_map;

extern Lisp_Object Vctl_x_map;

extern Lisp_Object Vminibuffer_local_map;

extern Lisp_Object Vminibuffer_local_ns_map;

/* keymap used for minibuffers when doing completion */
extern Lisp_Object Vminibuffer_local_completion_map;

/* keymap used for minibuffers when doing completion and require a match */
extern Lisp_Object Vminibuffer_local_must_match_map;

/* Last character of last key sequence.  */
extern int last_command_char;

/* Command character to be re-read, or -1 */
extern int unread_command_char;

/* Previous command symbol found here for comparison */
extern Lisp_Object last_command;

/* Nonzero means ^G can quit instantly */
extern int immediate_quit;

extern Lisp_Object Vexecuting_macro;

/* Nonzero if input is coming from the keyboard */
#define FROM_KBD (NULL (Vexecuting_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of mode line. */

extern int update_mode_lines;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */

extern int cursor_in_echo_area;
