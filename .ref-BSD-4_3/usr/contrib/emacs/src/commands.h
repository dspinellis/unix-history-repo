/* Definitions needed by most editing commands.
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


#define Ctl(c) ((c)&037)

/* Define the names of keymaps, just so people can refer to them in calls to defkey */

extern struct Lisp_Vector *GlobalMap;
				/* default global key bindings */

extern struct Lisp_Vector *ESCmap;
				/* The keymap used for globally bound
				   ESC-prefixed default commands */

extern struct Lisp_Vector *CtlXmap;
				/* The keymap used for globally bound
				   C-x-prefixed default commands */

extern Lisp_Object Vminibuffer_local_map;
				/* The keymap used by the minibuf for
				   local bindings when spaces are allowed
				   in the minibuf */

extern Lisp_Object Vminibuffer_local_ns_map;
				/* The keymap used by the minibuf for
				   local bindings when spaces are not
				   encouraged in the minibuf */

/* keymap used for minibuffers when doing completion */
extern Lisp_Object Vminibuffer_local_completion_map;

/* keymap used for minibuffers when doing completion and require a match */
extern Lisp_Object Vminibuffer_local_must_match_map;

extern int last_command_char;	    /* The last key struck as a command */

extern int unread_command_char;	/* Command character to be re-read, or -1 */

/* Previous command symbol found here for comparison */
extern Lisp_Object last_command;

extern int immediate_quit;	    /* Nonzero means ^G can quit instantly */

extern Lisp_Object Vexecuting_macro;

/* Nonzero if input is coming from the keyboard */

#define INTERACTIVE (NULL (Vexecuting_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of mode line. */

extern int RedoModes;
