/* Definitions of objects used by the GNU Emacs undo facility.
   Copyright (C) 1985 Fen Labalme and Richard M. Stallman.

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


enum Ukinds {			/* The events that can exist in the undo
				   queue. */
    Uboundary,			/* A boundary between sets of undoable things
				   */
    Unundoable,			/* What's done is done -- some things can't
				   be undone */
    Udelete,			/* Delete characters to perform the undo */
    Uinsert,			/* Insert .... */
    Uchange,			/* Replace characters */
    Uunmod,			/* Clear modification-flag to perform undo */
};

struct UndoRec {		/* A record of a single undo action */
    enum Ukinds kind;		/* the kind of action to be undone */
    int pos;			/* Where dot is */
    int len;			/* The extent of the undo (characters
				   inserted or deleted) */
};

/* The undo history consists of two circular queues, one of characters and
   one of UndoRecs.  When Uinsert recs are added to UndoRQ characters get
   added to UndoCQ.  The position of the characters can be reconstructed by
   subtracting len from the fill pointer. */

#define NUndoR	(((1 << 13) - 4) / sizeof (struct UndoRec))
#define NUndoC	((1 << 13) - 4)

/* Initially allocate them these sizes;
 if these sizes get filled up, make them full size */

#define InitNUndoR 8
#define InitNUndoC (512 - 4)

struct UndoData
  {
    struct UndoRec *undorecs;	/* The undo records, NUndoR of them */
    char *undochars;	/* And the characters associated, NUndoC in all */
    int nextrec;		/* Indices for storing in above two */
    int nextchar;
    int num_undorecs;		/* Sizes allocated */
    int num_undochars;
  };

