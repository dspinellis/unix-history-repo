/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define DIRTY		((daddr) 01)	/* needs update for some reason */
#define MODELINE	02		/* this is a modeline */
#define L_MOD		04		/* line has been modified internally */

#define makedirty(line)	{ (line)->l_dline |= DIRTY; }
#define isdirty(line)	((line)->l_dline & DIRTY)

struct scrimage {
	int	s_offset,	/* offset to start printing at */
		s_flags,	/* various flags */
		s_id,		/* which buffer line */
		s_vln;		/* Visible Line Number */
	Line	*s_lp;		/* so we can turn off red bit */
	Window	*s_window;	/* window that contains this line */
};

extern struct scrimage
	*DesiredScreen,		/* what we want */
	*PhysScreen;		/* what we got */

extern bool
	UpdModLine,	/* whether we want to update the mode line */
	UpdMesg;	/* update the message line */

extern int
	chkmail proto((int force)),
	calc_pos proto((char *lp,int c_char));

extern void
	disp_opt_init proto((void)),
	ChkWindows proto((struct line *line1,struct line *line2)),
	ChkWinLines proto((void)),
	DrawMesg proto((bool abortable)),
	TOstart proto((char *name, bool auto_newline)),
	TOstop proto((void)),
	Typeout proto((char *, ...)),
	rbell proto((void)),
	redisplay proto((void));

extern int
	DisabledRedisplay;

#ifdef	ID_CHAR
extern bool
	IN_INSmode;

extern int
	IMlen;

extern void
	INSmode proto((bool));
#endif	/* ID_CHAR */
