/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

struct screenline {
	char	*s_line,
		*s_length;
};

extern struct screenline	*Screen,
				*Curline;

extern char *cursend;

extern int
	i_line,
	i_col,
	AbortCnt,

	CapLine,	/* cursor line and cursor column */
	CapCol;

extern bool
	CanScroll,	/* can this terminal scroll? */

	BufSwrite proto((int linenum)),
	swrite proto((char *line,bool inversep,bool abortable));

extern void
	IDline_setup proto((char *tname)),
	Placur proto((int line,int col)),
	cl_eol proto((void)),
	cl_scr proto((bool doit)),
	clrline proto((char *cp1,char *cp2)),
	i_set proto((int nline,int ncol)),
	make_scr proto((void)),
	v_ins_line proto ((int num, int top, int bottom)),
	v_del_line proto ((int num, int top, int bottom)),
	InitCM proto((void)),
	SO_off proto((void)),
	SO_on proto((void));
