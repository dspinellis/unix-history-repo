/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* some utility functions, as macros, to be included by jove.h */

extern char	key_strokes[100],
		*keys_p;

extern int	ModCount;

#define init_strokes()	{ keys_p = key_strokes; *keys_p = '\0'; }
#define add_stroke(c)	{ \
	if (keys_p < &key_strokes[sizeof (key_strokes) - 1]) \
		(*keys_p++ = (c), *keys_p = 0); \
}

#define IsModified(b)	((b)->b_modified)
#define SavLine(a, b)	((a)->l_dline = putline((b)))
#define SetLine(line)	DotTo((line), 0)
#define bobp()		(firstp(curline) && bolp())
#define bolp()		(curchar == 0)
#define eobp()		(lastp(curline) && eolp())
#define eolp()		(linebuf[curchar] == '\0')
#define firstp(line)	((line) == curbuf->b_first)
#define getDOT()	getline(curline->l_dline, linebuf)
#define lastp(line)	((line) == curbuf->b_last)

extern char
	*IOerr proto((char *err, char *file)),
	*StrIndex proto((int dir,char *buf,int charpos, int what)),
	*basename proto((char *f)),
	*copystr proto((char *str)),
	*emalloc proto((size_t size)),
	*filename proto((struct buffer *b)),
	*get_time proto((time_t *timep,char *buf,int from,int to)),
	*itoa proto((int num)),
	*lcontents proto((struct line *line)),
	*ltobuf proto((struct line *line,char *buf));

extern int
	LineDist proto((struct line *nextp,struct line *endp)),
	ModBufs proto((int allp)),
	TwoBlank proto((void)),
	blnkp proto((char *buf)),
	within_indent proto((void)),
	casecmp proto((char *s1,char *s2)),
	casencmp proto((char *s1,char *s2, size_t n)),
	fixorder proto((struct line * *line1,int *char1,struct line * *line2,int *char2)),
	inlist proto((struct line *first,struct line *what)),
	inorder proto((struct line *nextp,int char1,struct line *endp,int char2)),
	length proto((struct line *line)),
	max proto((int a,int b)),
	min proto((int a,int b)),
	numcomp proto((char *s1,char *s2)),
	pnt_line proto((void)),
	sindex proto((char *pattern,char *string)),
	waitchar proto((int *slow));

extern void
	DOTsave proto((struct position *buf)),
	DotTo proto((struct line *line,int col)),
	ExecCmd proto((struct data_obj *cp)),
	PushPntp proto((struct line *line)),
	SetDot proto((struct position *bp)),
	ToFirst proto((void)),
	ToLast proto((void)),
	ins_c proto((int c,char *buf,int atchar,int num,int max)),
	len_error proto((int flag)),
	linecopy proto((char *onto,int atchar,char *from)),
	make_argv proto((char * *argv,char *ap)),
	modify proto((void)),
	SitFor proto((int delay)),
	null_ncpy proto((char *to, char *from, size_t n)),
#ifdef	UNIX
	dopipe proto((int *p)),
	pclose proto((int *p)),
#endif
	pop_env proto((jmp_buf)),
	push_env proto((jmp_buf)),
#ifdef LOAD_AV
	get_la proto((double *dp)),
#endif /* LOAD_AV */
	to_word proto((int dir)),
	unmodify proto((void)),
	pp_key_strokes proto((char *buffer, size_t size));
