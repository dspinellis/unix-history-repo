/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/ed.screen.c,v 3.11 1991/12/19 22:34:14 christos Exp $ */
/*
 * ed.screen.c: Editor/termcap-curses interface
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: ed.screen.c,v 3.11 1991/12/19 22:34:14 christos Exp $")

#include "ed.h"
#include "tc.h"
#include "ed.defns.h"

/*
 * We don't prototype these, cause some systems have them wrong!
 */
extern char *tgoto();
extern char *tgetstr();
extern char *tputs();
extern int tgetent();
extern int tgetflag();
extern int tgetnum();


/* #define DEBUG_LITERAL */

/*
 * IMPORTANT NOTE: these routines are allowed to look at the current screen
 * and the current possition assuming that it is correct.  If this is not
 * true, then the update will be WRONG!  This is (should be) a valid
 * assumption...
 */

#define TC_BUFSIZE 2048

#define GoodStr(a) (tstr[a].str != NULL && tstr[a].str[0] != '\0')
#define Str(a) tstr[a].str
#define Val(a) tval[a].val

static struct {
    char   *b_name;
    int     b_rate;
}       baud_rate[] = {

#ifdef B0
    { "0", B0 },
#endif
#ifdef B50
    { "50", B50 },
#endif
#ifdef B75
    { "75", B75 },
#endif
#ifdef B110
    { "110", B110 },
#endif
#ifdef B134
    { "134", B134 },
#endif
#ifdef B150
    { "150", B150 },
#endif
#ifdef B200
    { "200", B200 },
#endif
#ifdef B300
    { "300", B300 },
#endif
#ifdef B600
    { "600", B600 },
#endif
#ifdef B900
    { "900", B900 },
#endif
#ifdef B1200
    { "1200", B1200 },
#endif
#ifdef B1800
    { "1800", B1800 },
#endif
#ifdef B2400
    { "2400", B2400 },
#endif
#ifdef B3600
    { "3600", B3600 },
#endif
#ifdef B4800
    { "4800", B4800 },
#endif
#ifdef B7200
    { "7200", B7200 },
#endif
#ifdef B9600
    { "9600", B9600 },
#endif
#ifdef EXTA
    { "19200", EXTA },
#endif
#ifdef B19200
    { "19200", B19200 },
#endif
#ifdef EXTB
    { "38400", EXTB },
#endif
#ifdef B38400
    { "38400", B38400 },
#endif
    { NULL, 0 }
};

static struct termcapstr {
    char   *name;
    char   *long_name;
    char   *str;
}       tstr[] = {

#define T_al	0
    {	"al",	"add new blank line",		NULL },
#define T_bl	1
    {	"bl",	"audible bell",			NULL },
#define T_cd	2
    {	"cd",	"clear to bottom",		NULL },
#define T_ce	3
    {	"ce",	"clear to end of line",		NULL },
#define T_ch	4
    {	"ch",	"cursor to horiz pos",		NULL },
#define T_cl	5
    {	"cl",	"clear screen",			NULL },
#define	T_dc	6
    {	"dc",	"delete a character",		NULL },
#define	T_dl	7
    {	"dl",	"delete a line",		NULL },
#define	T_dm	8
    {	"dm",	"start delete mode",		NULL },
#define	T_ed	9
    {	"ed",	"end delete mode",		NULL },
#define	T_ei	10
    {	"ei",	"end insert mode",		NULL },
#define	T_fs	11
    {	"fs",	"cursor from status line",	NULL },
#define	T_ho	12
    {	"ho",	"home cursor",			NULL },
#define	T_ic	13
    {	"ic",	"insert character",		NULL },
#define	T_im	14 
    {	"im",	"start insert mode",		NULL },
#define	T_ip	15
    {	"ip",	"insert padding",		NULL },
#define	T_kd	16
    {	"kd",	"sends cursor down",		NULL },
#define	T_kl	17
    {	"kl",	"sends cursor left",		NULL },
#define T_kr	18
    {	"kr",	"sends cursor right",		NULL },
#define T_ku	19
    {	"ku",	"sends cursor up",		NULL },
#define T_md	20
    {	"md",	"begin bold",			NULL },
#define T_me	21
    {	"me",	"end attributes",		NULL },
#define T_nd	22
    {	"nd",	"non destructive space",	NULL },
#define T_se	23
    {	"se",	"end standout",			NULL },
#define T_so	24
    {	"so",	"begin standout",		NULL },
#define T_ts	25
    {	"ts",	"cursor to status line",	NULL },
#define T_up	26
    {	"up",	"cursor up one",		NULL },
#define T_us	27
    {	"us",	"begin underline",		NULL },
#define T_ue	28
    {	"ue",	"end underline",		NULL },
#define T_vb	29
    {	"vb",	"visible bell",			NULL },
#define T_DC	30
    {	"DC",	"delete multiple chars",	NULL },
#define T_DO	31
    {	"DO",	"cursor down multiple",		NULL },
#define T_IC	32
    {	"IC",	"insert multiple chars",	NULL },
#define T_LE	33
    {	"LE",	"cursor left multiple",		NULL },
#define T_RI	34
    {	"RI",	"cursor right multiple",	NULL },
#define T_UP	35
    {	"UP",	"cursor up multiple",		NULL },
#define T_str	36
    {	NULL,	NULL,		NULL }
};

static struct termcapval {
    char   *name;
    char   *long_name;
    int     val;
}       tval[] = {
#define T_pt	0
    {	"pt",	"can use physical tabs", 0 },
#define T_li	1
    {	"li",	"Number of lines",	 0 },
#define T_co	2
    {	"co",	"Number of columns",	 0 },
#define T_km	3
    {	"km",	"Has meta key",		 0 },
#define T_val	4
    {	NULL, NULL,		 0 }
};

static bool me_all = 0;		/* does two or more of the attributes use me */

static	void	ReBufferDisplay	__P((void));
static	void	TCalloc		__P((struct termcapstr *, char *)); 


static void
TCalloc(t, cap)
    struct termcapstr *t;
    char   *cap;
{
    static char termcap_alloc[TC_BUFSIZE];
    char    termbuf[TC_BUFSIZE];
    struct termcapstr *ts;
    static int tloc = 0;
    int     tlen, clen;

    if (cap == NULL || *cap == '\0') {
	t->str = NULL;
	return;
    }
    else
	clen = strlen(cap);

    if (t->str == NULL)
	tlen = 0;
    else
	tlen = strlen(t->str);

    /*
     * New string is shorter; no need to allocate space
     */
    if (clen <= tlen) {
	(void) strcpy(t->str, cap);
	return;
    }

    /*
     * New string is longer; see if we have enough space to append
     */
    if (tloc + 3 < TC_BUFSIZE) {
	(void) strcpy(t->str = &termcap_alloc[tloc], cap);
	tloc += clen + 1;	/* one for \0 */
	return;
    }

    /*
     * Compact our buffer; no need to check compaction, cause we know it
     * fits...
     */
    tlen = 0;
    for (ts = tstr; ts->name != NULL; ts++)
	if (t != ts && ts->str != NULL && ts->str[0] != '\0') {
	    char   *ptr;

	    for (ptr = ts->str; *ptr != '\0'; termbuf[tlen++] = *ptr++);
	    termbuf[tlen++] = '\0';
	}
    copy(termcap_alloc, termbuf, TC_BUFSIZE);
    tloc = tlen;
    if (tloc + 3 >= TC_BUFSIZE) {
	stderror(ERR_NAME | ERR_TCNOSTR);
	return;
    }
    (void) strcpy(t->str = &termcap_alloc[tloc], cap);
    tloc += clen + 1;		/* one for \0 */
    return;
}


/*ARGSUSED*/
void
TellTC(what)
    char   *what;
{
    struct termcapstr *t;

    xprintf("\n\tTcsh thinks your terminal has the\n");
    xprintf("\tfollowing characteristics:\n\n");
    xprintf("\tIt has %d columns and %d lines\n",
	    Val(T_co), Val(T_li));
    xprintf("\tIt has %s meta key\n", T_HasMeta ? "a" : "no");
    xprintf("\tIt can%suse tabs\n", T_Tabs ? " " : "not ");

    for (t = tstr; t->name != NULL; t++)
	xprintf("\t%25s (%s) == %s\n", t->long_name, t->name,
		t->str && *t->str ? t->str : "(empty)");
    xprintf("\n");
}


static void
ReBufferDisplay()
{
    register int i;
    Char  **b;
    Char  **bufp;

    b = Display;
    Display = NULL;
    if (b != NULL) {
	for (bufp = b; *bufp != NULL; bufp++)
	    xfree((ptr_t) * bufp);
	xfree((ptr_t) b);
    }
    b = Vdisplay;
    Vdisplay = NULL;
    if (b != NULL) {
	for (bufp = b; *bufp != NULL; bufp++)
	    xfree((ptr_t) * bufp);
	xfree((ptr_t) b);
    }
    /* make this public, -1 to avoid wraps */
    TermH = Val(T_co) - 1;
    TermV = (INBUFSIZE * 4) / TermH + 1;
    b = (Char **) xmalloc((size_t) (sizeof(Char *) * (TermV + 1)));
    for (i = 0; i < TermV; i++)
	b[i] = (Char *) xmalloc((size_t) (sizeof(Char) * (TermH + 1)));
    b[TermV] = NULL;
    Display = b;
    b = (Char **) xmalloc((size_t) (sizeof(Char *) * (TermV + 1)));
    for (i = 0; i < TermV; i++)
	b[i] = (Char *) xmalloc((size_t) (sizeof(Char) * (TermH + 1)));
    b[TermV] = NULL;
    Vdisplay = b;
}

void
SetTC(what, how)
    char   *what, *how;
{
    struct termcapstr *ts;
    struct termcapval *tv;

    /*
     * Do the strings first
     */
    setname("settc");
    for (ts = tstr; ts->name != NULL; ts++)
	if (strcmp(ts->name, what) == 0)
	    break;
    if (ts->name != NULL) {
	TCalloc(ts, how);
	/*
	 * Reset variables
	 */
	if (GoodStr(T_me) && GoodStr(T_ue))
	    me_all = (strcmp(Str(T_me), Str(T_ue)) == 0);
	else
	    me_all = 0;
	if (GoodStr(T_me) && GoodStr(T_se))
	    me_all |= (strcmp(Str(T_me), Str(T_se)) == 0);

	T_CanCEOL = GoodStr(T_ce);
	T_CanDel = GoodStr(T_dc) || GoodStr(T_DC);
	T_CanIns = GoodStr(T_im) || GoodStr(T_ic) || GoodStr(T_IC);
	T_CanUP = GoodStr(T_up) || GoodStr(T_UP);
	return;
    }

    /*
     * Do the numeric ones second
     */
    for (tv = tval; tv->name != NULL; tv++)
	if (strcmp(tv->name, what) == 0)
	    break;

    if (tv->name != NULL) {
	if (tv == &tval[T_pt] || tv == &tval[T_km]) {
	    if (strcmp(how, "yes") == 0)
		tv->val = 1;
	    else if (strcmp(how, "no") == 0)
		tv->val = 0;
	    else {
		stderror(ERR_SETTCUS, tv->name);
		return;
	    }
	    T_Tabs = Val(T_pt);
	    T_HasMeta = Val(T_km);
	    return;
	}
	else {
	    tv->val = atoi(how);
	    T_Cols = Val(T_co);
	    T_Lines = Val(T_li);
	    if (tv == &tval[T_co] || tv == &tval[T_li])
		ChangeSize(Val(T_li), Val(T_co));
	    return;
	}
    }
    stderror(ERR_NAME | ERR_TCCAP, what);
    return;
}


/*
 * Print the termcap string out with variable substitution
 */
void
EchoTC(v)
    Char  **v;
{
    char   *cap, *scap, cv[BUFSIZE];
    int     arg_need, arg_cols, arg_rows;
    int     verbose = 0, silent = 0;
    char   *area;
    static char *fmts = "%s\n", *fmtd = "%d\n";
    char    buf[TC_BUFSIZE];

    area = buf;

    setname("echotc");

    tglob(v);
    if (gflag) {
	v = globall(v);
	if (v == 0)
	    stderror(ERR_NAME | ERR_NOMATCH);
    }
    else
	v = gargv = saveblk(v);
    trim(v);

    if (!*v || *v[0] == '\0')
	return;
    if (v[0][0] == '-') {
	switch (v[0][1]) {
	case 'v':
	    verbose = 1;
	    break;
	case 's':
	    silent = 1;
	    break;
	default:
	    stderror(ERR_NAME | ERR_TCUSAGE);
	    break;
	}
	v++;
    }
    if (!*v || *v[0] == '\0')
	return;
    (void) strcpy(cv, short2str(*v));
    if (strcmp(cv, "tabs") == 0) {
	xprintf(fmts, T_Tabs ? "yes" : "no");
	flush();
	return;
    }
    else if (strcmp(cv, "meta") == 0) {
	xprintf(fmts, Val(T_km) ? "yes" : "no");
	flush();
	return;
    }
    else if (strcmp(cv, "baud") == 0) {
	int     i;

	for (i = 0; baud_rate[i].b_name != NULL; i++)
	    if (T_Speed == baud_rate[i].b_rate) {
		xprintf(fmts, baud_rate[i].b_name);
		flush();
		return;
	    }
	xprintf(fmtd, 0);
	flush();
	return;
    }
    else if (strcmp(cv, "rows") == 0 || strcmp(cv, "lines") == 0) {
	xprintf(fmtd, Val(T_li));
	flush();
	return;
    }
    else if (strcmp(cv, "cols") == 0) {
	xprintf(fmtd, Val(T_co));
	flush();
	return;
    }

    /*
     * Count home many values we need for this capability.
     */
    scap = tgetstr(cv, &area);
    if (!scap || scap[0] == '\0') {
	if (silent)
	    return;
	else
	    stderror(ERR_NAME | ERR_TCCAP, cv);
    }

    for (cap = scap, arg_need = 0; *cap; cap++)
	if (*cap == '%')
	    switch (*++cap) {
	    case 'd':
	    case '2':
	    case '3':
	    case '.':
	    case '+':
		arg_need++;
		break;
	    case '%':
	    case '>':
	    case 'i':
	    case 'r':
	    case 'n':
	    case 'B':
	    case 'D':
		break;
	    default:
		/*
		 * hpux has lot's of them...
		 */
		if (verbose)
		    stderror(ERR_NAME | ERR_TCPARM, *cap);
		/* This is bad, but I won't complain */
		break;
	    }

    switch (arg_need) {
    case 0:
	v++;
	if (*v && *v[0]) {
	    if (silent)
		return;
	    else
		stderror(ERR_NAME | ERR_TCARGS, cv, arg_need);
	}
	(void) tputs(scap, 1, putraw);
	break;
    case 1:
	v++;
	if (!*v || *v[0] == '\0')
	    stderror(ERR_NAME | ERR_TCNARGS, cv, 1);
	arg_rows = 0;
	arg_cols = atoi(short2str(*v));
	v++;
	if (*v && *v[0]) {
	    if (silent)
		return;
	    else
		stderror(ERR_NAME | ERR_TCARGS, cv, arg_need);
	}
	(void) tputs(tgoto(scap, arg_cols, arg_rows), 1, putraw);
	break;
    default:
	/* This is wrong, but I will ignore it... */
	if (verbose)
	    stderror(ERR_NAME | ERR_TCARGS, cv, arg_need);
    case 2:
	v++;
	if (!*v || *v[0] == '\0') {
	    if (silent)
		return;
	    else
		stderror(ERR_NAME | ERR_TCNARGS, cv, 2);
	}
	arg_cols = atoi(short2str(*v));
	v++;
	if (!*v || *v[0] == '\0') {
	    if (silent)
		return;
	    else
		stderror(ERR_NAME | ERR_TCNARGS, cv, 2);
	}
	arg_rows = atoi(short2str(*v));
	v++;
	if (*v && *v[0]) {
	    if (silent)
		return;
	    else
		stderror(ERR_NAME | ERR_TCARGS, cv, arg_need);
	}
	(void) tputs(tgoto(scap, arg_cols, arg_rows), arg_rows, putraw);
	break;
    }
    flush();
    if (gargv) {
	blkfree(gargv);
	gargv = 0;
    }
}

bool    GotTermCaps = 0;

void
BindArrowKeys()
{
    KEYCMD *map, *dmap;
    int     i, j;
    char   *p;
    static struct {
	int     key, fun;
    }       ar[] =
    {
	{ T_kd, F_DOWN_HIST },
	{ T_ku, F_UP_HIST   },
	{ T_kl, F_CHARBACK  },
	{ T_kr, F_CHARFWD   }
    };

    if (!GotTermCaps)
	return;
    map = VImode ? CcAltMap : CcKeyMap;
    dmap = VImode ? CcViCmdMap : CcEmacsMap;

    for (i = 0; i < 4; i++) {
	p = tstr[ar[i].key].str;
	if (p && *p) {
	    j = (unsigned char) *p;
	    /*
	     * Assign the arrow keys only if:
	     *
	     * 1. They are multi-character arrow keys and the user 
	     *    has not re-assigned the leading character, or 
	     *    has re-assigned the leading character to be F_XKEY
	     * 2. They are single arrow keys pointing to an unassigned key.
	     */
	    if (p[1] && (dmap[j] == map[j] || map[j] == F_XKEY)) {
		AddXkey(str2short(p), XmapCmd(ar[i].fun), XK_CMD);
		map[j] = F_XKEY;
	    }
	    else if (map[j] == F_UNASSIGNED) {
		ClearXkey(map, str2short(p));
		map[j] = ar[i].fun;
	    }
	}
    }
}

static Char cur_atr = 0;	/* current attributes */

void
SetAttributes(atr)
    int     atr;
{
    atr &= ATTRIBUTES;
    if (atr != cur_atr) {
	if (me_all && GoodStr(T_me)) {
	    if (((cur_atr & BOLD) && !(atr & BOLD)) ||
		((cur_atr & UNDER) && !(atr & UNDER)) ||
		((cur_atr & STANDOUT) && !(atr & STANDOUT))) {
		(void) tputs(Str(T_me), 1, putpure);
		cur_atr = 0;
	    }
	}
	if ((atr & BOLD) != (cur_atr & BOLD)) {
	    if (atr & BOLD) {
		if (GoodStr(T_md) && GoodStr(T_me)) {
		    (void) tputs(Str(T_md), 1, putpure);
		    cur_atr |= BOLD;
		}
	    }
	    else {
		if (GoodStr(T_md) && GoodStr(T_me)) {
		    (void) tputs(Str(T_me), 1, putpure);
		    if ((cur_atr & STANDOUT) && GoodStr(T_se)) {
			(void) tputs(Str(T_se), 1, putpure);
			cur_atr &= ~STANDOUT;
		    }
		    if ((cur_atr & UNDER) && GoodStr(T_ue)) {
			(void) tputs(Str(T_ue), 1, putpure);
			cur_atr &= ~UNDER;
		    }
		    cur_atr &= ~BOLD;
		}
	    }
	}
	if ((atr & STANDOUT) != (cur_atr & STANDOUT)) {
	    if (atr & STANDOUT) {
		if (GoodStr(T_so) && GoodStr(T_se)) {
		    (void) tputs(Str(T_so), 1, putpure);
		    cur_atr |= STANDOUT;
		}
	    }
	    else {
		if (GoodStr(T_se)) {
		    (void) tputs(Str(T_se), 1, putpure);
		    cur_atr &= ~STANDOUT;
		}
	    }
	}
	if ((atr & UNDER) != (cur_atr & UNDER)) {
	    if (atr & UNDER) {
		if (GoodStr(T_us) && GoodStr(T_ue)) {
		    (void) tputs(Str(T_us), 1, putpure);
		    cur_atr |= UNDER;
		}
	    }
	    else {
		if (GoodStr(T_ue)) {
		    (void) tputs(Str(T_ue), 1, putpure);
		    cur_atr &= ~UNDER;
		}
	    }
	}
    }
}

/* PWP 6-27-88 -- if the tty driver thinks that we can tab, we ask termcap */
int
CanWeTab()
{
    return (Val(T_pt));
}

void
MoveToLine(where)		/* move to line <where> (first line == 0) */
    int     where;		/* as efficiently as possible; */
{
    int     del, i;

    if (where == CursorV)
	return;

    if (where > TermV) {
#ifdef DEBUG_SCREEN
	xprintf("MoveToLine: where is ridiculous: %d\r\n", where);
	flush();
#endif /* DEBUG_SCREEN */
	return;
    }

    if ((del = where - CursorV) > 0) {
	if ((del > 1) && GoodStr(T_DO))
	    (void) tputs(tgoto(Str(T_DO), del, del), del, putpure);
	else {
	    for (i = 0; i < del; i++)
		(void) putraw('\n');
	    CursorH = 0;	/* because the \n will become \r\n */
	}
    }
    else {			/* del < 0 */
	if (GoodStr(T_UP) && (-del > 1 || !GoodStr(T_up)))
	    (void) tputs(tgoto(Str(T_UP), -del, -del), -del, putpure);
	else {
	    if (GoodStr(T_up))
		for (i = 0; i < -del; i++)
		    (void) tputs(Str(T_up), 1, putpure);
	}
    }
    CursorV = where;		/* now where is here */
}

void
MoveToChar(where)		/* move to character position (where) */
    int     where;
{				/* as efficiently as possible */
    int     del, i;

mc_again:
    if (where == CursorH)
	return;

    if (where > (TermH + 1)) {
#ifdef DEBUG_SCREEN
	xprintf("MoveToChar: where is riduculous: %d\r\n", where);
	flush();
#endif /* DEBUG_SCREEN */
	return;
    }

    if (!where) {		/* if where is first column */
	(void) putraw('\r');	/* do a CR */
	CursorH = 0;
	return;
    }

    del = where - CursorH;

    if ((del < -4 || del > 4) && GoodStr(T_ch))
	/* go there directly */
	(void) tputs(tgoto(Str(T_ch), where, where), where, putpure);
    else {
	if (del > 0) {		/* moving forward */
	    if ((del > 4) && GoodStr(T_RI))
		(void) tputs(tgoto(Str(T_RI), del, del), del, putpure);
	    else {
		if (T_Tabs) {	/* if I can do tabs, use them */
		    if ((CursorH & 0370) != (where & 0370)) {
			/* if not within tab stop */
			for (i = (CursorH & 0370); i < (where & 0370); i += 8)
			    (void) putraw('\t');	/* then tab over */
			CursorH = where & 0370;
		    }
		}
		/* it's usually cheaper to just write the chars, so we do. */

		/* NOTE THAT so_write() WILL CHANGE CursorH!!! */
		so_write(&Display[CursorV][CursorH], where - CursorH);

	    }
	}
	else {			/* del < 0 := moving backward */
	    if ((-del > 4) && GoodStr(T_LE))
		(void) tputs(tgoto(Str(T_LE), -del, -del), -del, putpure);
	    else {		/* can't go directly there */
		/* if the "cost" is greater than the "cost" from col 0 */
		if (T_Tabs ? (-del > ((where >> 3) + (where & 07)))
		    : (-del > where)) {
		    (void) putraw('\r');	/* do a CR */
		    CursorH = 0;
		    goto mc_again;	/* and try again */
		}
		for (i = 0; i < -del; i++)
		    (void) putraw('\b');
	    }
	}
    }
    CursorH = where;		/* now where is here */
}

void
so_write(cp, n)
    register Char *cp;
    register int n;
{
    if (n <= 0)
	return;			/* catch bugs */

    if (n > (TermH + 1)) {
#ifdef DEBUG_SCREEN
	xprintf("so_write: n is riduculous: %d\r\n", n);
	flush();
#endif /* DEBUG_SCREEN */
	return;
    }

    do {
	if (*cp & LITERAL) {
	    extern Char *litptr[];
	    Char   *d;

#ifdef DEBUG_LITERAL
	    xprintf("so: litnum %d, litptr %x\r\n",
		    *cp & CHAR, litptr[*cp & CHAR]);
#endif /* DEBUG_LITERAL */
	    for (d = litptr[*cp++ & CHAR]; *d & LITERAL; d++)
		(void) putraw(*d & CHAR);
	    (void) putraw(*d);

	}
	else
	    (void) putraw(*cp++);
	CursorH++;
    } while (--n);
}


void
DeleteChars(num)		/* deletes <num> characters */
    int     num;
{
    if (num <= 0)
	return;

    if (!T_CanDel) {
#ifdef DEBUG_EDIT
	xprintf("   ERROR: cannot delete   \n");
#endif /* DEBUG_EDIT */
	flush();
	return;
    }

    if (num > TermH) {
#ifdef DEBUG_SCREEN
	xprintf("DeleteChars: num is riduculous: %d\r\n", num);
	flush();
#endif /* DEBUG_SCREEN */
	return;
    }

    if (GoodStr(T_DC))		/* if I have multiple delete */
	if ((num > 1) || !GoodStr(T_dc)) {	/* if dc would be more expen. */
	    (void) tputs(tgoto(Str(T_DC), num, num), num, putpure);
	    return;
	}

    if (GoodStr(T_dm))		/* if I have delete mode */
	(void) tputs(Str(T_dm), 1, putpure);

    if (GoodStr(T_dc))		/* else do one at a time */
	while (num--)
	    (void) tputs(Str(T_dc), 1, putpure);

    if (GoodStr(T_ed))		/* if I have delete mode */
	(void) tputs(Str(T_ed), 1, putpure);
}

void
Insert_write(cp, num)		/* Puts terminal in insert character mode, */
    register Char *cp;
    register int num;		/* or inserts num characters in the line */
{
    if (num <= 0)
	return;
    if (!T_CanIns) {
#ifdef DEBUG_EDIT
	xprintf("   ERROR: cannot insert   \n");
#endif /* DEBUG_EDIT */
	flush();
	return;
    }

    if (num > TermH) {
#ifdef DEBUG_SCREEN
	xprintf("StartInsert: num is riduculous: %d\r\n", num);
	flush();
#endif /* DEBUG_SCREEN */
	return;
    }

    if (GoodStr(T_IC))		/* if I have multiple insert */
	if ((num > 1) || !GoodStr(T_ic)) {	/* if ic would be more expen. */
	    (void) tputs(tgoto(Str(T_IC), num, num), num, putpure);
	    so_write(cp, num);	/* this updates CursorH */
	    return;
	}

    if (GoodStr(T_im) && GoodStr(T_ei)) { /* if I have insert mode */
	(void) tputs(Str(T_im), 1, putpure);

	CursorH += num;
	do 
	    (void) putraw(*cp++);
	while (--num);

	if (GoodStr(T_ip))	/* have to make num chars insert */
	    (void) tputs(Str(T_ip), 1, putpure);

	(void) tputs(Str(T_ei), 1, putpure);
	return;
    }

    do {
	if (GoodStr(T_ic))	/* have to make num chars insert */
	    (void) tputs(Str(T_ic), 1, putpure);	/* insert a char */

	(void) putraw(*cp++);

	CursorH++;

	if (GoodStr(T_ip))	/* have to make num chars insert */
	    (void) tputs(Str(T_ip), 1, putpure);/* pad the inserted char */

    } while (--num);

}

void
ClearEOL(num)			/* clear to end of line.  There are num */
    int     num;		/* characters to clear */
{
    register int i;

    if (T_CanCEOL && GoodStr(T_ce))
	(void) tputs(Str(T_ce), 1, putpure);
    else {
	for (i = 0; i < num; i++)
	    (void) putraw(' ');
	CursorH += num;		/* have written num spaces */
    }
}

void
ClearScreen()
{				/* clear the whole screen and home */
    if (GoodStr(T_cl))
	/* send the clear screen code */
	(void) tputs(Str(T_cl), Val(T_li), putpure);
    else if (GoodStr(T_ho) && GoodStr(T_cd)) {
	(void) tputs(Str(T_ho), Val(T_li), putpure);	/* home */
	/* clear to bottom of screen */
	(void) tputs(Str(T_cd), Val(T_li), putpure);
    }
    else {
	(void) putraw('\r');
	(void) putraw('\n');
    }
}

void
Beep()
{				/* produce a sound */
    beep_cmd ();
    if (adrof(STRnobeep))
	return;

    if (GoodStr(T_vb) && adrof(STRvisiblebell))
	(void) tputs(Str(T_vb), 1, putpure);	/* visible bell */
    else if (GoodStr(T_bl))
	/* what termcap says we should use */
	(void) tputs(Str(T_bl), 1, putpure);
    else
	(void) putraw('\007');	/* an ASCII bell; ^G */
}

void
ClearToBottom()
{				/* clear to the bottom of the screen */
    if (GoodStr(T_cd))
	(void) tputs(Str(T_cd), Val(T_li), putpure);
    else if (GoodStr(T_ce))
	(void) tputs(Str(T_ce), Val(T_li), putpure);
}

void
GetTermCaps()
{				/* read in the needed terminal capabilites */
    register int i;
    char   *ptr;
    char    buf[TC_BUFSIZE];
    static char bp[TC_BUFSIZE];
    char   *area;
    extern char *getenv();
    struct termcapstr *t;


#ifdef SIG_WINDOW
# ifdef BSDSIGS
    sigmask_t omask;
# endif /* BSDSIGS */
    int     lins, cols;

    /* don't want to confuse things here */
# ifdef BSDSIGS
    omask = sigblock(sigmask(SIG_WINDOW)) & ~sigmask(SIG_WINDOW);
# else /* BSDSIGS */
    (void) sighold(SIG_WINDOW);
# endif /* BSDSIGS */
#endif /* SIG_WINDOW */
    area = buf;

    GotTermCaps = 1;

    setname("gettermcaps");
    ptr = getenv("TERM");

#ifdef apollo
    /*
     * If we are on a pad, we pretend that we are dumb. Otherwise the termcap
     * library will put us in a weird screen mode, thinking that we are going
     * to use curses
     */
    if (isapad())
	ptr = "dumb";
#endif /* apollo */

    if (!ptr || !ptr[0])
	ptr = "dumb";

    setzero(bp, TC_BUFSIZE);

    i = tgetent(bp, ptr);
    if (i <= 0) {
	if (i == -1) {
#if (SVID == 0) || defined(IRIS3D)
	    xprintf("tcsh: Cannot open /etc/termcap.\n");
	}
	else if (i == 0) {
#endif /* SVID */
	    xprintf("tcsh: No entry for terminal type \"%s\"\n",
		    getenv("TERM"));
	}
	xprintf("tcsh: using dumb terminal settings.\n");
	Val(T_co) = 80;		/* do a dumb terminal */
	Val(T_pt) = Val(T_km) = Val(T_li) = 0;
	for (t = tstr; t->name != NULL; t++)
	    TCalloc(t, NULL);
    }
    else {
	/* Can we tab */
	Val(T_pt) = tgetflag("pt") && !tgetflag("xt");
	/* do we have a meta? */
	Val(T_km) = (tgetflag("km") || tgetflag("MT"));
	Val(T_co) = tgetnum("co");
	Val(T_li) = tgetnum("li");
	for (t = tstr; t->name != NULL; t++)
	    TCalloc(t, tgetstr(t->name, &area));
    }
    if (Val(T_co) < 2)
	Val(T_co) = 80;		/* just in case */
    if (Val(T_li) < 1)
	Val(T_li) = 24;

    T_Cols = Val(T_co);
    T_Lines = Val(T_li);
    if (T_Tabs)
	T_Tabs = Val(T_pt);
    T_HasMeta = Val(T_km);
    T_CanCEOL = GoodStr(T_ce);
    T_CanDel = GoodStr(T_dc) || GoodStr(T_DC);
    T_CanIns = GoodStr(T_im) || GoodStr(T_ic) || GoodStr(T_IC);
    T_CanUP = GoodStr(T_up) || GoodStr(T_UP);
    if (GoodStr(T_me) && GoodStr(T_ue))
	me_all = (strcmp(Str(T_me), Str(T_ue)) == 0);
    else
	me_all = 0;
    if (GoodStr(T_me) && GoodStr(T_se))
	me_all |= (strcmp(Str(T_me), Str(T_se)) == 0);


#ifdef DEBUG_SCREEN
    if (!T_CanUP) {
	xprintf("tcsh: WARNING: Your terminal cannot move up.\n");
	xprintf("Editing may be odd for long lines.\n");
    }
    if (!T_CanCEOL)
	xprintf("no clear EOL capability.\n");
    if (!T_CanDel)
	xprintf("no delete char capability.\n");
    if (!T_CanIns)
	xprintf("no insert char capability.\n");
#endif /* DEBUG_SCREEN */



#ifdef SIG_WINDOW
    (void) GetSize(&lins, &cols);	/* get the correct window size */
    ChangeSize(lins, cols);

# ifdef BSDSIGS
    (void) sigsetmask(omask);	/* can change it again */
# else /* BSDSIGS */
    (void) sigrelse(SIG_WINDOW);
# endif /* BSDSIGS */
#else /* SIG_WINDOW */
    ChangeSize(Val(T_li), Val(T_co));
#endif /* SIG_WINDOW */

    BindArrowKeys();
}

#ifdef SIG_WINDOW
/* GetSize():
 *	Return the new window size in lines and cols, and
 *	true if the size was changed. This can fail if SHIN
 *	is not a tty, but it will work in most cases.
 */
int
GetSize(lins, cols)
    int    *lins, *cols;
{
    *cols = Val(T_co);
    *lins = Val(T_li);

#ifdef TIOCGWINSZ
# define KNOWsize
# ifndef lint
    {
	struct winsize ws;	/* from 4.3 */

	if (ioctl(SHIN, TIOCGWINSZ, (ioctl_t) &ws) != -1) {
	    if (ws.ws_col)
		*cols = ws.ws_col;
	    if (ws.ws_row)
		*lins = ws.ws_row;
	}
    }
# endif /* !lint */
#else /* TIOCGWINSZ */
# ifdef TIOCGSIZE
#  define KNOWsize
    {
	struct ttysize ts;	/* from Sun */

	if (ioctl(SHIN, TIOCGSIZE, (ioctl_t) &ts) != -1) {
	    if (ts.ts_cols)
		*cols = ts.ts_cols;
	    if (ts.ts_lines)
		*lins = ts.ts_lines;
	}
    }
# endif /* TIOCGSIZE */
#endif /* TIOCGWINSZ */

    return (Val(T_co) != *cols || Val(T_li) != *lins);
}

#endif /* SIGWINDOW */

void
ChangeSize(lins, cols)
    int     lins, cols;
{
    /*
     * Just in case
     */
    Val(T_co) = (cols < 2) ? 80 : cols;
    Val(T_li) = (lins < 1) ? 24 : lins;

#ifdef KNOWsize
    /*
     * We want to affect the environment only when we have a valid
     * setup, not when we get bad settings. Consider the following scenario:
     * We just logged in, and we have not initialized the editor yet.
     * We reset termcap with tset, and not $TERMCAP has the right
     * terminal size. But since the editor is not initialized yet, and
     * the kernel's notion of the terminal size might be wrong we arrive
     * here with lines = columns = 0. If we reset the environment we lose
     * our only chance to get the window size right.
     */
    if (Val(T_co) == cols && Val(T_li) == lins) {
	Char    buf[10];
	char   *tptr;

	if (getenv("COLUMNS")) {
	    Itoa(Val(T_co), buf);
	    Setenv(STRCOLUMNS, buf);
	}

	if (getenv("LINES")) {
	    Itoa(Val(T_li), buf);
	    Setenv(STRLINES, buf);
	}

	if (tptr = getenv("TERMCAP")) {
	    Char    termcap[1024], backup[1024], *ptr;
	    int     i;

	    ptr = str2short(tptr);
	    (void) Strncpy(termcap, ptr, 1024);
	    termcap[1023] = '\0';

	    /* update termcap string; first do columns */
	    buf[0] = 'c';
	    buf[1] = 'o';
	    buf[2] = '#';
	    buf[3] = '\0';
	    if ((ptr = Strstr(termcap, buf)) == NULL) {
		(void) Strcpy(backup, termcap);
	    }
	    else {
		i = ptr - termcap + Strlen(buf);
		(void) Strncpy(backup, termcap, i);
		backup[i] = '\0';
		Itoa(Val(T_co), buf);
		(void) Strcat(backup + i, buf);
		ptr = Strchr(ptr, ':');
		(void) Strcat(backup, ptr);
	    }

	    /* now do lines */
	    buf[0] = 'l';
	    buf[1] = 'i';
	    buf[2] = '#';
	    buf[3] = '\0';
	    if ((ptr = Strstr(backup, buf)) == NULL) {
		(void) Strcpy(termcap, backup);
	    }
	    else {
		i = ptr - backup + Strlen(buf);
		(void) Strncpy(termcap, backup, i);
		termcap[i] = '\0';
		Itoa(Val(T_li), buf);
		(void) Strcat(termcap, buf);
		ptr = Strchr(ptr, ':');
		(void) Strcat(termcap, ptr);
	    }
	    Setenv(STRTERMCAP, termcap);
	}
    }
#endif /* KNOWsize */

    ReBufferDisplay();		/* re-make display buffers */
    ClearDisp();
}
