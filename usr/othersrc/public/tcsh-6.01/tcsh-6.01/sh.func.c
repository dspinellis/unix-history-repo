/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/sh.func.c,v 3.20 1991/12/19 22:34:14 christos Exp $ */
/*
 * sh.func.c: csh builtin functions
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

RCSID("$Id: sh.func.c,v 3.20 1991/12/19 22:34:14 christos Exp $")

#include "ed.h"
#include "tw.h"
#include "tc.h"

/*
 * C shell
 */
extern int just_signaled;
extern char **environ;

extern bool MapsAreInited;
extern bool NLSMapsAreInited;
extern bool NoNLSRebind;

static int zlast = -1;

static	void	islogin		__P((void));
static	void	reexecute	__P((struct command *));
static	void	preread		__P((void));
static	void	doagain		__P((void));
static  char   *isrchx		__P((int));
static	void	search		__P((int, int, Char *));
static	int	getword		__P((Char *));
static	int	keyword		__P((Char *));
static	void	toend		__P((void));
static	void	xecho		__P((int, Char **));

struct biltins *
isbfunc(t)
    struct command *t;
{
    register Char *cp = t->t_dcom[0];
    register struct biltins *bp, *bp1, *bp2;
    static struct biltins label = {"", dozip, 0, 0};
    static struct biltins foregnd = {"%job", dofg1, 0, 0};
    static struct biltins backgnd = {"%job &", dobg1, 0, 0};

    if (lastchr(cp) == ':') {
	label.bname = short2str(cp);
	return (&label);
    }
    if (*cp == '%') {
	if (t->t_dflg & F_AMPERSAND) {
	    t->t_dflg &= ~F_AMPERSAND;
	    backgnd.bname = short2str(cp);
	    return (&backgnd);
	}
	foregnd.bname = short2str(cp);
	return (&foregnd);
    }
#ifdef WARP
    /*
     * This is a perhaps kludgy way to determine if the warp builtin is to be
     * acknowledged or not.  If checkwarp() fails, then we are to assume that
     * the warp command is invalid, and carry on as we would handle any other
     * non-builtin command.         -- JDK 2/4/88
     */
    if (eq(STRwarp, cp) && !checkwarp()) {
	return (0);		/* this builtin disabled */
    }
#endif /* WARP */
    /*
     * Binary search Bp1 is the beginning of the current search range. Bp2 is
     * one past the end.
     */
    for (bp1 = bfunc, bp2 = bfunc + nbfunc; bp1 < bp2;) {
	register i;

	bp = bp1 + ((bp2 - bp1) >> 1);
	if ((i = *cp - *bp->bname) == 0 &&
	    (i = Strcmp(cp, str2short(bp->bname))) == 0)
	    return bp;
	if (i < 0)
	    bp2 = bp;
	else
	    bp1 = bp + 1;
    }
    return (0);
}

void
func(t, bp)
    register struct command *t;
    register struct biltins *bp;
{
    int     i;

    xechoit(t->t_dcom);
    setname(bp->bname);
    i = blklen(t->t_dcom) - 1;
    if (i < bp->minargs)
	stderror(ERR_NAME | ERR_TOOFEW);
    if (i > bp->maxargs)
	stderror(ERR_NAME | ERR_TOOMANY);
    (*bp->bfunct) (t->t_dcom, t);
}

/*ARGSUSED*/
void
doonintr(v, c)
    Char  **v;
    struct command *c;
{
    register Char *cp;
    register Char *vv = v[1];

    if (parintr == SIG_IGN)
	return;
    if (setintr && intty)
	stderror(ERR_NAME | ERR_TERMINAL);
    cp = gointr;
    gointr = 0;
    xfree((ptr_t) cp);
    if (vv == 0) {
#ifdef BSDSIGS
	if (setintr)
	    (void) sigblock(sigmask(SIGINT));
	else
	    (void) signal(SIGINT, SIG_DFL);
#else /* !BSDSIGS */
	if (setintr)
	    (void) sighold(SIGINT);
	else
	    (void) sigset(SIGINT, SIG_DFL);
#endif /* BSDSIGS */
	gointr = 0;
    }
    else if (eq((vv = strip(vv)), STRminus)) {
#ifdef BSDSIGS
	(void) signal(SIGINT, SIG_IGN);
#else /* !BSDSIGS */
	(void) sigset(SIGINT, SIG_IGN);
#endif /* BSDSIGS */
	gointr = Strsave(STRminus);
    }
    else {
	gointr = Strsave(vv);
#ifdef BSDSIGS
	(void) signal(SIGINT, pintr);
#else /* !BSDSIGS */
	(void) sigset(SIGINT, pintr);
#endif /* BSDSIGS */
    }
}

/*ARGSUSED*/
void
donohup(v, c)
    Char **v;
    struct command *c;
{
    if (intty)
	stderror(ERR_NAME | ERR_TERMINAL);
    if (setintr == 0) {
	(void) signal(SIGHUP, SIG_IGN);
#ifdef CC
	submit(getpid());
#endif /* CC */
    }
}

/*ARGSUSED*/
void
dozip(v, c)
    Char **v;
    struct command *c;
{
    ;
}

void
prvars()
{
    plist(&shvhed);
}

/*ARGSUSED*/
void
doalias(v, c)
    register Char **v;
    struct command *c;
{
    register struct varent *vp;
    register Char *p;

    v++;
    p = *v++;
    if (p == 0)
	plist(&aliases);
    else if (*v == 0) {
	vp = adrof1(strip(p), &aliases);
	if (vp)
	    blkpr(vp->vec), xprintf("\n");
    }
    else {
	if (eq(p, STRalias) || eq(p, STRunalias)) {
	    setname(short2str(p));
	    stderror(ERR_NAME | ERR_DANGER);
	}
	set1(strip(p), saveblk(v), &aliases);
	tw_clear_comm_list();
    }
}

/*ARGSUSED*/
void
unalias(v, c)
    Char  **v;
    struct command *c;
{
    unset1(v, &aliases);
    tw_clear_comm_list();
}

/*ARGSUSED*/
void
dologout(v, c)
    Char **v;
    struct command *c;
{
    islogin();
    goodbye(NULL, NULL);
}

/*ARGSUSED*/
void
dologin(v, c)
    Char  **v;
    struct command *c;
{
    islogin();
    rechist();
    (void) signal(SIGTERM, parterm);
    (void) execl(_PATH_LOGIN, "login", short2str(v[1]), NULL);
    untty();
    xexit(1);
}


#ifdef NEWGRP
/*ARGSUSED*/
void
donewgrp(v, c)
    Char  **v;
    struct command *c;
{
    if (chkstop == 0 && setintr)
	panystop(0);
    (void) signal(SIGTERM, parterm);
    (void) execl(_PATH_BIN_NEWGRP, "newgrp", short2str(v[1]), NULL);
    (void) execl(_PATH_USRBIN_NEWGRP, "newgrp", short2str(v[1]), NULL);
    untty();
    xexit(1);
}
#endif /* NEWGRP */

static void
islogin()
{
    if (chkstop == 0 && setintr)
	panystop(0);
    if (loginsh)
	return;
    stderror(ERR_NOTLOGIN);
}

void
doif(v, kp)
    Char  **v;
    struct command *kp;
{
    register int i;
    register Char **vv;

    v++;
    i = expr(&v);
    vv = v;
    if (*vv == NULL)
	stderror(ERR_NAME | ERR_EMPTYIF);
    if (eq(*vv, STRthen)) {
	if (*++vv)
	    stderror(ERR_NAME | ERR_IMPRTHEN);
	setname(short2str(STRthen));
	/*
	 * If expression was zero, then scan to else , otherwise just fall into
	 * following code.
	 */
	if (!i)
	    search(T_IF, 0, NULL);
	return;
    }
    /*
     * Simple command attached to this if. Left shift the node in this tree,
     * munging it so we can reexecute it.
     */
    if (i) {
	lshift(kp->t_dcom, vv - kp->t_dcom);
	reexecute(kp);
	donefds();
    }
}

/*
 * Reexecute a command, being careful not
 * to redo i/o redirection, which is already set up.
 */
static void
reexecute(kp)
    register struct command *kp;
{
    kp->t_dflg &= F_SAVE;
    kp->t_dflg |= F_REPEAT;
    /*
     * If tty is still ours to arbitrate, arbitrate it; otherwise dont even set
     * pgrp's as the jobs would then have no way to get the tty (we can't give
     * it to them, and our parent wouldn't know their pgrp, etc.
     */
    execute(kp, (tpgrp > 0 ? tpgrp : -1), NULL, NULL);
}

/*ARGSUSED*/
void
doelse (v, c)
    Char **v;
    struct command *c;
{
    search(T_ELSE, 0, NULL);
}

/*ARGSUSED*/
void
dogoto(v, c)
    Char  **v;
    struct command *c;
{
    Char   *lp;

    gotolab(lp = globone(v[1], G_ERROR));
    xfree((ptr_t) lp);
}

void
gotolab(lab)
    Char *lab;
{
    register struct whyle *wp;
    /*
     * While we still can, locate any unknown ends of existing loops. This
     * obscure code is the WORST result of the fact that we don't really parse.
     */
    zlast = T_GOTO;
    for (wp = whyles; wp; wp = wp->w_next)
	if (wp->w_end.type == F_SEEK && wp->w_end.f_seek == 0) {
	    search(T_BREAK, 0, NULL);
	    btell(&wp->w_end);
	}
	else {
	    bseek(&wp->w_end);
	}
    search(T_GOTO, 0, lab);
    /*
     * Eliminate loops which were exited.
     */
    wfree();
}

/*ARGSUSED*/
void
doswitch(v, c)
    register Char **v;
    struct command *c;
{
    register Char *cp, *lp;

    v++;
    if (!*v || *(*v++) != '(')
	stderror(ERR_SYNTAX);
    cp = **v == ')' ? STRNULL : *v++;
    if (*(*v++) != ')')
	v--;
    if (*v)
	stderror(ERR_SYNTAX);
    search(T_SWITCH, 0, lp = globone(cp, G_ERROR));
    xfree((ptr_t) lp);
}

/*ARGSUSED*/
void
dobreak(v, c)
    Char **v;
    struct command *c;
{
    if (whyles)
	toend();
    else
	stderror(ERR_NAME | ERR_NOTWHILE);
}

/*ARGSUSED*/
void
doexit(v, c)
    Char  **v;
    struct command *c;
{
    if (chkstop == 0 && (intty || intact) && evalvec == 0)
	panystop(0);
    /*
     * Don't DEMAND parentheses here either.
     */
    v++;
    if (*v) {
	set(STRstatus, putn(expr(&v)));
	if (*v)
	    stderror(ERR_NAME | ERR_EXPRESSION);
    }
    btoeof();
    if (intty)
	(void) close(SHIN);
}

/*ARGSUSED*/
void
doforeach(v, c)
    register Char **v;
    struct command *c;
{
    register Char *cp, *sp;
    register struct whyle *nwp;

    v++;
    sp = cp = strip(*v);
    if (!letter(*sp))
	stderror(ERR_NAME | ERR_VARBEGIN);
    while (*cp && alnum(*cp))
	cp++;
    if (*cp)
	stderror(ERR_NAME | ERR_VARALNUM);
    if ((cp - sp) > MAXVARLEN)
	stderror(ERR_NAME | ERR_VARTOOLONG);
    cp = *v++;
    if (v[0][0] != '(' || v[blklen(v) - 1][0] != ')')
	stderror(ERR_NAME | ERR_NOPAREN);
    v++;
    gflag = 0, tglob(v);
    v = globall(v);
    if (v == 0)
	stderror(ERR_NAME | ERR_NOMATCH);
    nwp = (struct whyle *) xcalloc(1, sizeof *nwp);
    nwp->w_fe = nwp->w_fe0 = v;
    gargv = 0;
    btell(&nwp->w_start);
    nwp->w_fename = Strsave(cp);
    nwp->w_next = whyles;
    nwp->w_end.type = F_SEEK;
    whyles = nwp;
    /*
     * Pre-read the loop so as to be more comprehensible to a terminal user.
     */
    zlast = T_FOREACH;
    if (intty)
	preread();
    doagain();
}

/*ARGSUSED*/
void
dowhile(v, c)
    Char  **v;
    struct command *c;
{
    register int status;
    register bool again = whyles != 0 && SEEKEQ(&whyles->w_start, &lineloc) &&
    whyles->w_fename == 0;

    v++;
    /*
     * Implement prereading here also, taking care not to evaluate the
     * expression before the loop has been read up from a terminal.
     */
    if (intty && !again)
	status = !exp0(&v, 1);
    else
	status = !expr(&v);
    if (*v)
	stderror(ERR_NAME | ERR_EXPRESSION);
    if (!again) {
	register struct whyle *nwp =
	(struct whyle *) xcalloc(1, sizeof(*nwp));

	nwp->w_start = lineloc;
	nwp->w_end.type = F_SEEK;
	nwp->w_end.f_seek = 0;
	nwp->w_next = whyles;
	whyles = nwp;
	zlast = T_WHILE;
	if (intty) {
	    /*
	     * The tty preread
	     */
	    preread();
	    doagain();
	    return;
	}
    }
    if (status)
	/* We ain't gonna loop no more, no more! */
	toend();
}

static void
preread()
{
    whyles->w_end.type = I_SEEK;
    if (setintr)
#ifdef BSDSIGS
	(void) sigsetmask(sigblock((sigmask_t) 0) & ~sigmask(SIGINT));
#else /* !BSDSIGS */
	(void) sigrelse (SIGINT);
#endif /* BSDSIGS */
    search(T_BREAK, 0, NULL);		/* read the expression in */
    if (setintr)
#ifdef BSDSIGS
	(void) sigblock(sigmask(SIGINT));
#else /* !BSDSIGS */
	(void) sighold(SIGINT);
#endif /* BSDSIGS */
    btell(&whyles->w_end);
}

/*ARGSUSED*/
void
doend(v, c)
    Char **v;
    struct command *c;
{
    if (!whyles)
	stderror(ERR_NAME | ERR_NOTWHILE);
    btell(&whyles->w_end);
    doagain();
}

/*ARGSUSED*/
void
docontin(v, c)
    Char **v;
    struct command *c;
{
    if (!whyles)
	stderror(ERR_NAME | ERR_NOTWHILE);
    doagain();
}

static void
doagain()
{
    /* Repeating a while is simple */
    if (whyles->w_fename == 0) {
	bseek(&whyles->w_start);
	return;
    }
    /*
     * The foreach variable list actually has a spurious word ")" at the end of
     * the w_fe list.  Thus we are at the of the list if one word beyond this
     * is 0.
     */
    if (!whyles->w_fe[1]) {
	dobreak(NULL, NULL);
	return;
    }
    set(whyles->w_fename, Strsave(*whyles->w_fe++));
    bseek(&whyles->w_start);
}

void
dorepeat(v, kp)
    Char  **v;
    struct command *kp;
{
    register int i;

#ifdef BSDSIGS
    register sigmask_t omask = 0;

#endif /* BSDSIGS */

    i = getn(v[1]);
    if (setintr)
#ifdef BSDSIGS
	omask = sigblock(sigmask(SIGINT)) & ~sigmask(SIGINT);
#else /* !BSDSIGS */
	(void) sighold(SIGINT);
#endif /* BSDSIGS */
    lshift(v, 2);
    while (i > 0) {
	if (setintr)
#ifdef BSDSIGS
	    (void) sigsetmask(omask);
#else /* !BSDSIGS */
	    (void) sigrelse (SIGINT);
#endif /* BSDSIGS */
	reexecute(kp);
	--i;
    }
    donefds();
    if (setintr)
#ifdef BSDSIGS
	(void) sigsetmask(omask);
#else /* !BSDSIGS */
	(void) sigrelse (SIGINT);
#endif /* BSDSIGS */
}

/*ARGSUSED*/
void
doswbrk(v, c)
    Char **v;
    struct command *c;
{
    search(T_BRKSW, 0, NULL);
}

int
srchx(cp)
    register Char *cp;
{
    register struct srch *sp, *sp1, *sp2;
    register i;

    /*
     * Binary search Sp1 is the beginning of the current search range. Sp2 is
     * one past the end.
     */
    for (sp1 = srchn, sp2 = srchn + nsrchn; sp1 < sp2;) {
	sp = sp1 + ((sp2 - sp1) >> 1);
	if ((i = *cp - *sp->s_name) == 0 &&
	    (i = Strcmp(cp, str2short(sp->s_name))) == 0)
	    return sp->s_value;
	if (i < 0)
	    sp2 = sp;
	else
	    sp1 = sp + 1;
    }
    return (-1);
}

static char *
isrchx(n)
    register int n;
{
    register struct srch *sp, *sp2;

    for (sp = srchn, sp2 = srchn + nsrchn; sp < sp2; sp++)
	if (sp->s_value == n)
	    return (sp->s_name);
    return ("");
}


static Char Stype;
static Char *Sgoal;

static void
search(type, level, goal)
    int     type;
    register int level;
    Char   *goal;
{
    Char    wordbuf[BUFSIZE];
    register Char *aword = wordbuf;
    register Char *cp;

    Stype = type;
    Sgoal = goal;
    if (type == T_GOTO) {
	struct Ain a;
	a.type = F_SEEK;
	a.f_seek = 0;
	bseek(&a);
    }
    do {
	if (intty && fseekp == feobp && aret == F_SEEK)
	    printprompt(1, str2short(isrchx(type == T_BREAK ?
					    zlast : type)));
	/* xprintf("? "), flush(); */
	aword[0] = 0;
	(void) getword(aword);
	switch (srchx(aword)) {

	case T_ELSE:
	    if (level == 0 && type == T_IF)
		return;
	    break;

	case T_IF:
	    while (getword(aword))
		continue;
	    if ((type == T_IF || type == T_ELSE) &&
		eq(aword, STRthen))
		level++;
	    break;

	case T_ENDIF:
	    if (type == T_IF || type == T_ELSE)
		level--;
	    break;

	case T_FOREACH:
	case T_WHILE:
	    if (type == T_BREAK)
		level++;
	    break;

	case T_END:
	    if (type == T_BREAK)
		level--;
	    break;

	case T_SWITCH:
	    if (type == T_SWITCH || type == T_BRKSW)
		level++;
	    break;

	case T_ENDSW:
	    if (type == T_SWITCH || type == T_BRKSW)
		level--;
	    break;

	case T_LABEL:
	    if (type == T_GOTO && getword(aword) && eq(aword, goal))
		level = -1;
	    break;

	default:
	    if (type != T_GOTO && (type != T_SWITCH || level != 0))
		break;
	    if (lastchr(aword) != ':')
		break;
	    aword[Strlen(aword) - 1] = 0;
	    if ((type == T_GOTO && eq(aword, goal)) ||
		(type == T_SWITCH && eq(aword, STRdefault)))
		level = -1;
	    break;

	case T_CASE:
	    if (type != T_SWITCH || level != 0)
		break;
	    (void) getword(aword);
	    if (lastchr(aword) == ':')
		aword[Strlen(aword) - 1] = 0;
	    cp = strip(Dfix1(aword));
	    if (Gmatch(goal, cp))
		level = -1;
	    xfree((ptr_t) cp);
	    break;

	case T_DEFAULT:
	    if (type == T_SWITCH && level == 0)
		level = -1;
	    break;
	}
	(void) getword(NULL);
    } while (level >= 0);
}

static int
getword(wp)
    register Char *wp;
{
    register int found = 0;
    register int c, d;
    int     kwd = 0;
    Char   *owp = wp;

    c = readc(1);
    d = 0;
    do {
	while (c == ' ' || c == '\t')
	    c = readc(1);
	if (c == '#')
	    do
		c = readc(1);
	    while (c >= 0 && c != '\n');
	if (c < 0)
	    goto past;
	if (c == '\n') {
	    if (wp)
		break;
	    return (0);
	}
	unreadc(c);
	found = 1;
	do {
	    c = readc(1);
	    if (c == '\\' && (c = readc(1)) == '\n')
		c = ' ';
	    if (c == '\'' || c == '"')
		if (d == 0)
		    d = c;
		else if (d == c)
		    d = 0;
	    if (c < 0)
		goto past;
	    if (wp) {
		*wp++ = c;
		*wp = 0;	/* end the string b4 test */
	    }
	} while ((d || (!(kwd = keyword(owp)) && c != ' '
		  && c != '\t')) && c != '\n');
    } while (wp == 0);

    /*
     * if we have read a keyword ( "if", "switch" or "while" ) then we do not
     * need to unreadc the look-ahead char
     */
    if (!kwd) {
	unreadc(c);
	if (found)
	    *--wp = 0;
    }

    return (found);

past:
    switch (Stype) {

    case T_IF:
	stderror(ERR_NAME | ERR_NOTFOUND, "then/endif");

    case T_ELSE:
	stderror(ERR_NAME | ERR_NOTFOUND, "endif");

    case T_BRKSW:
    case T_SWITCH:
	stderror(ERR_NAME | ERR_NOTFOUND, "endsw");

    case T_BREAK:
	stderror(ERR_NAME | ERR_NOTFOUND, "end");

    case T_GOTO:
	setname(short2str(Sgoal));
	stderror(ERR_NAME | ERR_NOTFOUND, "label");

    default:
	break;
    }
    /* NOTREACHED */
    return (0);
}

/*
 * keyword(wp) determines if wp is one of the built-n functions if,
 * switch or while. It seems that when an if statement looks like
 * "if(" then getword above sucks in the '(' and so the search routine
 * never finds what it is scanning for. Rather than rewrite doword, I hack
 * in a test to see if the string forms a keyword. Then doword stops
 * and returns the word "if" -strike
 */

static int
keyword(wp)
    Char   *wp;
{
    static Char STRif[] = {'i', 'f', '\0'};
    static Char STRwhile[] = {'w', 'h', 'i', 'l', 'e', '\0'};
    static Char STRswitch[] = {'s', 'w', 'i', 't', 'c', 'h', '\0'};

    if (!wp)
	return (0);

    if ((Strcmp(wp, STRif) == 0) || (Strcmp(wp, STRwhile) == 0)
	|| (Strcmp(wp, STRswitch) == 0))
	return (1);

    return (0);
}

static void
toend()
{
    if (whyles->w_end.type == F_SEEK && whyles->w_end.f_seek == 0) {
	search(T_BREAK, 0, NULL);
	btell(&whyles->w_end);
	whyles->w_end.f_seek--;
    }
    else {
	bseek(&whyles->w_end);
    }
    wfree();
}

void
wfree()
{
    struct Ain    o;
    struct whyle *nwp;

#ifdef FDEBUG
    static char foo[] = "IAFE";
#endif /* FDEBUG */

    btell(&o);

#ifdef FDEBUG
    xprintf("o->type %c o->a_seek %d o->f_seek %d\n", 
	    foo[o.type + 1], o.a_seek, o.f_seek);
#endif /* FDEBUG */

    for (; whyles; whyles = nwp) {
	register struct whyle *wp = whyles;
	nwp = wp->w_next;

#ifdef FDEBUG
	xprintf("start->type %c start->a_seek %d start->f_seek %d\n", 
		foo[wp->w_start.type+1], 
		wp->w_start.a_seek, wp->w_start.f_seek);
	xprintf("end->type %c end->a_seek %d end->f_seek %d\n", 
		foo[wp->w_end.type + 1], wp->w_end.a_seek, wp->w_end.f_seek);
#endif /* FDEBUG */

	/*
	 * XXX: We free loops that have different seek types.
	 */
	if (wp->w_end.type != I_SEEK && wp->w_start.type == wp->w_end.type &&
	    wp->w_start.type == o.type) {
	    if (wp->w_end.type == F_SEEK) {
		if (o.f_seek >= wp->w_start.f_seek && 
		    (wp->w_end.f_seek == 0 || o.f_seek < wp->w_end.f_seek))
		    break;
	    }
	    else {
		if (o.a_seek >= wp->w_start.a_seek && 
		    (wp->w_end.a_seek == 0 || o.a_seek < wp->w_end.a_seek))
		    break;
	    }
	}

	if (wp->w_fe0)
	    blkfree(wp->w_fe0);
	if (wp->w_fename)
	    xfree((ptr_t) wp->w_fename);
	xfree((ptr_t) wp);
    }
}

/*ARGSUSED*/
void
doecho(v, c)
    Char  **v;
    struct command *c;
{
    xecho(' ', v);
}

/*ARGSUSED*/
void
doglob(v, c)
    Char  **v;
    struct command *c;
{
    xecho(0, v);
    flush();
}

static void
xecho(sep, v)
    int    sep;
    register Char **v;
{
    register Char *cp;
    int     nonl = 0;

    if (setintr)
#ifdef BSDSIGS
	(void) sigsetmask(sigblock((sigmask_t) 0) & ~sigmask(SIGINT));
#else /* !BSDSIGS */
	(void) sigrelse (SIGINT);
#endif /* BSDSIGS */
    v++;
    if (*v == 0)
	return;
    gflag = 0, tglob(v);
    if (gflag) {
	v = globall(v);
	if (v == 0)
	    stderror(ERR_NAME | ERR_NOMATCH);
    }
    else {
	v = gargv = saveblk(v);
	trim(v);
    }
    if (sep == ' ' && *v && eq(*v, STRmn))
	nonl++, v++;
    while (cp = *v++) {
	register int c;

	while (c = *cp++) {
#if SVID > 0
#ifndef OREO
	    if (c == '\\') {
		switch (c = *cp++) {
		case 'b':
		    c = '\b';
		    break;
		case 'c':
		    nonl = 1;
		    goto done;
		case 'f':
		    c = '\f';
		    break;
		case 'n':
		    c = '\n';
		    break;
		case 'r':
		    c = '\r';
		    break;
		case 't':
		    c = '\t';
		    break;
		case 'v':
		    c = '\v';
		    break;
		case '\\':
		    c = '\\';
		    break;
		case '0':
		    c = 0;
		    if (*cp >= '0' && *cp < '8')
			c = c * 8 + *cp++ - '0';
		    if (*cp >= '0' && *cp < '8')
			c = c * 8 + *cp++ - '0';
		    if (*cp >= '0' && *cp < '8')
			c = c * 8 + *cp++ - '0';
		    break;
		case '\0':
		    c = *--cp;
		    break;
		default:
		    xputchar('\\' | QUOTE);
		    break;
		}
	    }
#endif /* OREO */
#endif /* SVID > 0 */
	    xputchar(c | QUOTE);

	}
	if (*v)
	    xputchar(sep | QUOTE);
    }
#if SVID > 0
#ifndef OREO
done:
#endif /* OREO */
#endif /* SVID > 0 */
    if (sep && nonl == 0)
	xputchar('\n');
    else
	flush();
    if (setintr)
#ifdef BSDSIGS
	(void) sigblock(sigmask(SIGINT));
#else /* !BSDSIGS */
	(void) sighold(SIGINT);
#endif /* BSDSIGS */
    if (gargv)
	blkfree(gargv), gargv = 0;
}

/* from "Karl Berry." <karl%mote.umb.edu@relay.cs.net> -- for NeXT things
   (and anything else with a modern compiler) */

/*ARGSUSED*/
void
dosetenv(v, c)
    register Char **v;
    struct command *c;
{
    Char   *vp, *lp;

    v++;
    if ((vp = *v++) == 0) {
	register Char **ep;

	if (setintr)
#ifdef BSDSIGS
	    (void) sigsetmask(sigblock((sigmask_t) 0) & ~sigmask(SIGINT));
#else /* !BSDSIGS */
	    (void) sigrelse (SIGINT);
#endif /* BSDSIGS */
	for (ep = STR_environ; *ep; ep++)
	    xprintf("%s\n", short2str(*ep));
	return;
    }
    if ((lp = *v++) == 0)
	lp = STRNULL;
    Setenv(vp, lp = globone(lp, G_APPEND));
    if (eq(vp, STRPATH)) {
	importpath(lp);
	dohash(NULL, NULL);
    }
#ifdef apollo
    else if (eq(vp, STRSYSTYPE))
	dohash(NULL, NULL);
#endif /* apollo */
    else if (eq(vp, STRLANG) || eq(vp, STRLC_CTYPE)) {
#ifdef NLS
	int     k;

	(void) setlocale(LC_ALL, "");
	for (k = 0200; k <= 0377 && !Isprint(k); k++);
	AsciiOnly = k > 0377;
#else /* !NLS */
	AsciiOnly = 0;
#endif /* NLS */
	NLSMapsAreInited = 0;
	ed_Init();
	if (MapsAreInited && !NLSMapsAreInited)
	    (void) ed_InitNLSMaps();
    }
    else if (eq(vp, STRNOREBIND)) {
	NoNLSRebind = 1;
    }
#ifdef SIG_WINDOW
    else if ((eq(lp, STRNULL) &&
	      (eq(vp, STRLINES) || eq(vp, STRCOLUMNS))) ||
	     eq(vp, STRTERMCAP)) {
	check_window_size(1);
    }
#endif /* SIG_WINDOW */
    xfree((ptr_t) lp);
}

/*ARGSUSED*/
void
dounsetenv(v, c)
    register Char **v;
    struct command *c;
{
    Char  **ep, *p, *n;
    int     i, maxi;
    static Char *name = NULL;

    if (name)
	xfree((ptr_t) name);
    /*
     * Find the longest environment variable
     */
    for (maxi = 0, ep = STR_environ; *ep; ep++) {
	for (i = 0, p = *ep; *p && *p != '='; p++, i++);
	if (i > maxi)
	    maxi = i;
    }

    name = (Char *) xmalloc((size_t) (maxi + 1) * sizeof(Char));

    while (++v && *v) 
	for (maxi = 1; maxi;)
	    for (maxi = 0, ep = STR_environ; *ep; ep++) {
		for (n = name, p = *ep; *p && *p != '='; *n++ = *p++);
		*n = '\0';
		if (!Gmatch(name, *v))
		    continue;
		maxi = 1;
		if (eq(name, STRNOREBIND))
		    NoNLSRebind = 0;
#ifdef apollo
		else if (eq(name, STRSYSTYPE))
		    dohash(NULL, NULL);
#endif /* apollo */
		else if (eq(name, STRLANG) || eq(name, STRLC_CTYPE)) {
#ifdef NLS
		    int     k;

		    (void) setlocale(LC_ALL, "");
		    for (k = 0200; k <= 0377 && !Isprint(k); k++);
		    AsciiOnly = k > 0377;
#else /* !NLS */
		    AsciiOnly = getenv("LANG") == NULL &&
			getenv("LC_CTYPE") == NULL;
#endif /* NLS */
		    NLSMapsAreInited = 0;
		    ed_Init();
		    if (MapsAreInited && !NLSMapsAreInited)
			(void) ed_InitNLSMaps();

		}
		/*
		 * Delete name, and start again cause the environment changes
		 */
		Unsetenv(name);
		break;
	    }
    xfree((ptr_t) name); name = NULL;
}

void
Setenv(name, val)
    Char   *name, *val;
{
#ifdef SETENV_IN_LIB
/*
 * XXX: This does not work right, since tcsh cannot track changes to
 * the environment this way. (the builtin setenv without arguments does
 * not print the right stuff neither does unsetenv). This was for Mach,
 * it is not needed anymore.
 */
#undef setenv
    char    nameBuf[BUFSIZE];
    char   *cname = short2str(name);

    if (cname == NULL)
	return;
    (void) strcpy(nameBuf, cname);
    setenv(nameBuf, short2str(val), 1);
#else /* !SETENV_IN_LIB */
    register Char **ep = STR_environ;
    register Char *cp, *dp;
    Char   *blk[2];
    Char  **oep = ep;


    for (; *ep; ep++) {
	for (cp = name, dp = *ep; *cp && *cp == *dp; cp++, dp++)
	    continue;
	if (*cp != 0 || *dp != '=')
	    continue;
	cp = Strspl(STRequal, val);
	xfree((ptr_t) * ep);
	*ep = strip(Strspl(name, cp));
	xfree((ptr_t) cp);
	blkfree((Char **) environ);
	environ = short2blk(STR_environ);
	return;
    }
    cp = Strspl(name, STRequal);
    blk[0] = strip(Strspl(cp, val));
    xfree((ptr_t) cp);
    blk[1] = 0;
    STR_environ = blkspl(STR_environ, blk);
    blkfree((Char **) environ);
    environ = short2blk(STR_environ);
    xfree((ptr_t) oep);
#endif /* SETENV_IN_LIB */
}

void
Unsetenv(name)
    Char   *name;
{
    register Char **ep = STR_environ;
    register Char *cp, *dp;
    Char  **oep = ep;

    for (; *ep; ep++) {
	for (cp = name, dp = *ep; *cp && *cp == *dp; cp++, dp++)
	    continue;
	if (*cp != 0 || *dp != '=')
	    continue;
	cp = *ep;
	*ep = 0;
	STR_environ = blkspl(STR_environ, ep + 1);
	environ = short2blk(STR_environ);
	*ep = cp;
	xfree((ptr_t) cp);
	xfree((ptr_t) oep);
	return;
    }
}

/*ARGSUSED*/
void
doumask(v, c)
    register Char **v;
    struct command *c;
{
    register Char *cp = v[1];
    register int i;

    if (cp == 0) {
	i = umask(0);
	(void) umask(i);
	xprintf("%o\n", i);
	return;
    }
    i = 0;
    while (Isdigit(*cp) && *cp != '8' && *cp != '9')
	i = i * 8 + *cp++ - '0';
    if (*cp || i < 0 || i > 0777)
	stderror(ERR_NAME | ERR_MASK);
    (void) umask(i);
}

#ifndef HAVENOLIMIT
# ifndef BSDTIMES
   typedef long RLIM_TYPE;
#  ifndef RLIM_INFINITY
    extern RLIM_TYPE ulimit();
#   define RLIM_INFINITY 0x003fffff
#   define RLIMIT_FSIZE 1
#  endif /* RLIM_INFINITY */
#  ifdef aiws
#   define toset(a) (((a) == 3) ? 1004 : (a) + 1)
#   define RLIMIT_DATA	3
#   define RLIMIT_STACK 1005
#  else /* aiws */
#   define toset(a) ((a) + 1)
#  endif /* aiws */
# else /* BSDTIMES */
   typedef int RLIM_TYPE;
# endif /* BSDTIMES */


static struct limits {
    int     limconst;
    char   *limname;
    int     limdiv;
    char   *limscale;
}       limits[] = {

# ifdef RLIMIT_CPU
    RLIMIT_CPU, 	"cputime",	1,	"seconds",
# endif /* RLIMIT_CPU */

# ifdef RLIMIT_FSIZE
#  ifndef aiws
    RLIMIT_FSIZE, 	"filesize",	1024,	"kbytes",
#  else
    RLIMIT_FSIZE, 	"filesize",	512,	"blocks",
#  endif /* aiws */
# endif /* RLIMIT_FSIZE */

# ifdef RLIMIT_DATA
    RLIMIT_DATA, 	"datasize",	1024,	"kbytes",
# endif /* RLIMIT_DATA */

# ifdef RLIMIT_STACK
#  ifndef aiws
    RLIMIT_STACK, 	"stacksize",	1024,	"kbytes",
#  else
    RLIMIT_STACK, 	"stacksize",	1024 * 1024,	"kbytes",
#  endif /* aiws */
# endif /* RLIMIT_STACK */

# ifdef RLIMIT_CORE
    RLIMIT_CORE, 	"coredumpsize",	1024,	"kbytes",
# endif /* RLIMIT_CORE */

# ifdef RLIMIT_RSS
    RLIMIT_RSS, 	"memoryuse",	1024,	"kbytes",
# endif /* RLIMIT_RSS */

# ifdef RLIMIT_NOFILE
    RLIMIT_NOFILE, 	"descriptors", 1,	"",
# endif /* RLIMIT_NOFILE */

# ifdef RLIMIT_CONCUR
    RLIMIT_CONCUR, 	"concurrency", 1,	"thread(s)",
# endif /* RLIMIT_CONCUR */

# ifdef RLIMIT_MEMLOCK
    RLIMIT_MEMLOCK,	"memorylocked",	1024,	"kbytes",
# endif /* RLIMIT_MEMLOCK */

# ifdef RLIMIT_NPROC
    RLIMIT_NPROC,	"maxproc",	1,	"",
# endif /* RLIMIT_NPROC */

# ifdef RLIMIT_OFILE
    RLIMIT_OFILE,	"openfiles",	1,	"",
# endif /* RLIMIT_OFILE */

    -1, 		NULL, 		0, 	NULL
};

static struct limits *findlim();
static RLIM_TYPE getval();
static void limtail();
static void plim();
static int setlim();

#if defined(convex) || defined(__convex__)
static  RLIM_TYPE
restrict_limit(value)
    double  value;
{
    /*
     * is f too large to cope with? return the maximum or minimum int
     */
    if (value > (double) INT_MAX)
	return (INT_MAX);
    else if (value < (double) INT_MIN)
	return (INT_MIN);
    else
	return ((int) value);
}
#endif /* convex */


static struct limits *
findlim(cp)
    Char   *cp;
{
    register struct limits *lp, *res;

    res = (struct limits *) NULL;
    for (lp = limits; lp->limconst >= 0; lp++)
	if (prefix(cp, str2short(lp->limname))) {
	    if (res)
		stderror(ERR_NAME | ERR_AMBIG);
	    res = lp;
	}
    if (res)
	return (res);
    stderror(ERR_NAME | ERR_LIMIT);
    /* NOTREACHED */
    return (0);
}

/*ARGSUSED*/
void
dolimit(v, c)
    register Char **v;
    struct command *c;
{
    register struct limits *lp;
    register RLIM_TYPE limit;
    char    hard = 0;

    v++;
    if (*v && eq(*v, STRmh)) {
	hard = 1;
	v++;
    }
    if (*v == 0) {
	for (lp = limits; lp->limconst >= 0; lp++)
	    plim(lp, hard);
	return;
    }
    lp = findlim(v[0]);
    if (v[1] == 0) {
	plim(lp, hard);
	return;
    }
    limit = getval(lp, v + 1);
    if (setlim(lp, hard, limit) < 0)
	stderror(ERR_SILENT);
}

static  RLIM_TYPE
getval(lp, v)
    register struct limits *lp;
    Char  **v;
{
# if defined(convex) || defined(__convex__)
    RLIM_TYPE restrict_limit();
# endif /* convex */

    register float f;
    double  atof();
    static int lmin = 0x80000000, lmax = 0x7fffffff;
    Char   *cp = *v++;

    f = atof(short2str(cp));

# if defined(convex) || defined(__convex__)
    /*
     * is f too large to cope with. limit f to minint, maxint  - X-6768 by
     * strike
     */
    if ((f < (double) INT_MIN) || (f > (double) INT_MAX)) {
	stderror(ERR_NAME | ERR_TOOLARGE);
    }
# endif /* convex */

    while (Isdigit(*cp) || *cp == '.' || *cp == 'e' || *cp == 'E')
	cp++;
    if (*cp == 0) {
	if (*v == 0)
# if defined(convex) || defined(__convex__)
	    return ((RLIM_TYPE) restrict_limit((f + 0.5) * lp->limdiv));
# else /* convex */
	    return ((RLIM_TYPE) ((f + 0.5) * lp->limdiv));
# endif /* convex */
	cp = *v;
    }
    switch (*cp) {
# ifdef RLIMIT_CPU
    case ':':
	if (lp->limconst != RLIMIT_CPU)
	    goto badscal;
#  if defined(convex) || defined(__convex__)
	return ((RLIM_TYPE)
		restrict_limit((f * 60.0 + atof(short2str(cp + 1)))));
#  else /* convex */
	return ((RLIM_TYPE) (f * 60.0 + atof(short2str(cp + 1))));
#  endif /* convex */
    case 'h':
	if (lp->limconst != RLIMIT_CPU)
	    goto badscal;
	limtail(cp, "hours");
	f *= 3600.0;
	break;
    case 'm':
	if (lp->limconst == RLIMIT_CPU) {
	    limtail(cp, "minutes");
	    f *= 60.0;
	    break;
	}
	*cp = 'm';
	limtail(cp, "megabytes");
	f *= 1024.0 * 1024.0;
	break;
    case 's':
	if (lp->limconst != RLIMIT_CPU)
	    goto badscal;
	limtail(cp, "seconds");
	break;
# endif /* RLIMIT_CPU */
    case 'M':
# ifdef RLIMIT_CPU
	if (lp->limconst == RLIMIT_CPU)
	    goto badscal;
# endif /* RLIMIT_CPU */
	*cp = 'm';
	limtail(cp, "megabytes");
	f *= 1024.0 * 1024.0;
	break;
    case 'k':
# ifdef RLIMIT_CPU
	if (lp->limconst == RLIMIT_CPU)
	    goto badscal;
# endif /* RLIMIT_CPU */
	limtail(cp, "kbytes");
	f *= 1024.0;
	break;
    case 'b':
# ifdef RLIMIT_CPU
	if (lp->limconst == RLIMIT_CPU)
	    goto badscal;
# endif /* RLIMIT_CPU */
	limtail(cp, "blocks");
	f *= 512.0;
	break;
    case 'u':
	limtail(cp, "unlimited");
	return (RLIM_INFINITY);
    default:
# ifdef RLIMIT_CPU
badscal:
# endif /* RLIMIT_CPU */
	stderror(ERR_NAME | ERR_SCALEF);
    }
# if defined(convex) || defined(__convex__)
    return ((RLIM_TYPE) restrict_limit((f + 0.5)));
# else
    f += 0.5;
    if (f > (float) lmax)
	return lmax;
    else if (f < (float) lmin)
	return lmin;
    else
	return ((RLIM_TYPE) f);
# endif /* convex */
}

static void
limtail(cp, str)
    Char   *cp;
    char   *str;
{
    while (*cp && *cp == *str)
	cp++, str++;
    if (*cp)
	stderror(ERR_BADSCALE, str);
}


/*ARGSUSED*/
static void
plim(lp, hard)
    register struct limits *lp;
    Char    hard;
{
# ifdef BSDTIMES
    struct rlimit rlim;
# endif /* BSDTIMES */
    RLIM_TYPE limit;

    xprintf("%s \t", lp->limname);

# ifndef BSDTIMES
    limit = ulimit(lp->limconst, 0);
#  ifdef aiws
    if (lp->limconst == RLIMIT_DATA)
	limit -= 0x20000000;
#  endif /* aiws */
# else /* BSDTIMES */
    (void) getrlimit(lp->limconst, &rlim);
    limit = hard ? rlim.rlim_max : rlim.rlim_cur;
# endif /* BSDTIMES */

    if (limit == RLIM_INFINITY)
	xprintf("unlimited");
# ifdef RLIMIT_CPU
    else if (lp->limconst == RLIMIT_CPU)
	psecs((long) limit);
# endif /* RLIMIT_CPU */
    else
# ifndef BSDTIMES
    if (lp->limconst == RLIMIT_FSIZE)
	/*
	 * Christos: filesize comes in 512 blocks. we divide by 2 to get 1024
	 * blocks. Note we cannot pre-multiply cause we might overflow (A/UX)
	 */
	xprintf("%ld %s", (long) (limit / (lp->limdiv == 1024 ? 2 : 1)), 
	        lp->limscale);
    else
# endif /* BSDTIMES */
	xprintf("%ld %s", (long) (limit / lp->limdiv), lp->limscale);
    xprintf("\n");
}

/*ARGSUSED*/
void
dounlimit(v, c)
    register Char **v;
    struct command *c;
{
    register struct limits *lp;
    int     lerr = 0;
    Char    hard = 0;

    v++;
    if (*v && eq(*v, STRmh)) {
	hard = 1;
	v++;
    }
    if (*v == 0) {
	for (lp = limits; lp->limconst >= 0; lp++)
	    if (setlim(lp, hard, (RLIM_TYPE) RLIM_INFINITY) < 0)
		lerr++;
	if (lerr)
	    stderror(ERR_SILENT);
	return;
    }
    while (*v) {
	lp = findlim(*v++);
	if (setlim(lp, hard, (RLIM_TYPE) RLIM_INFINITY) < 0)
	    stderror(ERR_SILENT);
    }
}

static int
setlim(lp, hard, limit)
    register struct limits *lp;
    Char    hard;
    RLIM_TYPE limit;
{
# ifdef BSDTIMES
    struct rlimit rlim;

    (void) getrlimit(lp->limconst, &rlim);

    if (hard)
	rlim.rlim_max = limit;
    else if (limit == RLIM_INFINITY && geteuid() != 0)
	rlim.rlim_cur = rlim.rlim_max;
    else
	rlim.rlim_cur = limit;

    if (setrlimit(lp->limconst, &rlim) < 0) {
# else /* BSDTIMES */
    if (limit != RLIM_INFINITY && lp->limconst == RLIMIT_FSIZE)
	limit /= 512;
# ifdef aiws
    if (lp->limconst == RLIMIT_DATA)
	limit += 0x20000000;
# endif /* aiws */
    if (ulimit(toset(lp->limconst), limit) < 0) {
# endif /* BSDTIMES */
	xprintf("%s: %s: Can't %s%s limit\n", bname, lp->limname,
		limit == RLIM_INFINITY ? "remove" : "set",
		hard ? " hard" : "");
	return (-1);
    }
    return (0);
}

#endif /* !HAVENOLIMIT */

/*ARGSUSED*/
void
dosuspend(v, c)
    Char **v;
    struct command *c;
{
    int     ctpgrp;

    sigret_t(*old) ();

    if (loginsh)
	stderror(ERR_SUSPLOG);
    untty();

#ifdef BSDJOBS
    old = signal(SIGTSTP, SIG_DFL);
    (void) kill(0, SIGTSTP);
    /* the shell stops here */
    (void) signal(SIGTSTP, old);
#else /* !BSDJOBS */
    stderror(ERR_JOBCONTROL);
#endif /* BSDJOBS */

#ifdef BSDJOBS
    if (tpgrp != -1) {
retry:
	ctpgrp = tcgetpgrp(FSHTTY);
	if (ctpgrp != opgrp) {
	    old = signal(SIGTTIN, SIG_DFL);
	    (void) kill(0, SIGTTIN);
	    (void) signal(SIGTTIN, old);
	    goto retry;
	}
	(void) setpgid(0, shpgrp);
	(void) tcsetpgrp(FSHTTY, shpgrp);
    }
#endif /* BSDJOBS */
    (void) setdisc(FSHTTY);
}

/* This is the dreaded EVAL built-in.
 *   If you don't fiddle with file descriptors, and reset didfds,
 *   this command will either ignore redirection inside or outside
 *   its aguments, e.g. eval "date >x"  vs.  eval "date" >x
 *   The stuff here seems to work, but I did it by trial and error rather
 *   than really knowing what was going on.  If tpgrp is zero, we are
 *   probably a background eval, e.g. "eval date &", and we want to
 *   make sure that any processes we start stay in our pgrp.
 *   This is also the case for "time eval date" -- stay in same pgrp.
 *   Otherwise, under stty tostop, processes will stop in the wrong
 *   pgrp, with no way for the shell to get them going again.  -IAN!
 */

static Char **gv = NULL;

/*ARGSUSED*/
void
doeval(v, c)
    Char  **v;
    struct command *c;
{
    Char  **oevalvec;
    Char   *oevalp;
    int     odidfds;
#ifndef FIOCLEX
    int     odidcch;
#endif /* FIOCLEX */
    jmp_buf osetexit;
    int     my_reenter;
    Char  **savegv;
    int     saveIN, saveOUT, saveDIAG;
    int     oSHIN, oSHOUT, oSHDIAG;

    oevalvec = evalvec;
    oevalp = evalp;
    odidfds = didfds;
#ifndef FIOCLEX
    odidcch = didcch;
#endif /* FIOCLEX */
    oSHIN = SHIN;
    oSHOUT = SHOUT;
    oSHDIAG = SHDIAG;

    savegv = gv;

    v++;
    if (*v == 0)
	return;
    gflag = 0, tglob(v);
    if (gflag) {
	gv = v = globall(v);
	gargv = 0;
	if (v == 0)
	    stderror(ERR_NOMATCH);
	v = copyblk(v);
    }
    else {
	gv = NULL;
	v = copyblk(v);
	trim(v);
    }

    saveIN = dcopy(SHIN, -1);
    saveOUT = dcopy(SHOUT, -1);
    saveDIAG = dcopy(SHDIAG, -1);

    getexit(osetexit);

    /* PWP: setjmp/longjmp bugfix for optimizing compilers */
#ifdef cray
    my_reenter = 1;             /* assume non-zero return val */
    if (setexit() == 0) {
	my_reenter = 0;         /* Oh well, we were wrong */
#else /* !cray */
    if ((my_reenter = setexit()) == 0) {
#endif /* cray */
	evalvec = v;
	evalp = 0;
	SHIN = dcopy(0, -1);
	SHOUT = dcopy(1, -1);
	SHDIAG = dcopy(2, -1);
#ifndef FIOCLEX
	didcch = 0;
#endif /* FIOCLEX */
	didfds = 0;
	process(0);
    }

    evalvec = oevalvec;
    evalp = oevalp;
    doneinp = 0;
#ifndef FIOCLEX
    didcch = odidcch;
#endif /* FIOCLEX */
    didfds = odidfds;
    (void) close(SHIN);
    (void) close(SHOUT);
    (void) close(SHDIAG);
    SHIN = dmove(saveIN, oSHIN);
    SHOUT = dmove(saveOUT, oSHOUT);
    SHDIAG = dmove(saveDIAG, oSHDIAG);

    if (gv)
	blkfree(gv);

    gv = savegv;
    resexit(osetexit);
    if (my_reenter)
	stderror(ERR_SILENT);
}
