/* $Header: ngstuff.c,v 4.3.1.2 85/05/10 14:31:52 lwall Exp $
 *
 * $Log:	ngstuff.c,v $
 * Revision 4.3.1.2  85/05/10  14:31:52  lwall
 * Prevented "Junked" or "Marked unread" when no state change.
 * 
 * Revision 4.3.1.1  85/05/10  11:36:45  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:45:03  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "term.h"
#include "util.h"
#include "ng.h"
#include "bits.h"
#include "intrp.h"
#include "cheat.h"
#include "head.h"
#include "final.h"
#include "sw.h"
#include "INTERN.h"
#include "ngstuff.h"

void
ngstuff_init()
{
    ;
}

/* do a shell escape */

int
escapade()
{
    register char *s;
    bool interactive = (buf[1] == FINISHCMD);
    bool docd;
    char whereiam[256];

    if (!finish_command(interactive))	/* get remainder of command */
	return -1;
    s = buf+1;
    docd = *s != '!';
    if (!docd) {
	s++;
    }
    else {
	getwd(whereiam);
	if (chdir(cwd)) {
	    printf(nocd,cwd) FLUSH;
	    sig_catcher(0);
	}
    }
    while (*s == ' ') s++;
					/* skip leading spaces */
    interp(cmd_buf, (sizeof cmd_buf), s);/* interpret any % escapes */
    resetty();				/* make sure tty is friendly */
    doshell(Nullch,cmd_buf);	/* invoke the shell */
    noecho();				/* and make terminal */
    crmode();				/*   unfriendly again */
    if (docd) {
	if (chdir(whereiam)) {
	    printf(nocd,whereiam) FLUSH;
	    sig_catcher(0);
	}
    }
#ifdef MAILCALL;
    mailcount = 0;			/* force recheck */
#endif
    return 0;
}

/* process & command */

int
switcheroo()
{
    if (!finish_command(TRUE)) /* get rest of command */
	return -1;	/* if rubbed out, try something else */
    if (!buf[1])
	pr_switches();
#ifdef PUSHBACK
    else if (buf[1] == '&') {
	if (!buf[2]) {
	    page_init();
	    show_macros();
	}
	else {
	    char tmpbuf[LBUFLEN];
	    register char *s;

	    for (s=buf+2; isspace(*s); s++);
	    mac_line(s,tmpbuf,(sizeof tmpbuf));
	}
    }
#endif
    else {
	bool docd = (instr(buf,"-d") != Nullch);
 	char whereami[256];
 
	if (docd)
	    getwd(whereami);
	sw_list(buf+1);
	if (docd) {
	    cwd_check();
	    if (chdir(whereami)) {		/* -d does chdirs */
		printf(nocd,whereami) FLUSH;
		sig_catcher(0);
	    }
	}
    }
    return 0;
}

/* process range commands */

int
numnum()
{
    ART_NUM min, max;
    char *cmdlst = Nullch;
    register char *s, *c;
    ART_NUM oldart = art;
    char tmpbuf[LBUFLEN];
    bool justone = TRUE;		/* assume only one article */

    if (!finish_command(TRUE))	/* get rest of command */
	return NN_INP;
	if (lastart < 1) {
	    fputs("\nNo articles\n",stdout) FLUSH;
	    return NN_ASK;
	}
#ifdef ARTSRCH
    if (srchahead)
	srchahead = -1;
#endif
    for (s=buf; *s && (isdigit(*s) || index(" ,-.$",*s)); s++)
	if (!isdigit(*s))
	    justone = FALSE;
    if (*s) {
	cmdlst = savestr(s);
	justone = FALSE;
    }
    else if (!justone)
	cmdlst = savestr("m");
    *s++ = ',';
    *s = '\0';
    safecpy(tmpbuf,buf,LBUFLEN);
    for (s = tmpbuf; c = index(s,','); s = ++c) {
	*c = '\0';
	if (*s == '.')
	    min = oldart;
	else
	    min = atol(s);
	if (min<absfirst) {		/* make sure it is reasonable */
	    min = absfirst;
	    printf("(First article is %ld)\n",(long)absfirst) FLUSH;
	    pad(just_a_sec/3);
	}
	if ((s=index(s,'-')) != Nullch) {
	    s++;
	    if (*s == '$')
		max = lastart;
	    else if (*s == '.')
		max = oldart;
	    else
		max = atol(s);
	}
	else
	    max = min;
	if (max>lastart) {
	    max = lastart;
	    if (min > max)
		min = max;
	    printf("(Last article is %ld)\n",(long)lastart) FLUSH;
	    pad(just_a_sec/3);
	}
	if (max < min) {
	    fputs("\nBad range\n",stdout) FLUSH;
	    if (cmdlst)
		free(cmdlst);
	    return NN_ASK;
	}
	if (justone) {
	    art = min;
	    return NN_REREAD;
	}
	check_first(min);
	for (art=min; art<=max; art++) {
	    if (perform(cmdlst,TRUE)) {
#ifdef VERBOSE
		IF(verbose)
		    printf("\n(Interrupted at article %ld)\n",(long)art)
		      FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\n(Intr at %ld)\n",(long)art) FLUSH;
#endif
		if (cmdlst)
		    free(cmdlst);
		return NN_ASK;
	    }
	}
    }
    art = oldart;
    if (cmdlst)
	free(cmdlst);
    return NN_NORM;
}

int
perform(cmdlst,toplevel)
register char *cmdlst;
int toplevel;
{
    register int ch;
    
    if (toplevel) {
	printf("%-6ld",art);
	fflush(stdout);
    }
    for (; ch = *cmdlst; cmdlst++) {
	if (isspace(ch) || ch == ':')
	    continue;
	if (ch == 'j') {
	    if (!was_read(art)) {
		mark_as_read(art);
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tJunked",stdout);
#endif
	    }
	}
	else if (ch == 'm') {
	    if (was_read(art)) {
		unmark_as_read(art);
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tMarked unread",stdout);
#endif
	    }
	}
	else if (ch == 'M') {
#ifdef DELAYMARK
	    delay_unmark(art);
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tWill return",stdout);
#endif
#else
	    notincl("M");
	    return -1;
#endif
	}
	else if (ch == '=') {
	    printf("\t%s",fetchsubj(art,FALSE,FALSE));
#ifdef VERBOSE
	    IF(verbose)
		;
	    ELSE
#endif
		putchar('\n') FLUSH;		/* ghad! */
	}
	else if (ch == 'C') {
#ifdef ASYNC_PARSE
	    printf("\t%sancelled",(cancel_article() ? "Not c" : "C"));
#else
	    notincl("C");
	    return -1;
#endif
	}
	else if (ch == '%') {
#ifdef ASYNC_PARSE
	    char tmpbuf[512];

	    cmdlst = dointerp(tmpbuf, (sizeof tmpbuf), cmdlst,":");
	    if (*cmdlst != ':')
		--cmdlst;
	    if (perform(tmpbuf,FALSE))
		return -1;
#else
	    notincl("%");
	    return -1;
#endif
	}
	else if (index("!&sSwW|",ch)) {
	    cmdlst = cpytill(buf,cmdlst,':') - 1;
	    /* we now have the command in buf */
	    if (ch == '!') {
		escapade();
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tShell escaped",stdout);
#endif
	    }
	    else if (ch == '&') {
		switcheroo();
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tSwitched",stdout);
#endif
	    }
	    else {
		putchar('\t');
		save_article();
	    }
	}
	else {
	    printf("\t???%s\n",cmdlst);
	    return -1;
	}
#ifdef VERBOSE
	fflush(stdout);
#endif
    }
    if (toplevel) {
#ifdef VERBOSE
	IF(verbose)
	    putchar('\n') FLUSH;
	ELSE
#endif
#ifdef TERSE
	    putchar(' ');
#endif
    }
    return 0;
}

