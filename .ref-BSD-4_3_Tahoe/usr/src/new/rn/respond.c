/* $Header: respond.c,v 4.3.1.5 85/09/10 11:05:00 lwall Exp $
 *
 * $Log:	respond.c,v $
 * Revision 4.3.1.5  85/09/10  11:05:00  lwall
 * Improved %m in in_char().
 * 
 * Revision 4.3.1.4  85/05/23  17:24:49  lwall
 * Now allows 'r' and 'f' on null articles.
 * 
 * Revision 4.3.1.3  85/05/15  14:42:32  lwall
 * Removed duplicate include of intrp.h.
 * 
 * Revision 4.3.1.2  85/05/14  08:55:15  lwall
 * Default for normal/mailbox question was applied to wrong buffer.
 * 
 * Revision 4.3.1.1  85/05/10  11:37:33  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:47:04  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "head.h"
#include "term.h"
#include "ng.h"
#include "util.h"
#include "rn.h"
#include "artio.h"
#include "final.h"
#include "INTERN.h"
#include "respond.h"

static char nullart[] = "\nNull article\n";

void
respond_init()
{
    ;
}

int
save_article()
{
    bool use_pref;
    register char *s, *c;
    char altbuf[CBUFLEN];
    int iter;
    bool interactive = (buf[1] == FINISHCMD);
    
    if (!finish_command(interactive))	/* get rest of command */
	return SAVE_ABORT;
    use_pref = isupper(*buf);
#ifdef ASYNC_PARSE
    parse_maybe(art);
#endif
    savefrom = (*buf=='w' || *buf=='W' ? htype[PAST_HEADER].ht_minpos : 0);
    if (artopen(art) == Nullfp) {
#ifdef VERBOSE
	IF(verbose)
	    fputs("\n\
Saving null articles is not very productive!  :-)\n\
",stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs(nullart,stdout) FLUSH;
#endif
	return SAVE_DONE;
    }
    if (chdir(cwd)) {
	printf(nocd,cwd) FLUSH;
	sig_catcher(0);
    }
    if (savedest)
	free(savedest);
    if ((s = index(buf,'|')) != Nullch) {
				/* is it a pipe command? */
	s++;			/* skip the | */
	while (*s == ' ') s++;
	safecpy(altbuf,filexp(s),sizeof altbuf);
	savedest = altbuf;
	interp(cmd_buf, (sizeof cmd_buf), getval("PIPESAVER",PIPESAVER));
				/* then set up for command */
	resetty();		/* restore tty state */
	if (use_pref)		/* use preferred shell? */
	    doshell(Nullch,cmd_buf);
				/* do command with it */
	else
	    doshell(sh,cmd_buf);	/* do command with sh */
	noecho();		/* and stop echoing */
	crmode();		/* and start cbreaking */
	savedest = savestr(savedest);
    }
    else {			/* normal save */
	bool there, mailbox;
	char *savename = getval("SAVENAME",SAVENAME);

	s = buf+1;		/* skip s or S */
	if (*s == '-') {	/* if they are confused, skip - also */
#ifdef VERBOSE
	    IF(verbose)
		fputs("Warning: '-' ignored.  This isn't readnews.\n",stdout)
		  FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("'-' ignored.\n",stdout) FLUSH;
#endif
	    s++;
	}
	for (; *s == ' '; s++);	/* skip spaces */
	safecpy(altbuf,filexp(s),sizeof altbuf);
	s = altbuf;
	if (! index(s,'/')) {
	    interp(buf, (sizeof buf), getval("SAVEDIR",SAVEDIR));
	    if (makedir(buf,MD_DIR))	/* ensure directory exists */
		strcpy(buf,cwd);
	    if (*s) {
		for (c = buf; *c; c++) ;
		*c++ = '/';
		strcpy(c,s);		/* add filename */
	    }
	    s = buf;
	}
	for (iter = 0;
	    (there = stat(s,&filestat) >= 0) &&
	    (filestat.st_mode & S_IFDIR);
	    iter++) {			/* is it a directory? */

	    c = (s+strlen(s));
	    *c++ = '/';			/* put a slash before filename */
	    interp(c, s==buf?(sizeof buf):(sizeof altbuf),
		iter ? "News" : savename );
				/* generate a default name somehow or other */
	    if (index(c,'/')) {		/* yikes, a '/' in the filename */
		makedir(s,MD_FILE);
	    }
	}
	if (*s != '/') {		/* relative path? */
	    c = (s==buf ? altbuf : buf);
	    sprintf(c, "%s/%s", cwd, s);
	    s = c;			/* absolutize it */
	}
	s = savedest = savestr(s);	/* doesn't move any more */
					/* make it handy for %b */
	if (!there) {
	    if (mbox_always)
		mailbox = TRUE;
	    else if (norm_always)
		mailbox = FALSE;
	    else {
		char *dflt = (instr(savename,"%a") ? "nyq" : "ynq");
		
		sprintf(cmd_buf,
		"\nFile %s doesn't exist--\n	use mailbox format? [%s] ",
		  s,dflt);
	      reask_save:
		in_char(cmd_buf, 'M');
		putchar('\n') FLUSH;
		setdef(buf,dflt);
#ifdef VERIFY
		printcmd();
#endif
		if (*buf == 'h') {
#ifdef VERBOSE
		    IF(verbose)
			printf("\n\
Type y to create %s as a mailbox.\n\
Type n to create it as a normal file.\n\
Type q to abort the save.\n\
",s) FLUSH;
		    ELSE
#endif
#ifdef TERSE
			fputs("\n\
y to create mailbox.\n\
n to create normal file.\n\
q to abort.\n\
",stdout) FLUSH;
#endif
		    goto reask_save;
		}
		else if (*buf == 'n') {
		    mailbox = FALSE;
		}
		else if (*buf == 'y') {
		    mailbox = TRUE;
		}
		else if (*buf == 'q') {
		    goto s_bomb;
		}
		else {
		    fputs(hforhelp,stdout) FLUSH;
		    settle_down();
		    goto reask_save;
		}
	    }
	}
	else if (filestat.st_mode & S_IFCHR)
	    mailbox = FALSE;
	else {
	    int tmpfd;
	    
	    tmpfd = open(s,0);
	    if (tmpfd == -1)
		mailbox = FALSE;
	    else {
		read(tmpfd,buf,LBUFLEN);
		c = buf;
		if (!isspace(MBOXCHAR))
		    while (isspace(*c))
			c++;
		mailbox = (*c == MBOXCHAR);
		close(tmpfd);
	    }
	}

	safecpy(cmd_buf, filexp(mailbox ?
	    getval("MBOXSAVER",MBOXSAVER) :
	    getval("NORMSAVER",NORMSAVER) ), sizeof cmd_buf);
				/* format the command */
	resetty();		/* make terminal behave */
	if (doshell(use_pref?Nullch:SH,cmd_buf))
	    fputs("Not saved",stdout);
	else
	    printf("%s to %s %s",
	      there?"Appended":"Saved",
	      mailbox?"mailbox":"file",
	      s);
	if (interactive)
	    putchar('\n') FLUSH;
	noecho();		/* make terminal do what we want */
	crmode();
    }
s_bomb:
    if (chdir(spool) || chdir(ngdir)) {
	printf(nocd,ngdir) FLUSH;
	sig_catcher(0);
    }
    return SAVE_DONE;
}

int
cancel_article()
{
    char *artid_buf;
    char *ngs_buf;
    char *from_buf;
    char *reply_buf;
    int myuid = getuid();
    int r = -1;

    if (artopen(art) == Nullfp) {
#ifdef VERBOSE
	IF(verbose)
	    fputs("\n\
Cancelling null articles is your idea of fun?  :-)\n\
",stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs(nullart,stdout) FLUSH;
#endif
	return r;
    }
    reply_buf = fetchlines(art,REPLY_LINE);
    from_buf = fetchlines(art,FROM_LINE);
    artid_buf = fetchlines(art,ARTID_LINE);
    ngs_buf = fetchlines(art,NGS_LINE);
    if (!instr(from_buf,sitename) ||
	(!instr(from_buf,logname) &&
	 !instr(reply_buf,logname) &&
#ifdef NEWSADMIN
	 myuid != newsuid &&
#endif
	 myuid != ROOTID ) )
#ifdef VERBOSE
	    IF(verbose)
		fputs("You can't cancel someone else's article\n",stdout)
		  FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("Not your article\n",stdout) FLUSH;
#endif
    else {
	tmpfp = fopen(headname,"w");	/* open header file */
	if (tmpfp == Nullfp) {
	    printf(cantcreate,headname) FLUSH;
	    goto no_cancel;
	}
	interp(buf, (sizeof buf), getval("CANCELHEADER",CANCELHEADER));
	fputs(buf,tmpfp);
	fclose(tmpfp);
	r = doshell(sh,filexp(getval("CANCEL",CANCEL)));
    }
no_cancel:
    free(artid_buf);
    free(ngs_buf);
    free(from_buf);
    free(reply_buf);
    return r;
}

void
reply()
{
    bool incl_body = (*buf == 'R');
    char *maildoer = savestr(filexp(getval("MAILPOSTER",MAILPOSTER)));

    artopen(art);
    tmpfp = fopen(headname,"w");	/* open header file */
    if (tmpfp == Nullfp) {
	printf(cantcreate,headname) FLUSH;
	goto no_reply;
    }
    interp(buf, (sizeof buf), getval("MAILHEADER",MAILHEADER));
    fputs(buf,tmpfp);
    if (!instr(maildoer,"%h"))
#ifdef VERBOSE
	IF(verbose)
	    printf("\n%s\n(Above lines saved in file %s)\n",buf,headname)
	      FLUSH;
	ELSE
#endif
#ifdef TERSE
	    printf("\n%s\n(Header in %s)\n",buf,headname) FLUSH;
#endif
    if (incl_body && artfp != Nullfp) {
	interp(buf, (sizeof buf), getval("YOUSAID",YOUSAID));
	fprintf(tmpfp,"%s\n",buf);
#ifdef ASYNC_PARSE
	parse_maybe(art);
#endif
	fseek(artfp,(long)htype[PAST_HEADER].ht_minpos,0);
	while (fgets(buf,LBUFLEN,artfp) != Nullch) {
	    fprintf(tmpfp,"%s%s",indstr,buf);
	}
	fprintf(tmpfp,"\n");
    }
    fclose(tmpfp);
    interp(cmd_buf, (sizeof cmd_buf), maildoer);
    invoke(cmd_buf,origdir);
    UNLINK(headname);		/* kill the header file */
no_reply:
    free(maildoer);
}

void
followup()
{
    bool incl_body = (*buf == 'F');

    artopen(art);
    tmpfp = fopen(headname,"w");
    if (tmpfp == Nullfp) {
	printf(cantcreate,headname) FLUSH;
	return;
    }
    interp(buf, (sizeof buf), getval("NEWSHEADER",NEWSHEADER));
    fprintf(tmpfp,"%s",buf);
    if (incl_body && artfp != Nullfp) {
#ifdef VERBOSE
	if (verbose)
	    fputs("\n\
(Be sure to double-check the attribution against the signature, and\n\
trim the quoted article down as much as possible.)\n\
",stdout) FLUSH;
#endif
	interp(buf, (sizeof buf), getval("ATTRIBUTION",ATTRIBUTION));
	fprintf(tmpfp,"%s\n",buf);
#ifdef ASYNC_PARSE
	parse_maybe(art);
#endif
	fseek(artfp,(long)htype[PAST_HEADER].ht_minpos,0);
	while (fgets(buf,LBUFLEN,artfp) != Nullch) {
	    fprintf(tmpfp,"%s%s",indstr,buf);
	}
	fprintf(tmpfp,"\n");
    }
    fclose(tmpfp);
    safecpy(cmd_buf,filexp(getval("NEWSPOSTER",NEWSPOSTER)),sizeof cmd_buf);
    invoke(cmd_buf,origdir);
    UNLINK(headname);
}

void
invoke(cmd,dir)
char *cmd,*dir;
{
    if (chdir(dir)) {
	printf(nocd,dir) FLUSH;
	return;
    }
#ifdef VERBOSE
    IF(verbose)
	printf("\n(leaving cbreak mode; cwd=%s)\nInvoking command: %s\n\n",
	    dir,cmd) FLUSH;
    ELSE
#endif
#ifdef TERSE
	printf("\n(-cbreak; cwd=%s)\nInvoking: %s\n\n",dir,cmd) FLUSH;
#endif
    resetty();			/* make terminal well-behaved */
    doshell(sh,cmd);		/* do the command */
    noecho();			/* set no echo */
    crmode();			/* and cbreak mode */
#ifdef VERBOSE
    IF(verbose)
	fputs("\n(re-entering cbreak mode)\n",stdout) FLUSH;
    ELSE
#endif
#ifdef TERSE
	fputs("\n(+cbreak)\n",stdout) FLUSH;
#endif
    if (chdir(spool) || chdir(ngdir)) {
	printf(nocd,ngdir) FLUSH;
	sig_catcher(0);
    }
}

