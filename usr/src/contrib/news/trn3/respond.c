/* $Id: respond.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction or this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "cache.h"
#include "head.h"
#include "term.h"
#include "ngdata.h"
#include "ng.h"
#include "util.h"
#include "trn.h"
#include "artio.h"
#include "final.h"
#include "decode.h"
#include "INTERN.h"
#include "respond.h"

static char nullart[] = "\nNull article\n";

bool cut_line();

void
respond_init()
{
    ;
}

int
save_article()
{
    bool_int use_pref;
    register char *s, *c;
    char altbuf[CBUFLEN];
    int iter;
    bool interactive = (buf[1] == FINISHCMD);
    char cmd = *buf;
    
    if (!finish_command(interactive))	/* get rest of command */
	return SAVE_ABORT;
    if ((use_pref = isupper(cmd)) != 0)
	cmd = tolower(cmd);
    parseheader(art);
#ifdef METAMAIL
    savefrom = (!mime_article && (cmd == 'w' || cmd == 'e'))
#else
    savefrom = (cmd == 'w' || cmd == 'e')
#endif
		? htype[PAST_HEADER].ht_minpos : 0;
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
    if (cmd == 'e') {		/* is this an extract command? */
	static bool custom_extract = FALSE;
	int cnt = 0;
	bool found_cut = FALSE;
	char art_buf[LBUFLEN], *cmdstr;

	s = buf+1;		/* skip e */
	while (*s == ' ') s++;	/* skip leading spaces */
	safecpy(altbuf,filexp(s),sizeof altbuf);
	s = altbuf;
	if (*s) {
	    cmdstr = cpytill(buf,s,'|');	/* check for | */
	    s = buf + strlen(buf)-1;
	    while (*s == ' ') s--;		/* trim trailing spaces */
	    *++s = '\0';
	    if (*cmdstr) {
		s = cmdstr+1;			/* skip | */
		while (*s == ' ') s++;
		if (*s)	{			/* if new command, use it */
		    if (extractprog)
			free(extractprog);
		    extractprog = savestr(s);	/* put extracter in %e */
		}
		else
		    cmdstr = extractprog;
	    }
	    else
		cmdstr = Nullch;
	    s = buf;
	}
	else {
	    if (extractdest)
		strcpy(s, extractdest);
	    if (custom_extract)
		cmdstr = extractprog;
	    else
		cmdstr = Nullch;
	}
	if (cmdstr) {
	    if (strEQ(extractprog,"-"))
		cmdstr = Nullch;
	    else if (decode_fp != Nullfp)
		decode_end();
	}
	custom_extract = (cmdstr != 0);

	fseek(artfp,savefrom,0);
	if (*s != '/') {		/* relative path? */
	    c = (s==buf ? altbuf : buf);
	    interp(c, (sizeof buf), getval("SAVEDIR",SAVEDIR));
	    if (makedir(c,MD_DIR))	/* ensure directory exists */
		strcpy(c,cwd);
	    if (*s) {
		while (*c) c++;
		*c++ = '/';
		strcpy(c,s);		/* add filename */
	    }
	    s = (s==buf ? altbuf : buf);
	}
	if (*s != '/') {		/* path still relative? */
	    c = (s==buf ? altbuf : buf);
	    sprintf(c, "%s/%s", cwd, s);
	    s = c;			/* absolutize it */
	}
	if (decode_fp != Nullfp) {
	    printf("Continuing %s:%s\n", decode_fname,
		cmd != '\0' && strNE(extractdest,s) ?
		 " (Ignoring conflicting directory)" : nullstr ) FLUSH;
	    if (decode_type == UUDECODE)
		uudecode(artfp);
	    else
		unship(artfp);
	}
	else {
	    if (extractdest)
		free(extractdest);
	    s = extractdest = savestr(s); /* make it handy for %E */
	    if (makedir(s, MD_DIR)) {	/* ensure directory exists */
		int_count++;
		return SAVE_DONE;
	    }
	    if (chdir(s)) {
		printf(nocd,s) FLUSH;
		sig_catcher(0);
	    }
	    s = getwd(buf);		/* simplify path for output */
	    while(fgets(art_buf,LBUFLEN,artfp) != Nullch) {
		if (*art_buf <= ' ')
		    continue;	/* Ignore empty or initially-whitespace lines */
#ifdef METAMAIL
		if (mime_article) {
		    if (!custom_extract) {
			printf("Extracting MIME article into %s:\n", s) FLUSH;
			extractprog = savestr(filexp(getval("MIMESTORE",MIMESTORE)));
		    }
		    else
			printf("Extracting MIME article into %s using %s\n",
			       s, extractprog) FLUSH;
		    cnt = 0;
		    interp(cmd_buf, sizeof cmd_buf,
			   getval("EXMIMESAVER",EXMIMESAVER));
		    termlib_reset();
		    resetty();		/* restore tty state */
		    doshell(SH,cmd_buf);
		    noecho();		/* revert to cbreaking */
		    crmode();
		    termlib_init();
		    break;
		}
#endif
		if (found_cut && custom_extract) {
		    printf("Extracting data into %s using %s:\n",
			s, extractprog) FLUSH;
		    goto extract_it;
		}
		if (((*art_buf == '#' || *art_buf == ':')
		  && (strnEQ(art_buf+1, "! /bin/sh", 9)
		   || strnEQ(art_buf+1, "!/bin/sh", 8)
		   || strnEQ(art_buf+2, "This is ", 8)))
		 || strnEQ(art_buf, "sed ", 4)
		 || strnEQ(art_buf, "cat ", 4)
		 || strnEQ(art_buf, "echo ", 5)) {
		    fseek(artfp,-(long)strlen(art_buf)-NL_SIZE+1,1);
		    savefrom = ftell(artfp);
		    if (custom_extract) {
			printf("Extracting shar into %s using %s:\n",
				s, extractprog) FLUSH;
			goto extract_it;
		    }
		    /* Check for special-case of shar'ed-uuencoded file */
		    while(fgets(art_buf,LBUFLEN,artfp) != Nullch) {
			if (*art_buf == '#' || *art_buf == ':'
			 || strnEQ(art_buf, "echo ", 5)
			 || strnEQ(art_buf, "sed ", 4))
			    continue;
			if (strnEQ(art_buf, "Xbegin ", 7)) {
			    decode_type = UUDECODE;
			    goto decode_it;
			}
			break;
		    }
		    printf("Extracting shar into %s:\n", s) FLUSH;
		    if (extractprog)
			free(extractprog);
		    extractprog = savestr(filexp(getval("UNSHAR",UNSHAR)));
		  extract_it:
		    cnt = 0;
		    interp(cmd_buf,(sizeof cmd_buf),getval("EXSAVER",EXSAVER));
		    termlib_reset();
		    resetty();		/* restore tty state */
		    doshell(SH,cmd_buf);
		    noecho();		/* revert to cbreaking */
		    crmode();
		    termlib_init();
		    break;
		}
		else
		if (!custom_extract
		 && (strEQ(art_buf,"$\n")
		  || strEQ(art_buf,"$ f\n"))) {
		    savefrom = ftell(artfp)-strlen(art_buf)-NL_SIZE+1;
		    if (found_cut
		     || (fgets(art_buf,LBUFLEN,artfp) != Nullch
		      && (strnEQ(art_buf, "ship ", 5)
		       || strnEQ(art_buf, "cont ", 5)))) {
			decode_type = UNSHIP;
			goto decode_it;
		    }
		}
		else
		if (!custom_extract
		 && (strEQ(art_buf,"table\n")
		  || strnEQ(art_buf,"begin ", 6))) {
		    decode_type = UUDECODE;
		    savefrom = ftell(artfp)-strlen(art_buf)-NL_SIZE+1;
		 decode_it:
		    printf("Extracting %s file into %s:\n",
			decode_type == UNSHIP? "shipped":"uuencoded", s) FLUSH;
		    if (extractprog)
			free(extractprog);
		    extractprog = savestr("-");
		    fseek(artfp, savefrom, 0);
		    cnt = 0;
		    if (decode_type == UUDECODE) {
			uud_start();
			uudecode(artfp);
		    } else
			unship(artfp);
		    break;
		}
		else {
		    if (cut_line(art_buf)) {
			savefrom = ftell(artfp);
			found_cut = TRUE;
		    }
		    else if (found_cut || ++cnt == 300) {
			break;
		    }
		}
	    }/* while */
	    if (cnt) {
		if (custom_extract)
		    printf("Didn't find cut line for extraction to '%s'.\n",
			extractprog) FLUSH;
		else
		    printf("Unable to determine type of file.\n") FLUSH;
	    }
	}/* if */
    }
    else if ((s = index(buf,'|')) != Nullch) {
				/* is it a pipe command? */
	s++;			/* skip the | */
	while (*s == ' ') s++;
	safecpy(altbuf,filexp(s),sizeof altbuf);
	if (savedest)
	    free(savedest);
	savedest = savestr(altbuf);
	interp(cmd_buf, (sizeof cmd_buf), getval("PIPESAVER",PIPESAVER));
				/* then set up for command */
	termlib_reset();
	resetty();		/* restore tty state */
	if (use_pref)		/* use preferred shell? */
	    doshell(Nullch,cmd_buf);
				/* do command with it */
	else
	    doshell(sh,cmd_buf);	/* do command with sh */
	noecho();		/* and stop echoing */
	crmode();		/* and start cbreaking */
	termlib_init();
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
	if (*s != '/') {
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
	}
	makedir(s,MD_FILE);
	if (*s != '/') {		/* relative path? */
	    c = (s==buf ? altbuf : buf);
	    sprintf(c, "%s/%s", cwd, s);
	    s = c;			/* absolutize it */
	}
	if (savedest)
	    free(savedest);
	s = savedest = savestr(s);	/* doesn't move any more */
					/* make it handy for %b */
	if (!there) {
	    if (mbox_always)
		mailbox = TRUE;
	    else if (norm_always)
		mailbox = FALSE;
	    else {
		char *dflt = (instr(savename,"%a", TRUE) ? "nyq" : "ynq");
		
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
		if (read(tmpfd,buf,LBUFLEN)) {
		    c = buf;
		    if (!isspace(MBOXCHAR))   /* if non-zero, */
			while (isspace(*c))   /* check the first character */
			    c++;
		    mailbox = (*c == MBOXCHAR);
		} else {
		    mailbox = mbox_always;    /* if zero length, recheck -M */
		}
		close(tmpfd);
	    }
	}

	safecpy(cmd_buf, filexp(mailbox ?
	    getval("MBOXSAVER",MBOXSAVER) :
	    getval("NORMSAVER",NORMSAVER) ), sizeof cmd_buf);
				/* format the command */
	termlib_reset();
	resetty();		/* make terminal behave */
	if (doshell(use_pref?Nullch:SH,cmd_buf)) {
	    termlib_init();
	    fputs("Not saved",stdout);
	} else {
	    termlib_init();
	    printf("%s to %s %s",
	      there?"Appended":"Saved",
	      mailbox?"mailbox":"file",
	      s);
	}
	if (interactive)
	    putchar('\n') FLUSH;
	noecho();		/* make terminal do what we want */
	crmode();
    }
s_bomb:
#ifdef USE_NNTP
    if (chdir(spool)) {
#else
    if (chdir(spool) || chdir(ngdir)) {
#endif
	printf(nocd,ngdir) FLUSH;
	sig_catcher(0);
    }
    return SAVE_DONE;
}

int
cancel_article()
{
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
    ngs_buf = fetchlines(art,NGS_LINE);
    if (!instr(from_buf,phostname,FALSE) ||
	(!instr(from_buf,logname,TRUE) &&
	 !instr(reply_buf,logname,TRUE) &&
#ifdef NEWS_ADMIN
	 myuid != newsuid &&
#endif
	 myuid != ROOTID ) ) {
#ifdef DEBUG
	    if (debug)
		printf("\n%s@%s != %s\n",logname,phostname,from_buf) FLUSH;
#endif
#ifdef VERBOSE
	    IF(verbose)
		fputs("\nYou can't cancel someone else's article\n",stdout)
		  FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("\nNot your article\n",stdout) FLUSH;
#endif
    }
    else {
	tmpfp = fopen(headname,"w");	/* open header file */
	if (tmpfp == Nullfp) {
	    printf(cantcreate,headname) FLUSH;
	    goto no_cancel;
	}
	interp(buf, (sizeof buf), getval("CANCELHEADER",CANCELHEADER));
	fputs(buf,tmpfp);
	fclose(tmpfp);
	fputs("\nCanceling...\n",stdout) FLUSH;
	r = doshell(sh,filexp(getval("CANCEL",CANCEL)));
    }
no_cancel:
    free(ngs_buf);
    free(from_buf);
    free(reply_buf);
    return r;
}

int
supersede_article()		/* Supersedes: */
{
    char *ngs_buf;
    char *from_buf;
    char *reply_buf;
    int myuid = getuid();
    int r = -1;
    bool incl_body = (*buf == 'Z');

    if (artopen(art) == Nullfp) {
#ifdef VERBOSE
	IF(verbose)
	    fputs("\n\
Superceding null articles is your idea of fun?  :-)\n\
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
    ngs_buf = fetchlines(art,NGS_LINE);
    if (!instr(from_buf,phostname,FALSE) ||
	(!instr(from_buf,logname,TRUE) &&
	 !instr(reply_buf,logname,TRUE) &&
#ifdef NEWS_ADMIN
	 myuid != newsuid &&
#endif
	 myuid != ROOTID ) ) {
#ifdef DEBUG
	    if (debug)
		printf("\n%s@%s != %s\n",logname,phostname,from_buf) FLUSH;
#endif
#ifdef VERBOSE
	    IF(verbose)
		fputs("\nYou can't supersede someone else's article\n",stdout)
		  FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("\nNot your article\n",stdout) FLUSH;
#endif
    }
    else {
	tmpfp = fopen(headname,"w");	/* open header file */
	if (tmpfp == Nullfp) {
	    printf(cantcreate,headname) FLUSH;
	    goto no_commute;
	}
	interp(buf, (sizeof buf), getval("SUPERSEDEHEADER",SUPERSEDEHEADER));
	fputs(buf,tmpfp);
	if (incl_body && artfp != Nullfp) {
	    parseheader(art);
	    fseek(artfp,(long)htype[PAST_HEADER].ht_minpos,0);
	    while (fgets(buf,LBUFLEN,artfp) != Nullch) {
		fputs(buf,tmpfp);
	    }
	}
	fclose(tmpfp);
    	safecpy(cmd_buf,filexp(getval("NEWSPOSTER",NEWSPOSTER)),
		sizeof cmd_buf);
    	invoke(cmd_buf,origdir);
	r = 0;
    }
no_commute:
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
    if (!instr(maildoer,"%h",TRUE))
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
	parseheader(art);
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
    char hbuf[4*LBUFLEN];	/* four times the old size */
    ART_NUM oldart = art;

    if (!incl_body && art <= lastart) {
	in_answer("\n\nAre you starting an unrelated topic? [ynq] ", 'F');
	setdef(buf,"y");
	if (*buf == 'q')  /*TODO: need to add 'h' also */
	    return;
	if (*buf != 'n')
	    art = lastart + 1;
    }
    artopen(art);
    tmpfp = fopen(headname,"w");
    if (tmpfp == Nullfp) {
	printf(cantcreate,headname) FLUSH;
	art = oldart;
	return;
    }
    interp(hbuf, (sizeof hbuf), getval("NEWSHEADER",NEWSHEADER));
    fprintf(tmpfp,"%s",hbuf);
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
	parseheader(art);
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
    art = oldart;
}

void
invoke(cmd,dir)
char *cmd,*dir;
{
    if (chdir(dir)) {
	printf(nocd,dir) FLUSH;
	return;
    }
    termlib_reset();
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
    termlib_init();
#ifdef USE_NNTP
    if (chdir(spool)) {
#else
    if (chdir(spool) || chdir(ngdir)) {
#endif
	printf(nocd,ngdir) FLUSH;
	sig_catcher(0);
    }
}

/*
** cut_line() determines if a line is meant as a "cut here" marker.
** Some examples that we understand:
**
**  BEGIN--cut here--cut here
**
**  ------------------ tear at this line ------------------
**
**  #----cut here-----cut here-----cut here-----cut here----#
*/
bool
cut_line(str)
char *str;
{
    char *cp, got_flag;
    char word[80];
    int  dash_cnt, equal_cnt, other_cnt;

    /* Disallow any single-/double-quoted, parenthetical or c-commented
    ** string lines.  Make sure it has the cut-phrase and at least six
    ** '-'s or '='s.  If only four '-'s are present, check for a duplicate
    ** of the cut phrase.  If over 20 unknown characters are encountered,
    ** assume it isn't a cut line.  If we succeed, return TRUE.
    */
    for (cp = str, dash_cnt = equal_cnt = other_cnt = 0; *cp; cp++) {
	switch (*cp) {
	case '-':
	    dash_cnt++;
	    break;
	case '=':
	    equal_cnt++;
	    break;
	case '/':
	    if(*(cp+1) != '*') {
		break;
	    }
	case '"':
	case '\'':
	case '(':
	case ')':
	case '[':
	case ']':
	case '{':
	case '}':
	    return FALSE;
	default:
	    other_cnt++;
	    break;
	}
    }
    if (dash_cnt < 4 && equal_cnt < 6)
	return FALSE;

    got_flag = 0;

    for (*(cp = word) = '\0'; *str; str++) {
	if (islower(*str))
	    *cp++ = *str;
	else if (isupper(*str))
	    *cp++ = tolower(*str);
	else {
	    if (*word) {
		*cp = '\0';
		switch (got_flag) {
		case 2:
		    if (!strcmp(word, "line")
		     || !strcmp(word, "here"))
			if ((other_cnt -= 4) <= 20)
			    return TRUE;
		    break;
		case 1:
		    if (!strcmp(word, "this")) {
			got_flag = 2;
			other_cnt -= 4;
		    }
		    else if (!strcmp(word, "here")) {
			other_cnt -= 4;
			if ((dash_cnt >= 6 || equal_cnt >= 6)
			 && other_cnt <= 20)
			    return TRUE;
			dash_cnt = 6;
			got_flag = 0;
		    }
		    break;
		case 0:
		    if (!strcmp(word, "cut")
		     || !strcmp(word, "snip")
		     || !strcmp(word, "tear")) {
			got_flag = 1;
			other_cnt -= strlen(word);
		    }
		    break;
		}
		*(cp = word) = '\0';
	    }
	}
    } /* for *str */

    return FALSE;
}
