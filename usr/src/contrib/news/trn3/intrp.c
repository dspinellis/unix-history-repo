/* $Id: intrp.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "search.h"
#include "cache.h"
#include "bits.h"
#include "head.h"
#include "trn.h"
#include "artsrch.h"
#include "ng.h"
#include "ngdata.h"
#include "respond.h"
#include "rcstuff.h"
#include "artio.h"
#include "init.h"
#include "term.h"
#include "final.h"
#include "rthread.h"
#include "rt-select.h"
#include "nntp.h"
#include "INTERN.h"
#include "intrp.h"

static char * regexp_specials = "^$.*[\\/?%";

char orgname[] = ORGNAME;

#ifdef HAS_UNAME
#include <sys/utsname.h>
struct utsname utsn;
#endif

#ifdef TILDENAME
static char *tildename = Nullch;
static char *tildedir = Nullch;
#endif

#ifdef CONDSUB
char *skipinterp _((char *,char *));
#endif

static void abort_interp _((void));

void
intrp_init(tcbuf)
char *tcbuf;
{
    char *getlogin();
    
    /* get environmental stuff */

#ifdef NEWS_ADMIN
    {
#ifdef HAS_GETPWENT
	struct passwd *getpwnam();
	struct passwd *pwd = getpwnam(NEWS_ADMIN);

	if (pwd != NULL)
	    newsuid = pwd->pw_uid;
#else
#ifdef TILDENAME
	char tildenews[2+sizeof NEWS_ADMIN];
	strcpy(tildenews, "~");
	strcat(tildenews, NEWS_ADMIN);
	(void) filexp(tildenews);
#else
	??? "Define either HAS_GETPWENT or TILDENAME to get NEWS_ADMIN"
#endif  /* TILDENAME */
#endif	/* HAS_GETPWENT */
    }
#endif	/* NEWS_ADMIN */
    /* get home directory */

    homedir = getenv("HOME");
    if (homedir == Nullch)
	homedir = getenv("LOGDIR");

    dotdir = getval("DOTDIR",homedir);

    /* get login name */

    logname = getenv("USER");
    if (logname == Nullch)
	logname = getenv("LOGNAME");
#ifdef GETLOGIN
    if (logname == Nullch)
	logname = savestr(getlogin());
#endif

    spool = savestr(filexp(NEWSSPOOL));	/* usually /usr/spool/news */
    threaddir = filexp(THREAD_DIR);
    if (strEQ(threaddir,spool))
	threaddir = spool;
    else
	threaddir = savestr(threaddir);
    overviewdir = filexp(OVERVIEW_DIR);
    if (strEQ(overviewdir,spool))
	overviewdir = spool;
    else
	overviewdir = savestr(overviewdir);

#ifdef NEWS_ADMIN
    /* if this is the news admin than load his UID into newsuid */

    if ( strEQ(logname,NEWS_ADMIN) )
	newsuid = getuid();
#endif

    if (checkflag)			/* that getwd below takes ~1/3 sec. */
	return;				/* and we do not need it for -c */
    getwd(tcbuf);			/* find working directory name */
    origdir = savestr(tcbuf);		/* and remember it */

    /* get the real name of the person (%N) */
    /* Must be done after logname is read in because BERKNAMES uses that */

    strcpy(tcbuf,getrealname((long)getuid()));
    realname = savestr(tcbuf);

    /* name of header file (%h) */

    headname = savestr(filexp(HEADNAME));

    /* host name that goes in postings (%H) */

    phostname = PHOSTNAME;
    if (*phostname == '/') {
	if ((tmpfp = fopen(phostname,"r")) == NULL) {
	    printf("Warning: Couldn't open %s to determine hostname!\n",
		   phostname); 
	    sig_catcher(0);
	}
	fgets(buf, sizeof(buf), tmpfp);
	if (buf[strlen(buf)-1] == '\n')
	    buf[strlen(buf)-1] = 0;
	fclose(tmpfp);
    }
    else {
#ifdef HAS_GETHOSTNAME
	gethostname(buf,sizeof buf);
#else
# ifdef HAS_UNAME
	/* get sysname */
	uname(&utsn);
	strcpy(buf,utsn.nodename);
# else
#  ifdef PHOSTCMD
       {
	FILE *popen();
	FILE *pipefp = popen(PHOSTCMD,"r");
	
	if (pipefp == Nullfp) {
	    printf("Can't find hostname\n");
	    sig_catcher(0);
	}
	fgets(buf,sizeof buf,pipefp);
	buf[strlen(buf)-1] = '\0';	/* wipe out newline */
	pclose(pipefp);
       }
#  else
	*buf = '\0';
#  endif /* PHOSTCMD */
# endif /* HAS_UNAME */
#endif /* HAS_GETHOSTNAME */
	if (*buf) {
	    char *cp = index(buf,'.');
	    if (cp)
		*cp = '\0';
	    cp = index(phostname,'.');
	    if (cp)
		strcat(buf,cp);
	    phostname = savestr(buf);
	}
    }
}

/* expand filename via %, ~, and $ interpretation */
/* returns pointer to static area */
/* Note that there is a 1-deep cache of ~name interpretation */

char *
filexp(s)
register char *s;
{
    static char filename[CBUFLEN];
    char scrbuf[CBUFLEN];
    register char *d;

#ifdef DEBUG
    if (debug & DEB_FILEXP)
	printf("< %s\n",s) FLUSH;
#endif
    interp(filename, (sizeof filename), s);	
					/* interpret any % escapes */
#ifdef DEBUG
    if (debug & DEB_FILEXP)
	printf("%% %s\n",filename) FLUSH;
#endif
    s = filename;
    if (*s == '~') {	/* does destination start with ~? */
	if (!*(++s) || *s == '/') {
	    sprintf(scrbuf,"%s%s",homedir,s);
				/* swap $HOME for it */
#ifdef DEBUG
    if (debug & DEB_FILEXP)
	printf("~ %s\n",scrbuf) FLUSH;
#endif
	    strcpy(filename,scrbuf);
	}
	else {
#ifdef TILDENAME
	    for (d=scrbuf; isalnum(*s); s++,d++)
		*d = *s;
	    *d = '\0';
	    if (tildedir && strEQ(tildename,scrbuf)) {
		strcpy(scrbuf,tildedir);
		strcat(scrbuf, s);
		strcpy(filename, scrbuf);
#ifdef DEBUG
		if (debug & DEB_FILEXP)
		    printf("r %s %s\n",tildename,tildedir) FLUSH;
#endif
	    }
	    else {
		if (tildename) {
		    free(tildename);
		    free(tildedir);
		}
		tildedir = Nullch;
		tildename = savestr(scrbuf);
#ifdef HAS_GETPWENT	/* getpwnam() is not the paragon of efficiency */
		{
#ifdef notdef
		    struct passwd *getpwnam _((char*));
#endif
		    struct passwd *pwd = getpwnam(tildename);
		    if ( pwd == NULL){
			printf("%s is an unknown user. Using default.\n",tildename) FLUSH;
			return(Nullch);
		    }
		    sprintf(scrbuf,"%s%s",pwd->pw_dir,s);
		    tildedir = savestr(pwd->pw_dir);
		    strcpy(filename,scrbuf);
		    endpwent();
		}
#else			/* this will run faster, and is less D space */
		{	/* just be sure LOGDIRFIELD is correct */
		    FILE *pfp = fopen("/etc/passwd","r");
		    char tmpbuf[512];
		    int i;
		    
		    if (pfp == Nullfp) {
			printf(cantopen,"passwd") FLUSH;
			sig_catcher(0);
		    }
		    while (fgets(tmpbuf,512,pfp) != Nullch) {
			d = cpytill(scrbuf,tmpbuf,':');
#ifdef DEBUG
			if (debug & DEB_FILEXP)
			    printf("p %s\n",tmpbuf) FLUSH;
#endif
			if (strEQ(scrbuf,tildename)) {
			    for (i=LOGDIRFIELD-2; i; i--) {
				if (d)
				    d = index(d+1,':');
			    }
			    if (d) {
				cpytill(scrbuf,d+1,':');
				tildedir = savestr(scrbuf);
				strcat(scrbuf,s);
				strcpy(filename,scrbuf);
			    }
			    break;
			}
		    }
		    fclose(pfp);
		}
#endif
	    }
#else /* !TILDENAME */
#ifdef VERBOSE
	    IF(verbose)
		fputs("~loginname not implemented.\n",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("~login not impl.\n",stdout) FLUSH;
#endif
#endif
	}
    }
    else if (*s == '$') {	/* starts with some env variable? */
	d = scrbuf;
	*d++ = '%';
	if (s[1] == '{')
	    strcpy(d,s+2);
	else {
	    *d++ = '{';
	    for (s++; isalnum(*s); s++) *d++ = *s;
				/* skip over token */
	    *d++ = '}';
	    strcpy(d,s);
	}
#ifdef DEBUG
	if (debug & DEB_FILEXP)
	    printf("$ %s\n",scrbuf) FLUSH;
#endif
	interp(filename, (sizeof filename), scrbuf);
					/* this might do some extra '%'s but */
					/* that is how the Mercedes Benz */
    }
#ifdef DEBUG
    if (debug & DEB_FILEXP)
	printf("> %s\n",filename) FLUSH;
#endif
    return filename;
}

#ifdef CONDSUB
/* skip interpolations */

char *
skipinterp(pattern,stoppers)
register char *pattern;
char *stoppers;
{

    while (*pattern && (!stoppers || !index(stoppers,*pattern))) {
#ifdef DEBUG
	if (debug & DEB_INTRP)
	    printf("skipinterp till %s at %s\n",stoppers?stoppers:"",pattern);
#endif
	if (*pattern == '%' && pattern[1]) {
	    switch (*++pattern) {
	    case '{':
		for (pattern++; *pattern && *pattern != '}'; pattern++)
		    if (*pattern == '\\')
			pattern++;
		break;
	    case '[':
		for (pattern++; *pattern && *pattern != ']'; pattern++)
		    if (*pattern == '\\')
			pattern++;
		break;
#ifdef CONDSUB
	    case '(': {
		pattern = skipinterp(pattern+1,"!=");
		if (!*pattern)
		    goto getout;
		for (pattern++; *pattern && *pattern != '?'; pattern++)
		    if (*pattern == '\\')
			pattern++;
		if (!*pattern)
		    goto getout;
		pattern = skipinterp(pattern+1,":)");
		if (*pattern == ':')
		    pattern = skipinterp(pattern+1,")");
		break;
	    }
#endif
#ifdef BACKTICK
	    case '`': {
		pattern = skipinterp(pattern+1,"`");
		break;
	    }
#endif
#ifdef PROMPTTTY
	    case '"':
		pattern = skipinterp(pattern+1,"\"");
		break;
#endif
	    default:
		break;
	    }
	    pattern++;
	}
	else {
	    if (*pattern == '^' && pattern[1])
		pattern += 2;
	    else if (*pattern == '\\' && pattern[1])
		pattern += 2;
	    else
		pattern++;
	}
    }
getout:
    return pattern;			/* where we left off */
}
#endif

/* interpret interpolations */

char *
dointerp(dest,destsize,pattern,stoppers)
register char *dest;
register int destsize;
register char *pattern;
char *stoppers;
{
    char *subj_buf = Nullch;
    char *ngs_buf = Nullch;
    char *refs_buf = Nullch;
    char *artid_buf = Nullch;
    char *reply_buf = Nullch;
    char *from_buf = Nullch;
    char *path_buf = Nullch;
    char *follow_buf = Nullch;
    char *dist_buf = Nullch;
    char *line_buf = Nullch;
    register char *s, *h;
    register int i;
    char scrbuf[512];
    char spfbuf[512];
    bool upper = FALSE;
    bool lastcomp = FALSE;
    bool re_quote = FALSE;
    bool proc_sprintf = FALSE;
    int metabit = 0;

    while (*pattern && (!stoppers || !index(stoppers,*pattern))) {
#ifdef DEBUG
	if (debug & DEB_INTRP)
	    printf("dointerp till %s at %s\n",stoppers?stoppers:"",pattern);
#endif
	if (*pattern == '%' && pattern[1]) {
	    upper = FALSE;
	    lastcomp = FALSE;
	    re_quote = FALSE;
	    proc_sprintf = FALSE;
	    for (s=Nullch; !s; ) {
		switch (*++pattern) {
		case '^':
		    upper = TRUE;
		    break;
		case '_':
		    lastcomp = TRUE;
		    break;
		case '\\':
		    re_quote = TRUE;
		    break;
		case ':':
		    proc_sprintf = TRUE;
		    h = spfbuf;
		    *h++ = '%';
		    pattern++;	/* Skip over ':' */
		    while (*pattern
		     && (*pattern=='.' || *pattern=='-' || isdigit(*pattern))) {
			*h++ = *pattern++;
		    }
		    *h++ = 's';
		    *h++ = '\0';
		    pattern--;
		    break;
		case '/':
#ifdef ARTSRCH
		    s = scrbuf;
		    if (!index("/?g",pattern[-2]))
			*s++ = '/';
		    strcpy(s,lastpat);
		    s += strlen(s);
		    if (pattern[-2] != 'g') {
			if (index("/?",pattern[-2]))
			    *s++ = pattern[-2];
			else
			    *s++ = '/';
			if (art_howmuch == 1)
			    *s++ = 'h';
			else if (art_howmuch == 2)
			    *s++ = 'a';
			if (art_doread)
			    *s++ = 'r';
		    }
		    *s = '\0';
		    s = scrbuf;
#else
		    s = nullstr;
#endif
		    break;
		case '{':
		    pattern = cpytill(scrbuf,pattern+1,'}');
		    if (s = index(scrbuf,'-'))
			*s++ = '\0';
		    else
			s = nullstr;
		    s = getval(scrbuf,s);
		    break;
		case '[':
		    pattern = cpytill(scrbuf,pattern+1,']');
		    i = set_line_type(scrbuf,scrbuf+strlen(scrbuf));
		    if (line_buf)
			free(line_buf);
		    s = line_buf = fetchlines(art,i);
		    break;
#ifdef CONDSUB
		case '(': {
		    COMPEX *oldbra_compex = bra_compex;
		    COMPEX cond_compex;
		    char rch;
		    bool matched;
		    
		    init_compex(&cond_compex);
		    pattern = dointerp(dest,destsize,pattern+1,"!=");
		    rch = *pattern;
		    if (rch == '!')
			pattern++;
		    if (*pattern != '=')
			goto getout;
		    pattern = cpytill(scrbuf,pattern+1,'?');
		    if (!*pattern)
			goto getout;
		    if (s = compile(&cond_compex,scrbuf,TRUE,TRUE)) {
			printf("%s: %s\n",scrbuf,s) FLUSH;
			pattern += strlen(pattern);
			goto getout;
		    }
		    matched = (execute(&cond_compex,dest) != Nullch);
		    if (cond_compex.nbra)	/* were there brackets? */
			bra_compex = &cond_compex;
		    if (matched==(rch == '=')) {
			pattern = dointerp(dest,destsize,pattern+1,":)");
			if (*pattern == ':')
			    pattern = skipinterp(pattern+1,")");
		    }
		    else {
			pattern = skipinterp(pattern+1,":)");
			if (*pattern == ':')
			    pattern++;
			pattern = dointerp(dest,destsize,pattern,")");
		    }
		    s = dest;
		    bra_compex = oldbra_compex;
		    free_compex(&cond_compex);
		    break;
		}
#endif
#ifdef BACKTICK
		case '`': {
		    FILE *pipefp, *popen();

		    pattern = dointerp(scrbuf,(sizeof scrbuf),pattern+1,"`");
		    pipefp = popen(scrbuf,"r");
		    if (pipefp != Nullfp) {
			int len;

			len = fread(scrbuf,sizeof(char),(sizeof scrbuf)-1,
			    pipefp);
			scrbuf[len] = '\0';
			pclose(pipefp);
		    }
		    else {
			printf("\nCan't run %s\n",scrbuf);
			*scrbuf = '\0';
		    }
		    for (s=scrbuf; *s; s++) {
			if (*s == '\n') {
			    if (s[1])
				*s = ' ';
			    else
				*s = '\0';
			}
		    }
		    s = scrbuf;
		    break;
		}
#endif
#ifdef PROMPTTTY
		case '"':
		    pattern = dointerp(scrbuf,(sizeof scrbuf),pattern+1,"\"");
		    fputs(scrbuf,stdout) FLUSH;
		    resetty();
		    gets(scrbuf);
		    noecho();
		    crmode();
		    s = scrbuf;
		    break;
#endif
		case '~':
		    s = homedir;
		    break;
		case '.':
		    s = dotdir;
		    break;
		case '$':
		    s = scrbuf;
		    sprintf(s,"%ld",our_pid);
		    break;
		case '#':
		    s = scrbuf;
		    sprintf(s,"%d",perform_cnt);
		    break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
#ifdef CONDSUB
		    s = getbracket(bra_compex,*pattern - '0');
#else
		    s = nullstr;
#endif
		    break;
		case 'a':
		    s = scrbuf;
		    sprintf(s,"%ld",(long)art);
		    break;
		case 'A':
#ifdef LINKART
		    s = linkartname;	/* so Eunice people get right file */
#else
		    s = scrbuf;
#ifdef USE_NNTP
		    sprintf(s,"%s/%s",spool,nntp_artname());
#else
		    sprintf(s,"%s/%s/%ld",spool,ngdir,(long)art);
#endif
#endif
		    break;
		case 'b':
		    s = savedest;
		    break;
		case 'B':
		    s = scrbuf;
		    sprintf(s,"%ld",(long)savefrom);
		    break;
		case 'c':
		    s = ngdir;
		    break;
		case 'C':
		    s = ngname;
		    break;
		case 'd':
		    s = scrbuf;
		    sprintf(s,"%s/%s",spool,ngdir);
		    break;
		case 'D':
		    s = dist_buf = fetchlines(art,DIST_LINE);
		    break;
		case 'e':
		    s = (extractprog ? extractprog : "-");
		    break;
		case 'E':
		    s = extractdest;
		    break;
		case 'f':			/* from line */
		    parseheader(art);
		    if (htype[REPLY_LINE].ht_minpos >= 0) {
						/* was there a reply line? */
			if (!(s=reply_buf))
			    s = reply_buf = fetchlines(art,REPLY_LINE);
		    }
		    else if (!(s = from_buf))
			s = from_buf = fetchlines(art,FROM_LINE);
		    break;
		case 'F':
		    parseheader(art);
		    if (htype[FOLLOW_LINE].ht_minpos >= 0)
					/* is there a Followup-To line? */
			s = follow_buf = fetchlines(art,FOLLOW_LINE);
		    else 
			s = ngs_buf = fetchlines(art,NGS_LINE);
		    break;
		case 'h':			/* header file name */
		    s = headname;
		    break;
		case 'H':			/* host name in postings */
		    s = phostname;
		    break;
		case 'i':
		    if (!(s=artid_buf))
			s = artid_buf = fetchlines(art,MESSID_LINE);
		    if (*s && *s != '<') {
			sprintf(scrbuf,"<%s>",artid_buf);
			s = scrbuf;
		    }
		    break;
		case 'I':			/* ref article indicator */
		    s = scrbuf;
		    sprintf(scrbuf,"'%s'",indstr);
		    break;
		case 'l':			/* rn library */
#ifdef NEWS_ADMIN
		    s = newsadmin;
#else
		    s = "???";
#endif
		    break;
		case 'L':			/* login id */
		    s = logname;
		    break;
		case 'm':		/* current mode */
		    s = scrbuf;
		    *s = mode;
		    s[1] = '\0';
		    break;
		case 'M':
		    sprintf(scrbuf,"%ld",(long)dmcount);
		    s = scrbuf;
		    break;
		case 'n':			/* newsgroups */
		    s = ngs_buf = fetchlines(art,NGS_LINE);
		    break;
		case 'N':			/* full name */
		    s = getval("NAME",realname);
		    break;
		case 'o':			/* organization */
#ifdef IGNOREORG
		    s = getval("NEWSORG",orgname); 
#else
		    s = getenv("NEWSORG");
		    if (s == Nullch) 
			s = getval("ORGANIZATION",orgname); 
#endif
#ifdef ORGFILE
		    if (*s == '/') {
			FILE *ofp = fopen(s,"r");

			if (ofp) {
			    fgets(scrbuf,sizeof scrbuf,ofp);
			    fclose(ofp);
			    s = scrbuf+strlen(scrbuf)-1;
			    if (*s == '\n')
				*s = '\0';
			    s = scrbuf;
			}
		    }
#endif
		    break;
		case 'O':
		    s = origdir;
		    break;
		case 'p':
		    s = cwd;
		    break;
		case 'P':
		    s = spool;
		    break;
		case 'r':
		    parseheader(art);
		    if (htype[REFS_LINE].ht_minpos >= 0) {
			refs_buf = fetchlines(art,REFS_LINE);
			refscpy(scrbuf,(sizeof scrbuf),refs_buf);
		    }
		    else
			*scrbuf = '\0';
		    s = rindex(scrbuf,'<');
		    break;
		case 'R':
		    parseheader(art);
		    if (htype[REFS_LINE].ht_minpos >= 0) {
			refs_buf = fetchlines(art,REFS_LINE);
			refscpy(scrbuf,(sizeof scrbuf),refs_buf);
			/* no more than 3 prior references PLUS the
			** root article allowed, including the one
			** concatenated below */
			if ((s = rindex(scrbuf,'<')) > scrbuf) {
			    *s = '\0';
			    h = rindex(scrbuf,'<');
			    *s = '<';
			    if (h > scrbuf) {
				s = index(scrbuf+1,'<');
				if (s < h)
				    strcpy(s,h);
			    }
			}
		    }
		    else
			*scrbuf = '\0';
		    if (!artid_buf)
			artid_buf = fetchlines(art,MESSID_LINE);
		    if (artid_buf[0] == '<')
			safecat(scrbuf,artid_buf,sizeof(scrbuf));
		    else if (artid_buf[0]) {
			char tmpbuf[64];
    
			sprintf(tmpbuf,"<%s>",artid_buf);
			safecat(scrbuf,tmpbuf,sizeof(scrbuf));
		    }
		    s = scrbuf;
		    break;
		case 's':
		    if (!(s=subj_buf))
			s = subj_buf = fetchsubj(art,TRUE);
						/* get subject handy */
		    while ((*s=='R'||*s=='r')&&(s[1]=='E'||s[1]=='e')&&s[2]==':') {
						/* skip extra Re: */
			s += 3;
			if (*s == ' ')
			    s++;
		    }
		    if (h = instr(s,"- (nf", TRUE))
			*h = '\0';
		    break;
		case 'S':
		    if (!(s=subj_buf))
			s = subj_buf = fetchsubj(art,TRUE);
						/* get subject handy */
		    if ((*s=='R'||*s=='r')&&(s[1]=='E'||s[1]=='e')&&s[2]==':') {
						/* skip extra Re: */
			s += 3;
			if (*s == ' ')
			    s++;
		    }
		    break;
		case 't':
		case 'T':
		    parseheader(art);
		    if (htype[REPLY_LINE].ht_minpos >= 0) {
					/* was there a reply line? */
			if (!(s=reply_buf))
			    s = reply_buf = fetchlines(art,REPLY_LINE);
		    }
		    else if (!(s = from_buf))
			s = from_buf = fetchlines(art,FROM_LINE);
		    else
			s = "noname";
		    if (*pattern == 'T') {
			if (htype[PATH_LINE].ht_minpos >= 0) {
					/* should we substitute path? */
			    s = path_buf = fetchlines(art,PATH_LINE);
			}
			i = strlen(phostname);
			if (strnEQ(phostname,s,i) && s[i] == '!')
			    s += i + 1;
		    }
		    if ((h=index(s,'(')) != Nullch)
						/* strip garbage from end */
			*(h-1) = '\0';
		    else if ((h=index(s,'<')) != Nullch) {
						/* or perhaps from beginning */
			s = h+1;
			if ((h=index(s,'>')) != Nullch)
			    *h = '\0';
		    }
		    break;
		case 'u':
		    sprintf(scrbuf,"%ld",(long)toread[ng]);
		    s = scrbuf;
		    break;
		case 'U': {
		    int unseen;

		    unseen = (art <= lastart) && !was_read(art);
		    if (selected_only) {
			int selected;

			selected = (curr_artp->flags & AF_SEL);
			sprintf(scrbuf,"%ld",
				(long)selected_count - (selected && unseen));
		    }
		    else
			sprintf(scrbuf,"%ld",(long)toread[ng] - unseen);
		    s = scrbuf;
		    break;
		}
		case 'v': {
		    int selected, unseen;

		    selected = (curr_artp->flags & AF_SEL);
		    unseen = (art <= lastart) && !was_read(art);
		    sprintf(scrbuf,"%ld",(long)toread[ng] - selected_count
					 - (!selected && unseen));
		    s = scrbuf;
		    break;
		}
		case 'W':
		    s = threaddir;
		    break;
		case 'x':			/* news library */
		    s = lib;
		    break;
		case 'X':			/* rn library */
		    s = rnlib;
		    break;
		case 'z':
#ifdef LINKART
		    s = linkartname;	/* so Eunice people get right file */
#else
		    s = scrbuf;
		    sprintf(s,"%ld",(long)art);
#endif
		    if (stat(s,&filestat) < 0)
			filestat.st_size = 0L;
		    sprintf(scrbuf,"%5ld",(long)filestat.st_size);
		    s = scrbuf;
		    break;
		case 'Z':
		    sprintf(scrbuf,"%ld",(long)selected_count);
		    s = scrbuf;
		    break;
		default:
		    if (--destsize <= 0)
			abort_interp();
		    *dest++ = *pattern | metabit;
		    s = nullstr;
		    break;
		}
	    }
	    if (!s)
		s = nullstr;
	    if (proc_sprintf) {
		sprintf(scrbuf,spfbuf,s);
		s = scrbuf;
	    }
	    pattern++;
	    if (upper || lastcomp) {
		char *t;

		if (s != scrbuf) {
		    safecpy(scrbuf,s,(sizeof scrbuf));
		    s = scrbuf;
		}
		if (upper || !(t=rindex(s,'/')))
		    t = s;
		while (*t && !isalpha(*t))
		    t++;
		if (islower(*t))
		    *t = toupper(*t);
	    }
	    /* Do we have room left? */
	    i = strlen(s);
	    if (destsize <= i)
		abort_interp();
	    destsize -= i;	/* adjust the size now. */

	    /* A maze of twisty little conditions, all alike... */
	    if (metabit) {
		/* set meta bit while copying. */
		i = metabit;		/* maybe get into register */
		if (s == dest) {
		    while (*dest)
			*dest++ |= i;
		} else {
		    while (*s)
			*dest++ = *s++ | i;
		}
	    } else if (re_quote) {
		/* put a backslash before regexp specials while copying. */
		if (s == dest) {
		    /* copy out so we can copy in. */
		    safecpy(scrbuf, s, sizeof scrbuf);
		    s = scrbuf;
		    if (i > sizeof scrbuf)	/* we truncated, ack! */
			destsize += i - sizeof scrbuf;
		}
		while (*s) {
		    if (index(regexp_specials, *s)) {
			if (--destsize <= 0)
			    abort_interp();
			*dest++ = '\\';
		    }
		    *dest++ = *s++;
		}
	    } else {
		/* straight copy. */
		if (s == dest) {
		    dest += i;
		} else {
		    while (*s)
			*dest++ = *s++;
		}
	    }
	}
	else {
	    if (--destsize <= 0)
		abort_interp();
	    if (*pattern == '^' && pattern[1]) {
		++pattern;			/* skip uparrow */
		i = *pattern;		/* get char into a register */
		if (i == '?')
		    *dest++ = '\177' | metabit;
		else if (i == '(') {
		    metabit = 0200;
		    destsize++;
		}
		else if (i == ')') {
		    metabit = 0;
		    destsize++;
		}
		else
		    *dest++ = (i & 037) | metabit;
		pattern++;
	    }
	    else if (*pattern == '\\' && pattern[1]) {
		++pattern;			/* skip backslash */
		i = *pattern;		/* get char into a register */
    
		/* this used to be a switch but the if may save space */
		
		if (i >= '0' && i <= '7') {
		    i = 1;
		    while (i < 01000 && *pattern >= '0' && *pattern <= '7') {
			i <<= 3;
			i += *pattern++ - '0';
		    }
		    *dest++ = (i & 0377) | metabit;
		    --pattern;
		}
		else if (i == 'b')
		    *dest++ = '\b' | metabit;
		else if (i == 'f')
		    *dest++ = '\f' | metabit;
		else if (i == 'n')
		    *dest++ = '\n' | metabit;
		else if (i == 'r')
		    *dest++ = '\r' | metabit;
		else if (i == 't')
		    *dest++ = '\t' | metabit;
		else
		    *dest++ = i | metabit;
		pattern++;
	    }
	    else
		*dest++ = *pattern++ | metabit;
	}
    }
    *dest = '\0';
getout:
    if (subj_buf != Nullch)	/* return any checked out storage */
	free(subj_buf);
    if (ngs_buf != Nullch)
	free(ngs_buf);
    if (refs_buf != Nullch)
	free(refs_buf);
    if (artid_buf != Nullch)
	free(artid_buf);
    if (reply_buf != Nullch)
	free(reply_buf);
    if (from_buf != Nullch)
	free(from_buf);
    if (path_buf != Nullch)
	free(path_buf);
    if (follow_buf != Nullch)
	free(follow_buf);
    if (dist_buf != Nullch)
	free(dist_buf);
    if (line_buf != Nullch)
	free(line_buf);
    return pattern;			/* where we left off */
}

void
interp(dest,destsize,pattern)
char *dest;
int destsize;
char *pattern;
{
    dointerp(dest,destsize,pattern,Nullch);
#ifdef DEBUG
    if (debug & DEB_FILEXP)
	fputs(dest,stdout);
#endif
}

/* copy a references line, normalizing as we go */

void
refscpy(dest,destsize,src)
register char *dest, *src;
register int destsize;
{
    register char *dot, *at, *beg;
    char tmpbuf[64];
    
    while (*src) {
	if (*src != '<') {
	    if (--destsize <= 0)
		break;
	    *dest++ = '<';
	    at = dot = Nullch;
	    beg = src;
	    while (*src && *src != ' ' && *src != ',') {
		if (*src == '.')
		    dot = src;
		else if (*src == '@')
		    at = src;
		if (--destsize <= 0)
		    break;
		*dest++ = *src++;
	    }
	    if (destsize <= 0)
		break;
	    if (dot && !at) {
		int len;

		*dest = *dot++ = '\0';
		sprintf(tmpbuf,"%s@%s.UUCP",dot,beg);
		len = strlen(tmpbuf);
		if (destsize > len) {
		    strcpy(dest,tmpbuf);
		    dest = dest + len;
		    destsize -= len;
		}
	    }
	    if (--destsize <= 0)
		break;
	    *dest++ = '>';
	}
	else {
	    while (*src && --destsize > 0 && (*dest++ = *src++) != '>') ;
	    if (destsize <= 0)
		break;
	}
	while (*src == ' ' || *src == '\t' || *src == '\n' || *src == ',')
	    src++;
	if (*src && --destsize > 0)
	    *dest++ = ' ';
    }
    *dest = '\0';
} 

/* get the person's real name from /etc/passwd */
/* (string is overwritten, so it must be copied) */

char *
getrealname(uid)
long uid;
{
    char *s, *c;

#ifdef PASSNAMES
#ifdef HAS_GETPWENT
#ifdef notdef
    struct passwd *getpwuid _((uid_t));
#endif
    struct passwd *pwd = getpwuid(uid);

    if (!pwd)
	return nullstr;

    s = pwd->pw_gecos;
#else
    char tmpbuf[512];
    int i;

    getpw(uid, tmpbuf);
    for (s=tmpbuf, i=GCOSFIELD-1; i; i--) {
	if (s)
	    s = index(s,':')+1;
    }
    if (!s)
	return nullstr;
    cpytill(tmpbuf,s,':');
    s = tmpbuf;
#endif
#ifdef BERKNAMES
#ifdef BERKJUNK
    while (*s && !isalnum(*s) && *s != '&') s++;
#endif
    if ((c = index(s, ',')) != Nullch)
	*c = '\0';
    if ((c = index(s, ';')) != Nullch)
	*c = '\0';
    s = cpytill(buf,s,'&');
    if (*s == '&') {			/* whoever thought this one up was */
	c = buf + strlen(buf);		/* in the middle of the night */
	strcat(c,logname);		/* before the morning after */
	strcat(c,s+1);
	if (islower(*c))
	    *c = toupper(*c);		/* gack and double gack */
    }
#else
    if ((c = index(s, '(')) != Nullch)
	*c = '\0';
    if ((c = index(s, '-')) != Nullch)
	s = c;
    strcpy(buf,s);
#endif
#ifdef HAS_GETPWENT
    endpwent();
#endif
    return buf;				/* return something static */
#else
    if ((tmpfp=fopen(filexp(FULLNAMEFILE),"r")) != Nullfp) {
	fgets(buf,sizeof buf,tmpfp);
	fclose(tmpfp);
	buf[strlen(buf)-1] = '\0';
	return buf;
    }
    return "PUT YOUR NAME HERE";
#endif
}

static void
abort_interp()
{
    fputs("\n% interp buffer overflow!\n",stdout) FLUSH;
    sig_catcher(0);
}


