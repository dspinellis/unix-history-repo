/* $Header: intrp.c,v 4.3.1.5 85/05/23 17:21:24 lwall Exp $
 *
 * $Log:	intrp.c,v $
 * Revision 4.3.1.5  85/05/23  17:21:24  lwall
 * Now allows 'r' and 'f' on null articles.
 * 
 * Revision 4.3.1.4  85/05/21  13:35:21  lwall
 * Sped up "rn -c" by not doing unnecessary initialization.
 * 
 * Revision 4.3.1.3  85/05/17  10:37:11  lwall
 * Fixed & substitution to capitalize last name too.
 * 
 * Revision 4.3.1.2  85/05/15  14:39:45  lwall
 * Spelled gecos right.
 * 
 * Revision 4.3.1.1  85/05/10  11:33:51  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:40:54  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "search.h"
#include "head.h"
#include "rn.h"
#include "artsrch.h"
#include "ng.h"
#include "util.h"
#include "respond.h"
#include "rcstuff.h"
#include "bits.h"
#include "artio.h"
#include "term.h"
#include "final.h"
#include "INTERN.h"
#include "intrp.h"

char orgname[] = ORGNAME;

/* name of this site */
#ifdef GETHOSTNAME
    char *hostname;
#   undef SITENAME
#   define SITENAME hostname
#else !GETHOSTNAME
#   ifdef DOUNAME
#	include <sys/utsname.h>
	struct utsname uts;
#	undef SITENAME
#	define SITENAME uts.nodename
#   else !DOUNAME
#	ifdef PHOSTNAME
	    char *hostname;
#	    undef SITENAME
#	    define SITENAME hostname
#	else !PHOSTNAME
#	    ifdef WHOAMI
#		undef SITENAME
#		define SITENAME sysname
#	    endif WHOAMI
#	endif PHOSTNAME
#   endif DOUNAME
#endif GETHOSTNAME

#ifdef TILDENAME
static char *tildename = Nullch;
static char *tildedir = Nullch;
#endif

char *realname INIT(Nullch);	/* real name of sender from /etc/passwd */

char *dointerp();
char *getrealname();
#ifdef CONDSUB
char *skipinterp();
#endif

static void abort_interp();

void
intrp_init(tcbuf)
char *tcbuf;
{
    char *getlogin();

    spool = savestr(filexp(SPOOL));	/* usually /usr/spool/news */
    
    /* get environmental stuff */

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
    
    if (checkflag)			/* that getwd below takes ~1/3 sec. */
	return;				/* and we do not need it for -c */
    getwd(tcbuf);			/* find working directory name */
    origdir = savestr(tcbuf);		/* and remember it */

    /* get the real name of the person (%N) */
    /* Must be done after logname is read in because BERKNAMES uses that */

    strcpy(tcbuf,getrealname(getuid()));
    realname = savestr(tcbuf);

    /* name of header file (%h) */

    headname = savestr(filexp(HEADNAME));

    /* name of this site (%H) */

#ifdef GETHOSTNAME
    gethostname(buf,sizeof buf);
    hostname = savestr(buf);
#else
#ifdef DOUNAME
    /* get sysname */
    uname(&uts);
#else
#ifdef PHOSTNAME
    {
	FILE *popen();
	FILE *pipefp = popen(PHOSTNAME,"r");
	
	if (pipefp == Nullfp) {
	    printf("Can't find hostname\n");
	    sig_catcher(0);
	}
	fgets(buf,sizeof buf,pipefp);
	buf[strlen(buf)-1] = '\0';	/* wipe out newline */
	hostname = savestr(buf);
	pclose(pipefp);
    }
#endif
#endif
#endif
    sitename = savestr(SITENAME);
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

#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	printf("< %s\n",s) FLUSH;
#endif
    interp(filename, (sizeof filename), s);			/* interpret any % escapes */
#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	printf("%% %s\n",filename) FLUSH;
#endif
    s = filename;
    if (*s == '~') {	/* does destination start with ~? */
	if (!*(++s) || *s == '/') {
	    sprintf(scrbuf,"%s%s",homedir,s);
				/* swap $HOME for it */
#ifdef DEBUGGING
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
#ifdef DEBUGGING
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
#ifdef GETPWENT		/* getpwnam() is not the paragon of efficiency */
		{
		    struct passwd *getpwnam();
		    struct passwd *pwd = getpwnam(tildename);

		    sprintf(scrbuf,"%s%s",pwd->pw_dir,s);
		    tildedir = savestr(pwd->pw_dir);
#ifdef NEWSADMIN
		    if (strEQ(newsadmin,tildename))
			newsuid = atoi(pwd->pw_uid);
#endif
		    strcpy(filename,scrbuf);
#ifdef GETPWENT
		    endpwent();
#endif
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
#ifdef DEBUGGING
			if (debug & DEB_FILEXP)
			    printf("p %s\n",tmpbuf) FLUSH;
#endif
			if (strEQ(scrbuf,tildename)) {
#ifdef NEWSADMIN
			    if (strEQ(newsadmin,tildename))
				newsuid = atoi(index(d,':')+1);
#endif
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
#else !TILDENAME
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
#ifdef DEBUGGING
	if (debug & DEB_FILEXP)
	    printf("$ %s\n",scrbuf) FLUSH;
#endif
	interp(filename, (sizeof filename), scrbuf);
					/* this might do some extra '%'s but */
					/* that is how the Mercedes Benz */
    }
#ifdef DEBUGGING
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
#ifdef DEBUGGING
	if (debug & 8)
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
    bool upper = FALSE;
    bool lastcomp = FALSE;
    int metabit = 0;

    while (*pattern && (!stoppers || !index(stoppers,*pattern))) {
#ifdef DEBUGGING
	if (debug & 8)
	    printf("dointerp till %s at %s\n",stoppers?stoppers:"",pattern);
#endif
	if (*pattern == '%' && pattern[1]) {
	    upper = FALSE;
	    lastcomp = FALSE;
	    for (s=Nullch; !s; ) {
		switch (*++pattern) {
		case '^':
		    upper = TRUE;
		    break;
		case '_':
		    lastcomp = TRUE;
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
		    sprintf(s,"%d",getpid());
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
		    sprintf(s,"%s/%s/%ld",spool,ngdir,(long)art);
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
		case 'f':			/* from line */
#ifdef ASYNC_PARSE
		    parse_maybe(art);
#endif
		    if (htype[REPLY_LINE].ht_minpos >= 0) {
						/* was there a reply line? */
			if (!(s=reply_buf))
			    s = reply_buf = fetchlines(art,REPLY_LINE);
		    }
		    else if (!(s = from_buf))
			s = from_buf = fetchlines(art,FROM_LINE);
		    break;
		case 'F':
#ifdef ASYNC_PARSE
		    parse_maybe(art);
#endif
		    if (htype[FOLLOW_LINE].ht_minpos >= 0)
					/* is there a Followup-To line? */
			s = follow_buf = fetchlines(art,FOLLOW_LINE);
		    else {
			int off;
		
			s = ngs_buf = fetchlines(art,NGS_LINE);
			if (h = instr(s,"net.general")) {
			    off = h-s;
			    strncpy(scrbuf,s,off+4);
			    strcpy(scrbuf+off+4,"followup");
			    safecpy(scrbuf+off+12,h+11,sizeof(scrbuf));
			    s = scrbuf;
			}
		    }
		    break;
		case 'h':			/* header file name */
		    s = headname;
		    break;
		case 'H':			/* host name */
		    s = sitename;
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
#ifdef NEWSADMIN
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
#ifdef DELAYMARK
		    sprintf(scrbuf,"%ld",(long)dmcount);
		    s = scrbuf;
#else
		    s = nullstr;
#endif
		    break;
		case 'n':			/* newsgroups */
		    s = ngs_buf = fetchlines(art,NGS_LINE);
		    break;
		case 'N':			/* full name */
		    s = getval("NAME",realname);
		    break;
		case 'o':			/* organization */
		    s = getval("ORGANIZATION",orgname);
#ifdef ORGFILE
		    if (*s == '/') {
			FILE *ofp = fopen(s,"r");

			if (ofp) {
			    fgets(scrbuf,sizeof scrbuf,ofp);
			    fclose(ofp);
			    s = scrbuf;
			    s[strlen(s)-1] = '\0';
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
#ifdef ASYNC_PARSE
		    parse_maybe(art);
#endif
		    if (htype[REFS_LINE].ht_minpos >= 0) {
			refs_buf = fetchlines(art,REFS_LINE);
			refscpy(scrbuf,(sizeof scrbuf),refs_buf);
		    }
		    else
			*scrbuf = '\0';
		    s = rindex(scrbuf,'<');
		    break;
		case 'R':
#ifdef ASYNC_PARSE
		    parse_maybe(art);
#endif
		    if (htype[REFS_LINE].ht_minpos >= 0) {
			refs_buf = fetchlines(art,REFS_LINE);
			refscpy(scrbuf,(sizeof scrbuf),refs_buf);
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
			s = subj_buf = fetchsubj(art,TRUE,TRUE);
						/* get subject handy */
		    while ((*s=='R'||*s=='r')&&(s[1]=='E'||s[1]=='e')&&s[2]==':') {
						/* skip extra Re: */
			s += 3;
			if (*s == ' ')
			    s++;
		    }
		    if (h = instr(s,"- (nf"))
			*h = '\0';
		    break;
		case 'S':
		    if (!(s=subj_buf))
			s = subj_buf = fetchsubj(art,TRUE,TRUE);
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
#ifdef ASYNC_PARSE
		    parse_maybe(art);
#endif
		    if (htype[REPLY_LINE].ht_minpos >= 0) {
					/* was there a reply line? */
			if (!(s=reply_buf))
			    s = reply_buf = fetchlines(art,REPLY_LINE);
		    }
		    else if (!(s = from_buf))
			s = from_buf = fetchlines(art,FROM_LINE);
		    if (*pattern == 'T') {
			if (htype[PATH_LINE].ht_minpos >= 0) {
					/* should we substitute path? */
			    s = path_buf = fetchlines(art,PATH_LINE);
			}
			i = strlen(sitename);
			if (strnEQ(sitename,s,i) && s[i] == '!')
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
		case 'U':
		    sprintf(scrbuf,"%ld",
			(long)(((ART_NUM)toread[ng]) - 1 + was_read(art)));
		    s = scrbuf;
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
	    i = metabit;		/* maybe get into register */
	    if (s == dest) {
		while (*dest) {
		    if (--destsize <= 0)
			abort_interp();
		    *dest++ |= i;
		}
	    }
	    else {
		while (*s) {
		    if (--destsize <= 0)
			abort_interp();
		    *dest++ = *s++ | i;
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
		    *dest++ = i & 037 | metabit;
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
		    *dest++ = i & 0377 | metabit;
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
#ifdef DEBUGGING
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
	while (*src == ' ' || *src == ',') src++;
	if (*src && --destsize > 0)
	    *dest++ = ' ';
    }
    *dest = '\0';
} 

/* get the person's real name from /etc/passwd */
/* (string is overwritten, so it must be copied) */

char *
getrealname(uid)
int uid;
{
    char *s, *c;

#ifdef PASSNAMES
#ifdef GETPWENT
    struct passwd *pwd = getpwuid(uid);
    
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
    strcpy(buf,tmpbuf);
#endif
#ifdef GETPWENT
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
