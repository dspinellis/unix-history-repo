/* $Header: /usr/src/games/warp/RCS/intrp.c,v 1.2 87/07/03 00:56:37 games Exp $
 *
 * Revision 7.0.1.2  86/12/12  16:59:04  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.1  86/10/16  10:51:43  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:12:19  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "sig.h"
#include "util.h"
#include "term.h"
#include "INTERN.h"
#include "intrp.h"

/* name of this host */
    char *hostname;

#ifdef TILDENAME
static char *tildename = Nullch;
static char *tildedir = Nullch;
#endif

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
    
    /* get the real name of the person (%N) */
    /* Must be done after logname is read in because BERKNAMES uses that */

    strcpy(tcbuf,getrealname(getuid()));
    realname = savestr(tcbuf);

    /* name of this host (%H) */

    gethostname(buf,sizeof buf);
    hostname = savestr(buf);
    if (index(hostname,'.'))
	hostname = savestr(hostname);
    else {
	char hname[128];

	strcpy(hname,hostname);
	strcat(hname,MYDOMAIN);
	hostname=savestr(hname);
    }
    warplib = savestr(filexp(WARPLIB));

    if (scorespec)			/* that getwd below takes ~1/3 sec. */
	return;				/* and we do not need it for -s */
    (void) getwd(tcbuf);		/* find working directory name */
    origdir = savestr(tcbuf);		/* and remember it */
}

/* expand filename via %, ~, and $ interpretation */
/* returns pointer to static area */
/* Note that there is a 1-deep cache of ~name interpretation */

char *
filexp(s)
Reg1 char *s;
{
    static char filename[CBUFLEN];
    char scrbuf[CBUFLEN];
    Reg2 char *d;

#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	printf("< %s\r\n",s);
#endif
    interp(filename, (sizeof filename), s);			/* interpret any % escapes */
#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	printf("%% %s\r\n",filename);
#endif
    s = filename;
    if (*s == '~') {	/* does destination start with ~? */
	if (!*(++s) || *s == '/') {
	    Sprintf(scrbuf,"%s%s",homedir,s);
				/* swap $HOME for it */
#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	printf("~ %s\r\n",scrbuf);
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
		    printf("r %s %s\r\n",tildename,tildedir);
#endif
	    }
	    else {
		if (tildename) {
		    free(tildename);
		    free(tildedir);
		}
		tildedir = Nullch;
		tildename = savestr(scrbuf);
		{
		    struct passwd *getpwnam();
		    struct passwd *pwd = getpwnam(tildename);

		    Sprintf(scrbuf,"%s%s",pwd->pw_dir,s);
		    tildedir = savestr(pwd->pw_dir);
		    strcpy(filename,scrbuf);
		    endpwent();
		}
	    }
#else /* !TILDENAME */
#ifdef VERBOSE
	    IF(verbose)
		fputs("~loginname not implemented.\r\n",stdout);
	    ELSE
#endif
#ifdef TERSE
		fputs("~login not impl.\r\n",stdout);
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
	    printf("$ %s\r\n",scrbuf);
#endif
	interp(filename, (sizeof filename), scrbuf);
					/* this might do some extra '%'s but */
					/* that is how the Mercedes Benz */
    }
#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	printf("> %s\r\n",filename);
#endif
    return filename;
}

#ifdef CONDSUB
/* skip interpolations */

char *
skipinterp(pattern,stoppers)
Reg1 char *pattern;
char *stoppers;
{

    while (*pattern && (!stoppers || !index(stoppers,*pattern))) {
#ifdef DEBUGGING
	if (debug & 8)
	    printf("skipinterp till %s at %s\r\n",stoppers?stoppers:"",pattern);
#endif
	if (*pattern == '%' && pattern[1]) {
	    switch (*++pattern) {
	    case '{':
		for (pattern++; *pattern && *pattern != '}'; pattern++)
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
Reg1 char *dest;
Reg2 int destsize;
Reg3 char *pattern;
char *stoppers;
{
    Reg4 char *s;
    Reg5 int i;
    char scrbuf[512];
    bool upper = FALSE;
    bool lastcomp = FALSE;
    int metabit = 0;

    while (*pattern && (!stoppers || !index(stoppers,*pattern))) {
#ifdef DEBUGGING
	if (debug & 8)
	    printf("dointerp till %s at %s\r\n",stoppers?stoppers:"",pattern);
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
		case '{':
		    pattern = cpytill(scrbuf,pattern+1,'}');
		    if (s = index(scrbuf,'-'))
			*s++ = '\0';
		    else
			s = nullstr;
		    s = getval(scrbuf,s);
		    break;
#ifdef CONDSUB
		case '(': {
		    char rch;
		    bool matched;
		    
		    pattern = dointerp(dest,destsize,pattern+1,"!=");
		    rch = *pattern;
		    if (rch == '!')
			pattern++;
		    if (*pattern != '=')
			goto getout;
		    pattern = cpytill(scrbuf,pattern+1,'?');
		    if (!*pattern)
			goto getout;
		    if (*scrbuf == '^' && scrbuf[strlen(scrbuf)-1] == '$') {
			scrbuf[strlen(scrbuf)-1] = '\0';
			matched = strEQ(scrbuf+1,dest);
		    }
		    else
			matched = instr(dest,scrbuf) != Nullch;
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
			printf("\r\nCan't run %s\r\n",scrbuf);
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
		    fputs(scrbuf,stdout);
		    resetty();
		    gets(scrbuf);
		    crmode();
		    raw();
		    noecho();
		    nonl();
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
		    Sprintf(s,"%d",getpid());
		    break;
		case 'H':			/* host name */
		    s = hostname;
		    break;
		case 'L':			/* login id */
		    s = logname;
		    break;
		case 'N':			/* full name */
		    s = getval("NAME",realname);
		    break;
		case 'O':
		    s = origdir;
		    break;
		case 'p':
		    s = cwd;
		    break;
		case 'X':			/* warp library */
		    s = warplib;
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
		    Safecpy(scrbuf,s,(sizeof scrbuf));
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
    return pattern;			/* where we left off */
}

void
interp(dest,destsize,pattern)
char *dest;
int destsize;
char *pattern;
{
    (void) dointerp(dest,destsize,pattern,Nullch);
#ifdef DEBUGGING
    if (debug & DEB_FILEXP)
	fputs(dest,stdout);
#endif
}

/* get the person's real name from /etc/passwd */
/* (string is overwritten, so it must be copied) */

char *
getrealname(uid)
int uid;
{
    char *s, *c;

#ifdef PASSNAMES
    struct passwd *pwd = getpwuid(uid);
    
    s = pwd->pw_gecos;
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
    endpwent();
    return buf;				/* return something static */
#else
    if ((tmpfp=fopen(filexp(FULLNAMEFILE),"r")) != Nullfp) {
	Fgets(buf,sizeof buf,tmpfp);
	Fclose(tmpfp);
    }
    else {
	resetty();
	printf("What is your name? ");
	Fgets(buf,(sizeof buf),stdin);
	crmode();
	raw();
	noecho();
	nonl();
	if (fork())
	    wait(0);
	else {
	    setuid(getuid());
	    if ((tmpfp = fopen(filexp(FULLNAMEFILE),"w")) == NULL)
		exit(1);
	    fprintf(tmpfp, "%s\n", buf);
	    Fclose(tmpfp);
	    exit(0);
	}
    }
    buf[strlen(buf)-1] = '\0';
    return buf;
#endif
}

static void
abort_interp()
{
    fputs("\r\n% interp buffer overflow!\r\n",stdout);
    sig_catcher(0);
}
