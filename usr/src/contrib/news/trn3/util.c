/* $Id: util.c,v 3.0 1992/02/23 21:25:39 davison Trn $
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
#include "final.h"
#include "term.h"
#include "INTERN.h"
#include "util.h"

#ifndef HAS_STRFTIME
#include "sys/time.h"
#endif

void
util_init()
{
    ;
}
    
/* fork and exec a shell command */

int
doshell(shl,s)
char *s, *shl;
{
    int status, pid, w;
    char *shell;

#ifdef SIGTSTP
    sigset(SIGTSTP,SIG_DFL);
    sigset(SIGTTOU,SIG_DFL);
    sigset(SIGTTIN,SIG_DFL);
#endif
    if (shl != Nullch)
	shell = shl;
    else if ((shell = getenv("SHELL")) == Nullch || !*shell)
	shell = PREFSHELL;
    if ((pid = vfork()) == 0) {
#ifdef USE_NNTP
        int i;

	/* This is necessary to keep bourne shell from puking */

        for (i = 3; i < 10; ++i)
                close(i);
#endif /* USE_NNTP */

	if (*s)
	    execl(shell, shell, "-c", s, Nullch);
	else
	    execl(shell, shell, Nullch, Nullch, Nullch);
	_exit(127);
    }
    signal(SIGINT, SIG_IGN);
#ifdef SIGQUIT
    signal(SIGQUIT, SIG_IGN);
#endif 
    termlib_reset();
    waiting = TRUE;
    while ((w = wait(&status)) != pid)
	if (w == -1 && errno != EINTR)
	    break;
    if (w == -1)
	status = -1;
    termlib_init();
    waiting = FALSE;
    sigset(SIGINT, int_catcher);	/* always catch interrupts */
#ifdef SIGQUIT
    signal(SIGQUIT, SIG_DFL);
#endif 
#ifdef SIGTSTP
    sigset(SIGTSTP,stop_catcher);
    sigset(SIGTTOU,stop_catcher);
    sigset(SIGTTIN,stop_catcher);
#endif
    return status;
}

static char nomem[] = "trn: out of memory!\n";

/* paranoid version of malloc */

char *
safemalloc(size)
MEM_SIZE size;
{
    char *ptr;

    ptr = malloc(size ? size : (MEM_SIZE)1);
    if (ptr == Nullch) {
	fputs(nomem,stdout) FLUSH;
	sig_catcher(0);
    }
    return ptr;
}

/* paranoid version of realloc.  If where is NULL, call malloc */

char *
saferealloc(where,size)
char *where;
MEM_SIZE size;
{
    char *ptr;

    if (!where)
	ptr = malloc(size ? size : (MEM_SIZE)1);
    else
	ptr = realloc(where, size ? size : (MEM_SIZE)1);
    if (!ptr) {
	fputs(nomem,stdout) FLUSH;
	sig_catcher(0);
    }
    return ptr;
}

/* safe version of string copy */

char *
safecpy(to,from,len)
char *to;
register char *from;
register int len;
{
    register char *dest = to;

    if (from != Nullch) 
	for (len--; len && (*dest++ = *from++); len--) ;
    *dest = '\0';
    return to;
}

/* safe version of string concatenate, with \n deletion and space padding */

char *
safecat(to,from,len)
char *to;
register char *from;
register int len;
{
    register char *dest = to;

    len--;				/* leave room for null */
    if (*dest) {
	while (len && *dest++) len--;
	if (len) {
	    len--;
	    *(dest-1) = ' ';
	}
    }
    if (from != Nullch)
	while (len && (*dest++ = *from++)) len--;
    if (len)
	dest--;
    if (*(dest-1) == '\n')
	dest--;
    *dest = '\0';
    return to;
}

/* copy a string up to some (non-backslashed) delimiter, if any */

char *
cpytill(to,from,delim)
register char *to, *from;
register int delim;
{
    for (; *from; from++,to++) {
	if (*from == '\\' && from[1] == delim)
	    from++;
	else if (*from == delim)
	    break;
	*to = *from;
    }
    *to = '\0';
    return from;
}

/* return ptr to little string in big string, NULL if not found */

char *
instr(big, little, case_matters)
char *big, *little;
bool_int case_matters;
{
    register char *t, *s, *x;

    for (t = big; *t; t++) {
	for (x=t,s=little; *s; x++,s++) {
	    if (!*x)
		return Nullch;
	    if (case_matters == TRUE) {
		if(*s != *x)
		    break;
	    } else {
		register char c,d;
		if (isupper(*s)) 
		    c = tolower(*s);
		else
		    c = *s;
		if (isupper(*x)) 
		    d = tolower(*x);
		else
		    d = *x;
		if ( c != d )
		    break;
	   }
	}
	if (!*s)
	    return t;
    }
    return Nullch;
}

/* effective access */

#ifdef SETUIDGID
int
eaccess(filename, mod)
char *filename;
int mod;
{
    int protection, euid;
    
    mod &= 7;				/* remove extraneous garbage */
    if (stat(filename, &filestat) < 0)
	return -1;
    euid = geteuid();
    if (euid == ROOTID)
	return 0;
    protection = 7 & (filestat.st_mode >>
      (filestat.st_uid == euid ? 6 :
        (filestat.st_gid == getegid() ? 3 : 0)
      ));
    if ((mod & protection) == mod)
	return 0;
    errno = EACCES;
    return -1;
}
#endif

/*
 * Get working directory
 */
#ifndef HAS_GETWD
#ifdef HAS_GETCWD
char *
getwd(np)
char *np;
{
    extern char *getcwd();
    return getcwd(np,512);
}
#else
char *
getwd(np)
char *np;
{
    FILE *popen();
    FILE *pipefp = popen("/bin/pwd","r");

    if (pipefp == Nullfp) {
	printf("Can't run /bin/pwd\n") FLUSH;
	finalize(1);
    }
    fgets(np,512,pipefp);
    np[strlen(np)-1] = '\0';	/* wipe out newline */
    pclose(pipefp);
    return np;
}
#endif
#endif

/* just like fgets but will make bigger buffer as necessary */

char *
get_a_line(original_buffer,buffer_length,fp)
char *original_buffer;
register int buffer_length;
FILE *fp;
{
    register int bufix = 0;
    register int nextch;
    register char *some_buffer_or_other = original_buffer;

    do {
	if (bufix >= buffer_length) {
	    buffer_length *= 2;
	    if (some_buffer_or_other == original_buffer) {
					/* currently static? */
		some_buffer_or_other = safemalloc((MEM_SIZE)buffer_length+1);
		strncpy(some_buffer_or_other,original_buffer,buffer_length/2);
					/* so we must copy it */
	    }
	    else {			/* just grow in place, if possible */
		some_buffer_or_other = saferealloc(some_buffer_or_other,
		    (MEM_SIZE)buffer_length+1);
	    }
	}
	if ((nextch = getc(fp)) == EOF)
	    return Nullch;
	some_buffer_or_other[bufix++] = (char) nextch;
    } while (nextch && nextch != '\n');
    some_buffer_or_other[bufix] = '\0';
    len_last_line_got = bufix;
    buflen_last_line_got = buffer_length;
    return some_buffer_or_other;
}

/* copy a string to a safe spot */

char *
savestr(str)
char *str;
{
    register char *newaddr = safemalloc((MEM_SIZE)(strlen(str)+1));

    strcpy(newaddr,str);
    return newaddr;
}

int
makedir(dirname,nametype)
register char *dirname;
int nametype;
{
#ifdef MAKEDIR
    register char *end;
    register char *s;
    char tmpbuf[1024];
    register char *tbptr = tmpbuf+5;

    for (end = dirname; *end; end++) ;	/* find the end */
    if (nametype == MD_FILE) {		/* not to create last component? */
	for (--end; end != dirname && *end != '/'; --end) ;
	if (*end != '/')
	    return 0;			/* nothing to make */
	*end = '\0';			/* isolate file name */
    }
    strcpy(tmpbuf,"mkdir");

    s = end;
    for (;;) {
	if (stat(dirname,&filestat) >= 0 && (filestat.st_mode & S_IFDIR)) {
					/* does this much exist as a dir? */
	    *s = '/';			/* mark this as existing */
	    break;
	}
	s = rindex(dirname,'/');	/* shorten name */
	if (!s)				/* relative path! */
	    break;			/* hope they know what they are doing */
	*s = '\0';			/* mark as not existing */
    }
    
    for (s=dirname; s <= end; s++) {	/* this is grody but efficient */
	if (!*s) {			/* something to make? */
	    sprintf(tbptr," %s",dirname);
	    tbptr += strlen(tbptr);	/* make it, sort of */
	    *s = '/';			/* mark it made */
	}
    }
    if (nametype == MD_DIR)		/* don't need final slash unless */
	*end = '\0';			/*  a filename follows the dir name */

    return (tbptr==tmpbuf+5 ? 0 : doshell(sh,tmpbuf));
					/* exercise our faith */
#else
    sprintf(cmd_buf,"%s %s %d", filexp(DIRMAKER), dirname, nametype);
    return doshell(sh,cmd_buf);
#endif
}

#ifdef SETENV
static bool firstsetenv = TRUE;
extern char **environ;

void
setenv(nam,val)
char *nam, *val;
{
    register int i=envix(nam);		/* where does it go? */

    if (!environ[i]) {			/* does not exist yet */
	if (firstsetenv) {		/* need we copy environment? */
	    int j;
#ifndef lint
	    char **tmpenv = (char**)	/* point our wand at memory */
		safemalloc((MEM_SIZE) (i+2) * sizeof(char*));
#else
	    char **tmpenv = Null(char **);
#endif /* lint */
    
	    firstsetenv = FALSE;
	    for (j=0; j<i; j++)		/* copy environment */
		tmpenv[j] = environ[j];
	    environ = tmpenv;		/* tell exec where it is now */
	}
#ifndef lint
	else
	    environ = (char**) saferealloc((char*) environ,
		(MEM_SIZE) (i+2) * sizeof(char*));
					/* just expand it a bit */
#endif /* lint */
	environ[i+1] = Nullch;	/* make sure it's null terminated */
    }
    environ[i] = safemalloc((MEM_SIZE) strlen(nam) + strlen(val) + 2);
					/* this may or may not be in */
					/* the old environ structure */
    sprintf(environ[i],"%s=%s",nam,val);/* all that work just for this */
}

int
envix(nam)
char *nam;
{
    register int i, len = strlen(nam);

    for (i = 0; environ[i]; i++) {
	if (strnEQ(environ[i],nam,len) && environ[i][len] == '=')
	    break;			/* strnEQ must come first to avoid */
    }					/* potential SEGV's */
    return i;
}
#endif

void
notincl(feature)
char *feature;
{
    printf("\nNo room for feature \"%s\" on this machine.\n",feature) FLUSH;
}

char *
getval(nam,def)
char *nam,*def;
{
    char *val;

    if ((val = getenv(nam)) == Nullch || !*val)
	val = def;
    return val;
}

/* grow a static string to at least a certain length */

void
growstr(strptr,curlen,newlen)
char **strptr;
int *curlen;
int newlen;
{
    if (newlen > *curlen) {		/* need more room? */
	if (*curlen)
	    *strptr = saferealloc(*strptr,(MEM_SIZE)newlen);
	else
	    *strptr = safemalloc((MEM_SIZE)newlen);
	*curlen = newlen;
    }
}

void
setdef(buffer,dflt)
char *buffer,*dflt;
{
#ifdef STRICTCR
    if (*buffer == ' ')
#else
    if (*buffer == ' ' || *buffer == '\n')
#endif
    {
	if (*dflt == '^' && isupper(dflt[1]))
	    *buffer = Ctl(dflt[1]);
	else
	    *buffer = *dflt;
	lastchar = *buffer;
    }
}

#if !defined(HAS_STRFTIME) && defined(HAS_FTIME) && !defined(TM_ZONE)
/*
 * strftime: print formatted information about a given time.
 * Adapted from the routine by Eric R. Smith, Michal Jaegermann,
 * Arnold Robins, and Paul Close.
 */

/* Configuration choices for %x and %X */

#undef LOCAL_DDMMYY	/* choose DD/MM/YY instead of MM/DD/YY */
#undef LOCAL_DOTTIME	/* choose HH.MM.SS instead of HH:MM:SS */

static char *mth_name[] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};

static char *day_name[] = {
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday"
};

#ifdef HAS_FTIME
# ifndef TM_ZONE
char tznm[16] = "";
# endif
#else
extern char *tzname[];
#endif

size_t
strftime(str, maxsize, fmt, ts)
char *str;
size_t maxsize;
CONST char *fmt;
CONST struct tm *ts;
{
    size_t num = 0, len;
    char ch;
    char *putstr, *s;
    char tmpbuf[80];

    if (maxsize-- <= 0)
	return 0;

    for (;;) {
	if (!(ch = *fmt++))
	    break;
	if (num == maxsize) {
	    num = 0;
	    break;
	}
	if (ch != '%') {
	    *str++ = ch;
	    num++;
	    continue;
	}
	/* assume the finished product will be sprintf'ed into tmpbuf */
	putstr = tmpbuf;

	switch (ch = *fmt++) {
	case 'A':
	case 'a':
	    if (ts->tm_wday < 0 || ts->tm_wday > 6)
		putstr = "?";
	    else
		if (ch == 'A')
		    putstr = day_name[ts->tm_wday];
		else
		    sprintf(tmpbuf, "%-.3s", day_name[ts->tm_wday]);
	    break;
	case 'B':
	case 'b':
	case 'h':
	    if (ts->tm_mon < 0 || ts->tm_mon > 11)
		putstr = "?";
	    else if (ch == 'B')
		putstr = mth_name[ts->tm_mon];
	    else
		sprintf(tmpbuf, "%-.3s", mth_name[ts->tm_mon]);
	    break;
	case 'C':
	    strftime(tmpbuf, sizeof tmpbuf, "%A, %B %e, %Y", ts);
	    break;
	case 'c':
	    strftime(tmpbuf, sizeof tmpbuf, "%x %X", ts);
	    break;
	case 'D':
#ifndef LOCAL_DDMMYY
	case 'x':
#endif
	    strftime(tmpbuf, sizeof tmpbuf, "%m/%d/%y", ts);
	    break;
	case 'd':
	    sprintf(tmpbuf, "%02d", ts->tm_mday);
	    break;
	case 'e':	/* day of month, blank padded */
	    sprintf(tmpbuf, "%2d", ts->tm_mday);
	    break;
	case 'H':
	    sprintf(tmpbuf, "%02d", ts->tm_hour);
	    break;
	case 'I':
	{
	    int n;

	    n = ts->tm_hour;
	    if (n == 0)
		n = 12;
	    else if (n > 12)
		n -= 12;
	    sprintf(tmpbuf, "%02d", n);
	    break;
	}
	case 'j':
	    sprintf(tmpbuf, "%03d", ts->tm_yday + 1);
	    break;
	case 'm':
	    sprintf(tmpbuf, "%02d", ts->tm_mon + 1);
	    break;
	case 'M':
	    sprintf(tmpbuf, "%02d", ts->tm_min);
	    break;
	case 'p':
	    putstr = (ts->tm_hour < 12) ? "AM" : "PM";
	    break;
	case 'r':
	    strftime(tmpbuf, sizeof tmpbuf, "%I:%M:%S %p", ts);
	    break;
	case 'R':
	    strftime(tmpbuf, sizeof tmpbuf, "%H:%M", ts);
	    break;
	case 'S':
	    sprintf(tmpbuf, "%02d", ts->tm_sec);
	    break;
	case 'T':
#ifndef LOCAL_DOTTIME
	case 'X':
#endif
	    strftime(tmpbuf, sizeof tmpbuf, "%H:%M:%S", ts);
	    break;
	case 'U':	/* week of year - starting Sunday */
	    sprintf(tmpbuf, "%02d", (ts->tm_yday - ts->tm_wday + 10) / 7);
	    break;
	case 'W':	/* week of year - starting Monday */
	    sprintf(tmpbuf, "%02d", (ts->tm_yday - ((ts->tm_wday + 6) % 7)
			+ 10) / 7);
	    break;
	case 'w':
	    sprintf(tmpbuf, "%d", ts->tm_wday);
	    break;
	case 'y':
	    sprintf(tmpbuf, "%02d", ts->tm_year % 100);
	    break;
#ifdef LOCAL_DOTTIME
	case 'X':
	    strftime(tmpbuf, sizeof tmpbuf, "%H.%M.%S", ts);
	    break;
#endif
#ifdef LOCAL_DDMMYY
	case 'x':
	    strftime(tmpbuf, sizeof tmpbuf, "%d/%m/%y", ts);
	    break;
#endif
	case 'Y':
	    sprintf(tmpbuf, "%d", ts->tm_year + 1900);
	    break;
	case 'Z':
#ifdef HAS_FTIME
# ifdef TM_ZONE
	    sprintf(tmpbuf, "%s", ts->tm_zone);
# else
	    if (*tznm == '\0') {
		char *timezone();
		struct timeval tv;
		struct timezone tz;

		(void) gettimeofday(&tv, &tz);
		strcpy(tznm, timezone(tz.tz_minuteswest, ts->tm_isdst));
	    }
	    sprintf(tmpbuf, "%s", tznm);
# endif
#else
	    sprintf(tmpbuf, "%s", tzname[ts->tm_isdst]);
#endif
	    break;
	case '%':
	case '\0':
	    putstr = "%";
	    break;
	case 'n':	/* same as \n */
	    putstr = "\n";
	    break;
	case 't':	/* same as \t */
	    putstr = "\t";
	    break;
	default:
	    sprintf(tmpbuf, "%%%c", ch);
	    break;
	}
	len = strlen(putstr);
	num += len;
	if (num > maxsize) {
	    len -= num - maxsize;
	    num = 0;
	    ch = '\0';
	}
	strncpy(str, putstr, len);
	str += len;
	if (!ch)
	    break;
    }
    *str = '\0';
    return num;
}
#endif /* !HAS_STRFTIME */
