/* $Header: util.c,v 4.3.1.2 85/05/15 14:44:27 lwall Exp $
 *
 * $Log:	util.c,v $
 * Revision 4.3.1.2  85/05/15  14:44:27  lwall
 * Last arg of execl changed from 0 to Nullch [(char*)0].
 * 
 * Revision 4.3.1.1  85/05/10  11:41:30  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:51:44  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "final.h"
#include "ndir.h"
#include "INTERN.h"
#include "util.h"

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
    register int (*istat)(), (*qstat)();
    int (*signal())();
    char *shell;

#ifdef SIGTSTP
    sigset(SIGTSTP,SIG_DFL);
    sigset(SIGCONT,SIG_DFL);
#endif
    if (shl != Nullch)
	shell = shl;
    else if ((shell = getenv("SHELL")) == Nullch || !*shell)
	shell = PREFSHELL;
    if ((pid = vfork()) == 0) {
	if (*s)
	    execl(shell, shell, "-c", s, Nullch);
	else
	    execl(shell, shell, Nullch, Nullch, Nullch);
	_exit(127);
    }
#ifndef lint
    istat = signal(SIGINT, SIG_IGN);
    qstat = signal(SIGQUIT, SIG_IGN);
#else
    istat = Null(int (*)());
    qstat = Null(int (*)());
#endif lint
    waiting = TRUE;
    while ((w = wait(&status)) != pid && w != -1)
	;
    if (w == -1)
	status = -1;
    waiting = FALSE;
    signal(SIGINT, istat);
    signal(SIGQUIT, qstat);
#ifdef SIGTSTP
    sigset(SIGTSTP,stop_catcher);
    sigset(SIGCONT,cont_catcher);
#endif
    return status;
}

static char nomem[] = "rn: out of memory!\n";

/* paranoid version of malloc */

char *
safemalloc(size)
MEM_SIZE size;
{
    char *ptr;
    char *malloc();

    ptr = malloc(size?size:1);	/* malloc(0) is NASTY on our system */
    if (ptr != Nullch)
	return ptr;
    else {
	fputs(nomem,stdout) FLUSH;
	sig_catcher(0);
    }
    /*NOTREACHED*/
}

/* paranoid version of realloc */

char *
saferealloc(where,size)
char *where;
MEM_SIZE size;
{
    char *ptr;
    char *realloc();

    ptr = realloc(where,size?size:1);	/* realloc(0) is NASTY on our system */
    if (ptr != Nullch)
	return ptr;
    else {
	fputs(nomem,stdout) FLUSH;
	sig_catcher(0);
    }
    /*NOTREACHED*/
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
instr(big, little)
char *big, *little;

{
    register char *t, *s, *x;

    for (t = big; *t; t++) {
	for (x=t,s=little; *s; x++,s++) {
	    if (!*x)
		return Nullch;
	    if (*s != *x)
		break;
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

#ifdef GETWD
#define	dot	"."
#define	dotdot	".."

static	char	*name;

static	DIR	*dirp;
static	int	off;
static	struct	stat	d, dd;
static	struct	direct	*dir;

char *
getwd(np)
char *np;
{
	long rdev, rino;

	*np++ = '/';
	*np = 0;
	name = np;
	off = -1;
	stat("/", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;) {
		stat(dot, &d);
		if (d.st_ino==rino && d.st_dev==rdev)
			goto done;
		if ((dirp = opendir(dotdot)) == Null(DIR *))
			prexit("getwd: cannot open ..\n");
		stat(dotdot, &dd);
		chdir(dotdot);
		if(d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino)
				goto done;
			do
				if ((dir = readdir(dirp)) == Null(struct direct *))
					prexit("getwd: read error in ..\n");
			while (dir->d_ino != d.st_ino);
		}
		else do {
				if ((dir = readdir(dirp)) == Null(struct direct *))
					prexit("getwd: read error in ..\n");
				stat(dir->d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		cat();
		closedir(dirp);
	}
done:
	name--;
	if (chdir(name) < 0) {
		printf("getwd: can't cd back to %s\n",name) FLUSH;
		sig_catcher(0);
	}
	return (name);
}

void
cat()
{
	register i, j;

	i = -1;
	while (dir->d_name[++i] != 0);
	if ((off+i+2) > 1024-1)
		return;
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	if (off >= 0)
		name[i] = '/';
	off=i+off+1;
	name[off] = 0;
	for(--i; i>=0; --i)
		name[i] = dir->d_name[i];
}

void
prexit(cp)
char *cp;
{
	write(2, cp, strlen(cp));
	sig_catcher(0);
}
#else
char *
getwd(np)			/* shorter but slower */
char *np;
{
    FILE *popen();
    FILE *pipefp = popen("/bin/pwd","r");

    if (pipefd == Nullfp) {
	printf("Can't run /bin/pwd\n") FLUSH;
	finalize(1);
    }
    fgets(np,512,pipefp);
    np[strlen(np)-1] = '\0';	/* wipe out newline */
    pclose(pipefp);
    return np;
}
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
	if (stat(dirname,&filestat) >= 0) {
					/* does this much exist? */
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
#endif lint
    
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
#endif lint
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
    }
}
