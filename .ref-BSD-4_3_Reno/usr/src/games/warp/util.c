/* $Header: util.c,v 7.0.1.2 86/10/20 12:07:46 lwall Exp $ */

/* $Log:	util.c,v $
 * Revision 7.0.1.2  86/10/20  12:07:46  lwall
 * Made all exits reset tty.
 * 
 * Revision 7.0.1.1  86/10/16  10:54:02  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:14:31  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "sys/dir.h"
#include "object.h"
#include "sig.h"
#include "term.h"
#include "INTERN.h"
#include "util.h"

void
util_init()
{
    ;
}

void
movc3(len,src,dest)
#ifdef vax
char *dest, *src;
int len;
{
    asm("movc3 4(ap),*8(ap),*12(ap)");
}
#else
Reg1 char *dest;
Reg2 char *src;
Reg3 int len;
{
    if (dest <= src) {
	for (; len; len--) {
	    *dest++ = *src++;
	}
    }
    else {
	dest += len;
	src += len;
	for (; len; len--) {
	    *--dest = *--src;
	}
    }
}
#endif

void
no_can_do(what)
char *what;
{
    fprintf(stderr,"Sorry, your terminal is too %s to play warp.\r\n",what);
    finalize(1);
}

int
exdis(maxnum)
int maxnum;
{
    double temp, temp2;
    double exp();
    double log();

    temp = (double) maxnum;
#ifndef lint
    temp2 = (double) myrand();
#else
    temp2 = 0.0;
#endif
#if RANDBITS == 15
    return (int) exp(temp2 * log(temp)/0x7fff);
#else
#if RANDBITS == 16
    return (int) exp(temp2 * log(temp)/0xffff);
#else
    return (int) exp(temp2 * log(temp)/0x7fffffff);
#endif
#endif
}

static char nomem[] = "warp: out of memory!\r\n";

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
	fputs(nomem,stdout);
	sig_catcher(0);
    }
    /*NOTREACHED*/
}

/* safe version of string copy */

char *
safecpy(to,from,len)
char *to;
Reg2 char *from;
Reg1 int len;
{
    Reg3 char *dest = to;

    if (from != Nullch) 
	for (len--; len && (*dest++ = *from++); len--) ;
    *dest = '\0';
    return to;
}

/* copy a string up to some (non-backslashed) delimiter, if any */

char *
cpytill(to,from,delim)
Reg2 char *to;
Reg1 char *from;
Reg3 int delim;
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
    Reg3 char *t;
    Reg1 char *s;
    Reg2 char *x;

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
			prexit("getwd: cannot open ..\r\n");
		stat(dotdot, &dd);
		chdir(dotdot);
		if(d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino)
				goto done;
			do
				if ((dir = readdir(dirp)) == Null(struct direct *))
					prexit("getwd: read error in ..\r\n");
			while (dir->d_ino != d.st_ino);
		}
		else do {
				if ((dir = readdir(dirp)) == Null(struct direct *))
					prexit("getwd: read error in ..\r\n");
				stat(dir->d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		cat();
		closedir(dirp);
	}
done:
	name--;
	if (chdir(name) < 0) {
		printf("getwd: can't cd back to %s\r\n",name);
		sig_catcher(0);
	}
	return (name);
}

void
cat()
{
	Reg1 int i;
	Reg2 int j;

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

    if (pipefp == Nullfp) {
	printf("Can't run /bin/pwd\r\n");
	finalize(1);
    }
    Fgets(np,512,pipefp);
    np[strlen(np)-1] = '\0';	/* wipe out newline */
    pclose(pipefp);
    return np;
}
#endif

/* copy a string to a safe spot */

char *
savestr(str)
char *str;
{
    Reg1 char *newaddr = safemalloc((MEM_SIZE)(strlen(str)+1));

    strcpy(newaddr,str);
    return newaddr;
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
