/* Copyright (c) 1979 Regents of the University of California */
/* be sure to include <stdio.h> before these defns */
/* whoami.h #defines VAX, CORY, A, etc. */
# include <whoami.h>

# ifdef A
# define CC
# define CCTTY
# define PASSWDF
# define FUID
# define SPACCT
# define LOCAL 'a'
# endif

# ifdef C
# define CC
# define CCTTY
# define PASSWDF
# define FUID
# define SPACCT
# define LOCAL 'c'
# endif

# ifdef D
# define CC
# define CCTTY
# define PASSWDF
# define FUID
# define SPACCT
# define LOCAL 'd'
# endif

# ifdef E
# define CC
# define CCTTY
# define PASSWDF
# define FUID
# define SPACCT
# define LOCAL 'e'
# endif

# ifdef SRC
# define CC
# define OLDTTY
# define PASSWDF
# define FUID
# define SPACCT
# define LOCAL 's'
# endif

# ifdef VAX
# define V7
# define ROPTION
# define LOCAL 'v'
# endif

# ifdef CORY
# define OLDTTY
# define PASSWDF
# define FUID
# define HPASSWD
# define ROPTION
# define LOCAL 'y'
# endif

# ifdef VANILLA
# define FUID
# endif

# ifdef V7
# include <sys/param.h>
# include <sys/stat.h>
# include <sys/dir.h>
# include <sys/times.h>
# include <signal.h>
# include <ctype.h>
# include <sgtty.h>
# include <errno.h>
# include <pwd.h>
# include <assert.h>
# include <setjmp.h>
# define getsize(S)	((S)->st_size)
# define gettime()	(time(0))

# else

# define ETXTBSY 26
# define S_IREAD 0400
# define S_IFMT 060000
# define S_IFDIR 0040000
# define ANYP 0300
# define ECHO 010
# define ROOTINO 1
# define DIRSIZ 14
# define TIOCEXCL 0
# define SIGHUP		1
# define SIGINT 	2
# define SIGQUIT 	3
# define SIGKILL	9
# define SIGCLK 	14
# define SIGTERM 	15
# define SIG_IGN	1
# define ASSERT "Assertion failed: file %s, line %d\n", __FILE__, __LINE__);exit(1);}}
# define assert(ex) {if (!(ex)){fprintf(stderr,ASSERT
# define isprint(c) (040 <= c && c <= 0176)
# define longjmp(a,b) reset()
# define setjmp(a) setexit()

struct stat {
	int st_dev;
	int st_ino;
	int st_mode;
	char st_nlink:8;
	char st_uid:8;
	char st_gid:8;
	char st_size0;
	int st_size1;
	int st_addr[8];
	long st_atime;
	long st_mtime;
	};
struct direct {
	int d_ino;
	char d_name[DIRSIZ];
	};
struct tms {				/* see times - sect 2 */
	int	tms_utime;		/* user time */
	int	tms_stime;		/* system time */
	long	tms_cutime;		/* user time, children */
	long	tms_cstime;		/* system time, children */
	};
struct sgttyb {
	char sg_ispeed;
	char sg_ospeed;
	char sg_erase;
	char sg_kill;
	int sg_flags;
	};
struct	passwd { /* see getpwent(3) */
	char	*pw_name;
	char	*pw_passwd;
	int	pw_uid;
	int	pw_gid;
	int	pw_quota;
	char	*pw_comment;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
	};
typedef int jmp_buf[10];
long gettime(), getsize();
# endif
/* end of non-v7 defns */

# ifdef FUID
# define getgid(s) (0)
# endif


# ifdef CCTTY
# define ttyname(S) myttyname(S)
# endif

# ifdef OLDTTY
struct utmp {
	char ut_name[8];
	char ut_tty;
	char ut_fill;
	long ut_time;
	int  ut_fill1;
	};
# else
# include <utmp.h>
# endif

/* mail programs stuff, network acct uid info */

# ifdef A
# define NUID ((217 << 8) | 2)
# endif

# ifdef C
# define NUID ((217 << 8) | 2)
# endif

# ifdef D
# define NUID ((217 << 8) | 2)
# endif

# ifdef E
# define NUID ((217 << 8) | 2)
# endif

# ifdef SRC
# define NUID ((217 << 8) | 2)
# endif

# ifdef VAX
# define NUID (501)
# endif

# ifdef CORY
# define NUID ((128 << 8) | 1)
# endif

/* functions */

char *hgethome(), *calloc(), *ctime(), *getenv();
char *getname(), *getun(), *getlogin();
struct passwd *getpwnam(), *getpwuid();
long fixuplong(),atol();
