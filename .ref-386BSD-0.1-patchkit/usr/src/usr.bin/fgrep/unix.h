/* unix.h - prototype UNIX system calls and a few library routines.
   In the Public Domain; written by Mike Haertel. */

#define size_t unix_size_t
#include <sys/types.h>
#undef size_t
#include <sys/stat.h>
#include <sys/dir.h>

/* Optionally get the directory routines.  The Makefile needs to
   define which version. */
#ifdef DIRENT
#include <dirent.h>
#else
#ifdef SYSDIR
#include <sys/dir.h>
#else
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* SYSDIR */
#endif /* DIRENT */

/* Prototype certain UNIX system calls. */
extern int EXFUN(access, (const char *, int));
extern unsigned int EXFUN(alarm, (unsigned int));
extern PTR EXFUN(sbrk, (size_t));
extern int EXFUN(chdir, (const char *));
/* extern int EXFUN(chmod, (const char *, int)); /* */
extern int EXFUN(chown, (const char *, int, int));
extern int EXFUN(close, (int));
extern int EXFUN(creat, (const char *, int));
extern int EXFUN(dup, (int));
extern int EXFUN(execl, (const char *, ...));
extern int EXFUN(execv, (const char *, char **));
extern int EXFUN(execle, (const char *, ...));
extern int EXFUN(execve, (const char *, char **, char **));
extern int EXFUN(execlp, (const char *, ...));
extern int EXFUN(execvp, (const char *, char **));
extern void EXFUN(_exit, (int));
extern int EXFUN(fcntl, (int, int, int));
extern int EXFUN(fork, (void));
extern int EXFUN(getpid, (void));
extern int EXFUN(getppid, (void));
extern int EXFUN(ioctl, (int, int, PTR));
extern int EXFUN(kill, (int, int));
extern int EXFUN(link, (const char *, const char *));
extern long int EXFUN(lseek, (int, long int, int));
extern int EXFUN(open, (const char *, int, ...));
extern void EXFUN(pause, (void));
extern int EXFUN(pipe, (int *));
extern int EXFUN(read, (int, char *, size_t));
extern int EXFUN(stat, (const char *, struct stat *));
extern int EXFUN(fstat, (int, struct stat *));
/* extern int EXFUN(umask, (int)); /* */
extern int EXFUN(unlink, (const char *));
extern int EXFUN(wait, (int *));
extern int EXFUN(write, (int, const char *, size_t));

/* Prototype certain UNIX library routines. */
extern char *EXFUN(crypt, (const char *, const char *));
extern int EXFUN(getopt, (int, char **, const char *));
extern char EXFUN(getpass, (const char *));
extern unsigned int EXFUN(sleep, (unsigned int));
extern int EXFUN(isatty, (int));
extern const char *EXFUN(ttyname, (int));
extern int EXFUN(tgetent, (char *, const char *));
extern int EXFUN(tgetnum, (const char *));
extern int EXFUN(tgetflag, (const char *));
extern char *EXFUN(tgetstr, (const char *, char **));
extern char *EXFUN(tgoto, (const char *, int, int));
extern int EXFUN(tputs, (const char *, int, int EXFUN((*), (int))));
extern char *EXFUN(tparam, (const char *, char *, size_t, ...));

/* Declare a few UNIX-specific external objects. */
extern char PC, *BC, *UP;
extern short int ospeed;
extern char *optarg;
extern int optind;
