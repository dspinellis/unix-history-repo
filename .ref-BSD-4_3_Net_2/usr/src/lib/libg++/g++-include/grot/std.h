// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/


#ifndef _std_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _std_h 1

#include <stddef.h> /* sorry, but needed for DGUX folks... */
#include <stdio.h> /* sorry, but needed for USG folks... */

extern "C" {
void volatile _exit(int);
void volatile abort(void);
int       abs(int);
int       access(const char*, int);
int       acct(const char*);
unsigned  alarm(unsigned);
double    atof(const char*);
int       atoi(const char*);
long      atol(const char*);
int       bind(int, void*, int);
void      bcopy(const void*, void*, int); // USG uses version in bcopy.c
int       brk(void*);
int       bsearch (const void *, const void *, unsigned long, 
                   unsigned long, auto int (*ptf)(const void*, const void*));
void*     calloc(unsigned, unsigned);
void      cfree(void*);
int       chdir(const char*);
int       chmod(const char*, int);
int       chown(const char*, int, int);
#ifdef hpux
unsigned
#endif
long      clock(void);
int       close(int);
#if defined(DGUX)
int       creat(char*, unsigned long int);
#elif defined(convex)
int       creat(char*, unsigned short int);
#elif defined(hpux) || defined(USG)
int	  creat(const char*, unsigned short int);
#else
int       creat(const char*, unsigned long int);
#endif
char*     crypt(const char*, const char*);
char*     ctermid(char*);
char*     cuserid(char*);
double    drand48(void);
int       dup(int);
int       dup2(int, int);
int       dysize(int);
char*     ecvt(double, int, int*, int*);
char*     encrypt(char*, int);
double    erand(short*);
int       execl(const char*, const char *, ...);
int       execle(const char*, const char *, ...);
int       execlp(const char*, const char*, ...);
int       exect(const char*,  const char**,  char**);
int       execv(const char*,  const char**);
int       execve(const char*, const char**, char**);
int       execvp(const char*,  const char**);
void volatile exit(int);
int       fchmod(int, int);
int       fchown(int, int, int);
int       fcntl(int, int, ...);
char*     fcvt(double, int, int*, int*);
int       ffs(int);
#ifndef hpux
int       flock(int, int);
#endif
int       fork(void);
void      free(void*);
int       fsync(int);
long      ftok(const char*, int);
int       ftruncate(int, unsigned long);
char*     gcvt(double, int, char*);
char*     getcwd(char*, int);
int       getdomainname(char*, int);
int       getdtablesize(void);
int       getegid(void);
char*     getenv(const char*);
int       geteuid(void);
int       getgid(void);
int       getgroups(int, int*);
long      gethostid(void);
int       gethostname(char*, int);
char*     getlogin(void);
int       getopt(int, const char**, const char*);
int       getpagesize(void);
char*     getpass(const char*);
int       getpgrp(...);
int       getpid(void);
int       getppid(void);
int       getpriority(int, int);
int       getpw(int, char*);
unsigned  getuid(void);
char*     getwd(char*);
char*     initstate(unsigned, char*, int);
int       ioctl(int, int, void*);
int       isatty(int);
long      jrand48(short*);
int       kill(int, int);
int       killpg(int, int);
void      lcong48(short*);
int       link(const char*, const char*);
int       listen(int, int);
int       lock(int, int, long);
long      lrand48(void);
long      lseek(int, long, int);
void*     malloc(unsigned);
unsigned  malloc_usable_size(void*);
void*     memalign(unsigned, unsigned);
void*     memccpy(void*, const void*, int, int);
void*     memchr(const void*, int, int);
int       memcmp(const void*, const void*, int);
void*     memcpy(void*, const void*, int);
void*     memset(void*, int, int);
int       mkdir(const char*, int);
int       mknod(const char*, int, int);
int       mkstemp(char*);
char*     mktemp(char*);
long      mrand48(void);
int       nice(int);
long      nrand48(short*);
#ifdef DGUX
int       open(char*, int, ...);
#else
int       open(const char*, int, ...);
#endif
void volatile pause(void);
void      perror(const char*);
int       pipe(int*);
int       profil(char*, int, int, int);
int       psignal(unsigned, char*);
int       ptrace(int, int, int, int);
int       putenv(const char*);
void      qsort(void*, unsigned int, unsigned, auto int (*ptf)(void*,void*));
int       rand(void);
long      random(void);
int       read(int, void*, unsigned);
int       readlink(const char*, char*, int);
void*     realloc(void*, unsigned);
int       rename(const char*, const char*);
int       rmdir(const char*);               
void*     sbrk(int);              
short*    seed48(short*);
int       send(int, char*, int, int);
int       setgid(int);
int       sethostname(char*, int);
int       setkey(const char*);
int       setpgrp(...);
int       setpriority(int, int, int);
int       setregid(int, int);
int       setreuid(int, int);
char*     setstate(char*);
int       setuid(int);
int       sigblock(int);
int       siginterrupt(int, int);
int       sigpause(int);
int       sigsetmask(int);
unsigned  sleep(unsigned);
int       socket(int, int, int);
int       srand(unsigned int);
void      srand48(long);
void      srandom(int);
int       stime(long*);
char*     strcat(char*, const char*);
char*     strchr(const char*, int);
int       strcmp(const char*, const char*);
char*     strcpy(char*, const char*);
int       strcspn(const char*, const char*);
char*     strdup(const char*);
int       strlen(const char*);
char*     strncat(char*, const char*, int);
int       strncmp(const char*, const char*, int);
char*     strncpy(char*, const char*, int);
char*     strpbrk(const char*, const char*);
char*     strrchr(const char*, int);
int       strspn(const char*, const char*);
double    strtod(const char*, char**);
char*     strtok(char*, const char*);
long      strtol(const char*, char**, int);
void      swab(void*, void*, int);
int       symlink(const char*, const char*);
int       syscall(int, ...);
int       system(const char*);
char*     tempnam(const char*, const char*);
int       tgetent(const char*, const char*);
int       tgetnum(const char*);
int       tgetflag(const char*);
char*     tgetstr(const char *, char **);
char*     tgoto(const char*, int, int);
long      time(long*);
char*     tmpnam(char*);
int       tputs(const char *, int, auto int (*)());
int       truncate(const char*, unsigned long);
char*     ttyname(int);
int       ttyslot(void);
unsigned  ualarm(unsigned, unsigned);
long      ulimit(int, long);
// int       umask(int); /* commented out for now; wrong for SunOs4.1 */
int       unlink(const char*);
unsigned  usleep(unsigned);
int       vadvise(int);
void*     valloc(unsigned);
int       vfork(void);
int       vhangup(void);
int       wait(int*);
int       write(int, const void*, unsigned);


}


#ifdef USG
void  bzero(void* s, int l);
int   bcmp(const void* s, const void* t, int l);
char* index(const char* s, int c);
char* rindex(const char* s, int c);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)
inline void  bzero(void* s, int l) { memset(s, 0, l); }
inline int   bcmp(const void* s, const void* t, int l) {return memcmp(s,t,l);}
inline char* index(const char* s, int c) { return strchr(s, c); }
inline char* rindex(const char* s, int c) { return strrchr(s, c); }
#endif

#else /* not USG */

extern "C" {
int       bcmp(const void*, const void*, int);
void      bzero(void*, int);
char*     index(const char*, int);
char*     rindex(const char*, int);
}

#endif /* USG */

extern char**   environ;
extern volatile int errno;
extern char*    sys_errlist[];
extern int      sys_nerr;                  
extern char*    optarg;
extern int      opterr;
extern int      optind;


extern "C" void* alloca(unsigned long);
#ifndef alloca
#define alloca(x)  __builtin_alloca(x)
#endif
#ifndef __GNUG__
extern "C" void *__builtin_alloca (int);
#endif


#endif 
