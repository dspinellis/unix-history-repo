/* jobs.h -- structures and stuff used by the jobs.c file. */
#include "quit.h"

/* Defines controlling the fashion in which jobs are listed. */
#define JLIST_STANDARD 0
#define JLIST_LONG     1
#define JLIST_PID_ONLY 2
#define JLIST_CHANGED_ONLY 3

/* Cadmus machines are brain-dead from the moment of fission, like all
   bacteria. */
#if defined (cadmus) || defined (BrainDeath)
#  undef HAVE_WAIT_H
#endif /* BrainDeath */

/* HP/UX 6.x sys/wait.h is a complete loss when it comes to the WIF macros.
   Pretend we don't have a wait.h. */
#if defined (hpux) && !defined (_POSIX_VERSION)
#undef HAVE_WAIT_H
#endif

#if defined (HAVE_WAIT_H)
#include <sys/wait.h>
#else

#include "endian.h"

#if !defined (_POSIX_VERSION)
#if defined (LITTLE_ENDIAN)
union wait
  {
    int	w_status;		/* used in syscall */

    /* Terminated process status. */
    struct
      {
	unsigned short
	  w_Termsig  : 7,	/* termination signal */
	  w_Coredump : 1,	/* core dump indicator */
	  w_Retcode  : 8,	/* exit code if w_termsig==0 */
	  w_Fill1    : 16;	/* high 16 bits unused */
      } w_T;

    /* Stopped process status.  Returned
       only for traced children unless requested
       with the WUNTRACED option bit. */
    struct
      {
	unsigned short
	  w_Stopval : 8,	/* == W_STOPPED if stopped */
	  w_Stopsig : 8,	/* actually zero on XENIX */
	  w_Fill2   : 16;	/* high 16 bits unused */
      } w_S;
  };

#else  /* if !LITTLE_ENDIAN */

/* This is for big-endian machines like the IBM RT, HP 9000, or Sun-3 */

union wait
  {
    int	w_status;		/* used in syscall */

    /* Terminated process status. */
    struct
      {
	unsigned short w_Fill1    : 16;	/* high 16 bits unused */
	unsigned       w_Retcode  : 8;	/* exit code if w_termsig==0 */
	unsigned       w_Coredump : 1;	/* core dump indicator */
	unsigned       w_Termsig  : 7;	/* termination signal */
      } w_T;

    /* Stopped process status.  Returned
       only for traced children unless requested
       with the WUNTRACED option bit. */
    struct
      {
	unsigned short w_Fill2   : 16;	/* high 16 bits unused */
	unsigned       w_Stopsig : 8;	/* signal that stopped us */
	unsigned       w_Stopval : 8;	/* == W_STOPPED if stopped */
      } w_S;
  };

#endif /* LITTLE_ENDIAN */

#define	w_termsig w_T.w_Termsig
#define w_coredump w_T.w_Coredump
#define w_retcode w_T.w_Retcode
#define w_stopval w_S.w_Stopval
#define w_stopsig w_S.w_Stopsig

/* Note that sys/wait.h defines these for Posix systems. */
#define	WSTOPPED 0177
#define WIFSTOPPED(x) (((x) . w_stopval) == WSTOPPED)
#define WIFEXITED(x) ((! (WIFSTOPPED (x))) && (((x) . w_termsig) == 0))
#define WIFSIGNALED(x) ((! (WIFSTOPPED (x))) && (((x) . w_termsig) != 0))

#endif /* _POSIX_VERSION */
#endif  /* !HAVE_WAIT_H */

#if !defined (_POSIX_VERSION)
#define pid_t int
typedef union wait WAIT;
#else
typedef int WAIT;
#endif /* _POSIX_VERSION */

/* How to get the status of a job.  For Posix, this is just an int, but for
   other systems we have to crack the union wait. */
#if defined (_POSIX_VERSION)
#define WSTATUS(t)  (t)
#else
#define WSTATUS(t)  (t.w_status)
#endif

/* Make sure that parameters to wait3 are defined. */
#if !defined (WNOHANG)
#define WNOHANG 1
#define WUNTRACED 2
#endif /* WNOHANG */

/* More Posix P1003.1 definitions.  In these definitions, `s' is a
   `union wait' (the 1003.1 spec says they are `int'). */
#if !defined (WSTOPSIG)
#define WSTOPSIG(s)	((s).w_stopsig)
#define WTERMSIG(s)	((s).w_termsig)
#define WEXITSTATUS(s)	((s).w_retcode)
#endif /* WSTOPSIG */

#if !defined (WIFCORED)
#if !defined (_POSIX_VERSION)
#define WIFCORED(s)	((s).w_coredump)
#else
#define WIFCORED(s)	((s) & 0200)
#endif /* _POSIX_VERSION */
#endif /* WIFCORED */

/* I looked it up.  For pretty_print_job ().  The real answer is 24. */
#define LONGEST_SIGNAL_DESC 24

/* We keep an array of jobs.  Each entry in the array is a linked list
   of processes that are piped together.  The first process encountered is
   the group leader. */

/* Each child of the shell is remembered in a STRUCT PROCESS.  A chain of
   such structures is a pipeline.  The chain is circular. */
typedef struct process {
  struct process *next;	/* Next process in the pipeline.  A circular chain. */
  pid_t pid;		/* Process ID. */
  WAIT status;		/* The status of this command as returned by wait. */
  int running;		/* Non-zero if this process is running. */
  char *command;	/* The particular program that is running. */
} PROCESS;

/* A description of a pipeline's state. */
typedef enum { JRUNNING, JSTOPPED, JDEAD, JMIXED } JOB_STATE;
#define JOBSTATE(job) (jobs[(job)]->state)

typedef struct job {
  char *wd;		/* The working directory at time of invocation. */
  PROCESS *pipe;	/* The pipeline of processes that make up this job. */
  pid_t pgrp;		/* The process ID of the process group (necessary). */
  int foreground;	/* Non-zero if this is running in the foreground. */
  int notified;		/* Non-zero if already notified about job state. */
  JOB_STATE state;	/* The state that this job is in. */
  int job_control;	/* Non-zero if this job started under job control. */
#ifdef JOB_CONTROL
  COMMAND *deferred;	/* Commands that will execute when this job is done. */
#endif
} JOB;

#define NO_JOB -1	/* An impossible job array index. */
#define DUP_JOB -2	/* A possible return value for get_job_spec (). */

/* A value which cannot be a process ID. */
#define NO_PID (pid_t)-1

#if !defined (_POSIX_VERSION) && !defined (sigmask)
#define sigmask(x) (1 << ((x)-1))
#endif /* !POSIX && !sigmask */

#ifndef SIGABRT
#define SIGABRT SIGIOT
#endif

#ifndef SIGCHLD
#define SIGCHLD SIGCLD
#endif

#if !defined (_POSIX_VERSION)
#if !defined (SIG_BLOCK)
#define SIG_BLOCK 2
#define SIG_SETMASK 3
#endif /* SIG_BLOCK */

/* Type of a signal set. */
#define sigset_t int

/* Make sure there is nothing inside the signal set. */
#define sigemptyset(set) (*(set) = 0)

/* Add SIG to the contents of SET. */
#define sigaddset(set, sig) *(set) |= sigmask (sig)

/* Suspend the process until the reception of one of the signals
   not present in SET. */
#define sigsuspend(set) sigpause (*(set))

/* END of POSIX 1003.1 definitions. */
#endif /* _POSIX_VERSION */

/* These definitions are used both in POSIX and non-POSIX implementations. */

#define BLOCK_SIGNAL(sig, nvar, ovar) \
  sigemptyset (&nvar); \
  sigaddset (&nvar, sig); \
  sigemptyset (&ovar); \
  sigprocmask (SIG_BLOCK, &nvar, &ovar)

#if defined (_POSIX_VERSION)
#define BLOCK_CHILD(nvar, ovar) BLOCK_SIGNAL (SIGCHLD, nvar, ovar)
#define UNBLOCK_CHILD(ovar) sigprocmask (SIG_SETMASK, &ovar, (sigset_t *) NULL)
#else /* !_POSIX_VERSION */
#define BLOCK_CHILD(nvar, ovar) ovar = sigblock (sigmask (SIGCHLD))
#define UNBLOCK_CHILD(ovar) sigsetmask (ovar)
#endif /* !_POSIX_VERSION */

/* System calls. */
extern pid_t fork (), getpid (), getpgrp ();

/* Stuff from the jobs.c file. */
extern pid_t  original_pgrp, shell_pgrp, pipeline_pgrp;
extern pid_t last_made_pid, last_asynchronous_pid, make_child ();
extern int current_job, previous_job;
extern int asynchronous_notification;
extern JOB **jobs;
extern int job_slots;
