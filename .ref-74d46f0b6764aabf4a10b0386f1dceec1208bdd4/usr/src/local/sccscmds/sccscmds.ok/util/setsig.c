static char Sccsid[] = "@(#)setsig.c	1.3	%G%";

# include	"signal.h"
#undef NSIG
# ifdef PWB
#define NSIG 16
# else
#define NSIG 4
# endif
# include	"../hdr/macros.h"
#include <sys/syscall.h>
#define	syswrite(a,b,c)	syscall(SYS_write,a,b,c)

/*
	General-purpose signal setting routine.
	All non-ignored, non-caught signals are caught.
	If a signal other than hangup, interrupt, or quit is caught,
	a "user-oriented" message is printed on file descriptor 2 with
	a number for help(I).
	If hangup, interrupt or quit is caught, that signal	
	is set to ignore.
	Termination is like that of "fatal",
	via "clean_up(sig)" (sig is the signal number)
	and "exit(userexit(1))".
 
	If the file "dump.core" exists in the current directory
	the function commits
	suicide to produce a core dump
	(after calling clean_up, but before calling userexit).
*/


char	*Mesg[NSIG] = {
	0,
	0,	/* Hangup */
	0,	/* Interrupt */
	0,	/* Quit */
# ifdef PWB
	"Illegal instruction",
	"Trace/BPT trap",
	"IOT trap",
	"EMT trap",
	"Floating exception",
	"Killed",
	"Bus error",
	"Memory fault",
	"Bad system call",
	"Broken pipe",
	"Alarm clock",
	"Terminated"
# endif PWB
};


setsig()
{
	extern int setsig1();
	register int j, n;

	for (j=1; j<NSIG; j++)
		if (n=signal(j,setsig1))
			signal(j,n);
}


static char preface[] = "SIGNAL: ";
static char endmsg[] = " (ut12)\n";

setsig1(sig)
int sig;
{
# ifndef PWB
	sig = 2;
# endif PWB
	if (Mesg[sig]) {
		syswrite(2,preface,length(preface));
		syswrite(2,Mesg[sig],length(Mesg[sig]));
		syswrite(2,endmsg,length(endmsg));
	}
	else
		signal(sig,1);
	clean_up(sig);
	if(open("dump.core",0) > 0) {
		signal(SIGIOT,0);
		abort();
	}
	exit(userexit(1));
}
