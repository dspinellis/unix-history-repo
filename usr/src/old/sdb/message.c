static	char sccsid[] = "@(#)message.c 4.2 %G%";
#
/*
 *
 *	UNIX debugger
 *
 */



#include	"mac.h"
#include	"mode.h"


MSG		BADMOD	=  "bad modifier";
MSG		NOBKPT	=  "no breakpoint set";
MSG		NOPCS	=  "no process";
MSG		BADTXT	=  "text address not found";
MSG		BADDAT	=  "data address not found";
MSG		EXBKPT	=  "too many breakpoints";
MSG		BADWAIT	=  "wait error: process disappeared!";
MSG		ENDPCS	=  "process terminated";
MSG		NOFORK	=  "try again";
MSG		SZBKPT	=  "bkpt: command too long";
MSG		BADMAG	=  "bad core magic number";

STRING		signals[] = {
	"",
	"hangup",
	"interrupt",
	"quit",
	"illegal instruction",
	"trace/BPT",
	"IOT",
	"EMT",
	"floating exception",
	"killed",
	"bus error",
	"memory fault",
	"bad system call",
	"broken pipe",
	"alarm call",
	"terminated",
	"signal 16",
	"stop (signal)",
	"stop (tty)",
	"continue (signal)",
	"child termination",
	"stop (tty input)",
	"stop (tty output)",
	"input available (signal)",
	"cpu timelimit",
	"file sizelimit",
	"signal 26",
	"signal 27",
	"signal 28",
	"signal 29",
	"signal 30",
	"signal 31",
};
int	nsig = sizeof (signals)/sizeof (signals[0]);
