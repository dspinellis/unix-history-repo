/*	831.c	4.1	83/5/10	*/

#if V831
/*
 * Routines for dialing up on Vadic 831
 */
#include "tip.h"
#include <setjmp.h>
#include <errno.h>
#include <sgtty.h>
#include <sys/file.h>
#include <time.h>

static char *sccsid = "@(#)v831.c	4.1 %G%";

struct mx_leaves {
    char    *name;
    char    rack,modem;
} pdevs[] = {{"/dev/cua0",'4','0'}, {"/dev/cua1",'4','1'}, {0}};

struct timeval zerotime = {0L, 0L};

#define unlike(a,b) (strcmp(a,b))
#define pc(x) (c = x, write(AC,&c,1))
#define ABORT	01
#define SI	017
#define STX	02
#define ETX	03

int v831_abort();

int alarmtr();

static jmp_buf jmpbuf;
static int child = -1;

v831_dialer(num, acu)
	char *num, *acu;
{
	extern errno;
	char *p, *q, phone[40];
	char char_rv;
	int lt, nw, connected = 1;
	register int timelim;

	if (boolean(value(VERBOSE)))
		printf("\nstarting call...");
#ifdef DEBUG
	printf ("(acu=%s)", acu);
#endif
	if ((AC = open(acu, FRDWR)) < 0) {
		if (errno == EBUSY)
			printf("line busy...");
		else
			printf("acu open error...");
		return (0);
	}
	if (setjmp(jmpbuf)) {
		kill(child, SIGKILL);
		close(AC);
		return (0);
	}
	signal(SIGALRM, alarmtr);
	timelim = 5 * strlen(num);
	alarm(timelim < 30 ? 30 : timelim);
	if ((child = fork()) == 0) {
		/*
		 * ignore this stuff for aborts
		 */
		signal(SIGALRM, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		sleep(2);
		/*nw = write(AC, num, lt = strlen(num));*/
		char_rv = dialit (num, acu);
		exit(char_rv != 'A');
	}
	/*
	 * open line - will return on carrier
	 */
	if ((FD = open(DV, 2)) < 0) {
#ifdef DEBUG
		printf("(after open, errno=%d)", errno);
#endif
		if (errno == EIO)
			printf("lost carrier...");
		else
			printf("dialup line open failed...");
		alarm(0);
		kill(child, SIGKILL);
		close(AC);
		return (0);
	}
	alarm(0);
	/*ioctl(AC, TIOCHPCL, 0);*/
	signal(SIGALRM, SIG_DFL);
	while ((nw = wait(&lt)) != child && nw != -1)
		;
	fflush(stdout);
	/*close(AC);*/
	if (lt != 0) {
		close(AC);
		return (0);
	}
	return (1);
}

alarmtr()
{
	alarm(0);
	longjmp(jmpbuf, 1);
}

/*
 * Insurance, for some reason we don't seem to be
 *  hanging up...
 */
v831_disconnect()
{
	struct sgttyb cntrl;
	sleep(2);
#ifdef VMUNIX
#ifdef DEBUG
	printf ("[disconnect: FD=%d]", FD);
#endif
	if (FD > 0)
	{
		ioctl (FD, TIOCCDTR, 0);
		ioctl (FD, TIOCGETP, &cntrl);
		cntrl.sg_ispeed = 0;
		cntrl.sg_ospeed = 0;
		ioctl (FD, TIOCSETP, &cntrl);
		ioctl (FD, TIOCNXCL, (struct sgttyb *)NULL);
	}
#endif
	close(FD);
}

v831_abort()
{
#ifdef DEBUG
	printf ("[abort: AC=%d]", AC);
#endif
	sleep(2);
	if (child > 0)
		kill(child, SIGKILL);
	if (AC > 0)
		ioctl (FD, TIOCNXCL, (struct sgttyb *)NULL);
		close(AC);
#ifdef VMUNIX
	if (FD > 0)
		ioctl(FD, TIOCCDTR, 0);
#endif
	close(FD);
}
#endif

static struct sgttyb cntrl;
dialit(string, acu)
register char *string;
char *acu;
{
	char c, cc, *sanitize();
	int i;
	register struct mx_leaves *lp = pdevs;
	int test;
	int nfds, fdsmask;

	string = sanitize(string);
#ifdef DEBUG
	printf ("(dial string=%s)", string);
#endif
	if(*string=='<' && string[1]==0) {
		return('Z');
	}

	while(test = unlike(lp->name,acu))
	    if(lp->name==0) {
		printf("Unable to locate dialer (%s)\n", acu);
		return('K');
	    } else lp++;


	gtty (AC,&cntrl);	/* set raw, -echo, 2400 Baud */
	cntrl.sg_ispeed = cntrl.sg_ospeed = B2400;
	cntrl.sg_flags = RAW | EVENP | ODDP;
	stty (AC,&cntrl);

	/* check for characters waiting from dialer (throw them away) */

	fdsmask = 1<<AC;
#ifdef DEBUG
	printf ("{select returns=%d}", select (20, &fdsmask, 0, 0, &zerotime));
#endif

	pc (STX); pc (lp->rack); pc (lp->modem);
	for (;*string && *string!='<'; string++)
	{
#ifdef DEBUG
	    printf ("%c", *string);
#endif
	    pc(*string);
	}
	pc(SI); pc(ETX);

	sleep (1);
	i = read (AC, &c, 1);
#ifdef DEBUG
	printf ("read response of %d chars, char = %c\n", i, c);
	printf ("and errno is %d\n", errno);
#endif

	if (i !=1) c = 'M';
	if (c=='B' || c=='G') {
		char oc = c;
		pc(ABORT);
		read (AC, &cc, 1);
#ifdef DEBUG
		printf ("abort response=%c\n", cc);
#endif
		c = oc;
		v831_disconnect ();
	}

	close(AC);
#ifdef DEBUG
	printf ("dialit: returns %c\n", c);
#endif
	return(c);
}
char *
sanitize(string)
register char *string;
{
	static char buf[512];
	register char *cp = buf;
	for(;*string; string++) {
	    switch(*string) {
	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9': case '<':
		*cp++ = *string;
		break;
	    case '_':
		*cp++ = '=';
		break;
	    }
	}
	*cp++ = 0;
	return(buf);
}
