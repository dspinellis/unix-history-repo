#include	<signal.h>
#include	<stdio.h>
#include	<setjmp.h>
#include	<sys/ioctl.h>
#include	<sys/types.h>
#include	<sys/time.h>

#define	CTIME	1
#define	ASCTIME	2
#define	TZSET	3
#define	LOCALTIME 4
#define	GMTIME	5
#define	OFFTIME	6

extern	struct	tm	*offtime();

	jmp_buf	env;
	char	*cp, junk[52];
	long	l, off;
	int	timeout(), checkppid();
	struct	tm	tmtmp, *tp;

main()
	{
	register int i;
	struct	itimerval it;
	u_char	c;

	signal(SIGPIPE, SIG_DFL);
	for	(i = getdtablesize(); --i > 2; )
		close(i);
/*
 * Need a timer running while we disassociate from the control terminal
 * in case of a modem line which has lost carrier.
*/
	timerclear(&it.it_interval);
	it.it_value.tv_sec = 5;
	it.it_value.tv_usec = 0;
	signal(SIGALRM, timeout);
	setitimer(ITIMER_REAL, &it, (struct itimerval *) NULL);
	if	(setjmp(env) == 0)
		{
		i = open("/dev/tty", 0);
		if	(i >= 0)
			{
			ioctl(i, TIOCNOTTY, NULL);
			close(i);
			}
		}
/*
 * Now start a timer with one minute refresh.  In the signal service
 * routine, check the parent process id to see if this process has
 * been orphaned and if so exit.  This is primarily aimed at removing
 * the 'ctimed' process left behind by 'sendmail's multi-fork startup
 * but may prove useful in preventing accumulation of 'ctimed' processes
 * in other circumstances as well.  Normally this process is short
 * lived.
*/
	it.it_interval.tv_sec = 60;
	it.it_interval.tv_usec = 0;
	it.it_value.tv_sec = 60;
	it.it_value.tv_usec = 0;
	signal(SIGALRM, checkppid);
	setitimer(ITIMER_REAL, &it, (struct itimerval *) NULL);

	while	(read(fileno(stdin), &c, 1) == 1)
		{
		switch	(c)
			{
			case	CTIME:
				l = 0L;
				getb(fileno(stdin), &l, sizeof l);
				cp = ctime(&l);
				write(fileno(stdout), cp, 26);
				break;
			case	ASCTIME:
				getb(fileno(stdin), &tmtmp, sizeof tmtmp);
				cp = asctime(&tmtmp);
				write(fileno(stdout), cp, 26);
				break;
			case	TZSET:
				(void) tzset();
				break;
			case	LOCALTIME:
				l = 0L;
				getb(fileno(stdin), &l, sizeof l);
				tp = localtime(&l);
				write(fileno(stdout), tp, sizeof (*tp));
				strcpy(junk, tp->tm_zone);
				junk[24] = '\0';
				write(fileno(stdout), junk, 24);
				break;
			case	GMTIME:
				l = 0L;
				getb(fileno(stdin), &l, sizeof l);
				tp = gmtime(&l);
				write(fileno(stdout), tp, sizeof (*tp));
				strcpy(junk, tp->tm_zone);
				junk[24] = '\0';
				write(fileno(stdout), junk, 24);
				break;
			case	OFFTIME:
				getb(fileno(stdin), &l, sizeof l);
				getb(fileno(stdin), &off, sizeof off);
				tp = offtime(&l, off);
				write(fileno(stdout), tp, sizeof (*tp));
				break;
			default:
				abort("switch");
			}
		}
	}

getb(f, p, n)
	int	f;
	register char	*p;
	register int	n;
	{
	register int	i;

	while	(n)
		{
		i = read(f, p, n);
		if	(i <= 0)
			return;
		p += i;
		n -= i;
		}
	}

timeout()
	{

	longjmp(env, 1);
	}

checkppid()
	{

	if	(getppid() == 1)
		exit(0);
	}
