/*
 * 	unix6sys.h	4.2	83/07/31
 */
#define	NSYSTRAPS	64
#define	NSIGS	13
#define	ILLSYS	-1
#define	NORMRET	0
#define	LONGRET	1
#define	TWORET	2
#define	FORK	2
#define	OPEN	5
#define	WAIT	7
#define	CREAT	8
#define	LINK	9
#define	UNLNK	10
#define EXEC	11
#define	CHDIR	12
#define	TIME	13
#define	MKNOD	14
#define	BRK	17
#define	STAT	18
#define	SEEK	19
#define	SETUID	23
#define	GETUID	24
#define	STIME	25
#define	FSTAT	28
#define	STTY	31
#define	GTTY	32
#define	NICE	34
#define	SLEEP	35
#define	TELL	40
#define	PIPE	42
#define	TIMES	43
#define	SETGID	46
#define	GETGID	47
#define	SIG	48
#define	PWBSYS	57
#define	UNAME	0
#define	UDATA	1
#define	USTAT	2
#define	UTIME	3
/*
 *	The numerical entries in the following table are
 *	really composed of 2 parts.
 *	The first entry in each row indicates the number
 *	of register arguments for the system call, while
 *	the 2nd position is the number of memory arguments
 *	the 3rd position is LONGRET if the return is a long (ala time)
 *	or is TWORET if the return is 2 ints (ala pipe)
 *	otherwise it is NORMRET.
 */
int	sysargs[][3] =
{
	0, 0, NORMRET,		/*  0 = indir */
	1, 0, NORMRET,		/*  1 = exit */
	0, 0, NORMRET,		/*  2 = fork */
	1, 2, NORMRET,		/*  3 = read */
	1, 2, NORMRET,		/*  4 = write */
	0, 2, NORMRET,		/*  5 = open */
	1, 0, NORMRET,		/*  6 = close */
	0, 0, TWORET,		/*  7 = wait */
	0, 2, NORMRET,		/*  8 = creat */
	0, 2, NORMRET,		/*  9 = link */
	0, 1, NORMRET,		/* 10 = unlink */
	0, 2, NORMRET,		/* 11 = exec */
	0, 1, NORMRET,		/* 12 = chdir */
	0, 0, LONGRET,		/* 13 = time */
	0, 3, NORMRET,		/* 14 = mknod */
	0, 2, NORMRET,		/* 15 = chmod */
	0, 2, NORMRET,		/* 16 = chown */
	0, 1, NORMRET,		/* 17 = break */
	0, 2, NORMRET,		/* 18 = stat */
	1, 2, NORMRET,		/* 19 = seek */
	0, 0, NORMRET,		/* 20 = getpid */
	0, 3, NORMRET,		/* 21 = mount */
	0, 1, NORMRET,		/* 22 = umount */
	1, 0, NORMRET,		/* 23 = setuid */
	0, 0, NORMRET,		/* 24 = getuid */
	2, 0, NORMRET,		/* 25 = stime */
	1, 3, NORMRET,		/* 26 = ptrace */
	1, 0, NORMRET,		/* 27 = alarm */
	1, 1, NORMRET,		/* 28 = fstat */
	0, 0, NORMRET,		/* 29 = pause */
	1, 1, NORMRET,		/* 30 = smdate */
	1, 1, NORMRET,		/* 31 = stty */
	1, 1, NORMRET,		/* 32 = gtty */
	0, 2, NORMRET,		/* 33 = access */
	1, 0, NORMRET,		/* 34 = nice */
	1, 0, NORMRET,		/* 35 = sleep */
	0, 0, NORMRET,		/* 36 = sync */
	1, 1, NORMRET,		/* 37 = kill */
	0, 0, NORMRET,		/* 38 = switch */
	ILLSYS, 0, NORMRET,	/* 39 = x */
	1, 0, LONGRET,		/* 40 = tell */
	1, 0, NORMRET,		/* 41 = dup */
	0, 0, TWORET,		/* 42 = pipe */
	0, 1, NORMRET,		/* 43 = times */
	0, 4, NORMRET,		/* 44 = prof */
	ILLSYS, 0, NORMRET,	/* 45 = tiu */
	1, 0, NORMRET,		/* 46 = setgid */
	0, 0, NORMRET,		/* 47 = getgid */
	0, 2, NORMRET,		/* 48 = sig */
	ILLSYS, 0, NORMRET,	/* 49 = x */
	ILLSYS, 0, NORMRET,	/* 50 = x */
	ILLSYS, 0, NORMRET,	/* 51 = x */
	ILLSYS, 0, NORMRET,	/* 52 = x */
	ILLSYS, 0, NORMRET,	/* 53 = x */
	ILLSYS, 0, NORMRET,	/* 54 = x */
	ILLSYS, 0, NORMRET,	/* 55 = x */
	ILLSYS, 0, NORMRET,	/* 56 = x */
	2, 1, NORMRET,		/* 57 = pwbsys */
	ILLSYS, 0, NORMRET,	/* 58 = x */
	ILLSYS, 0, NORMRET,	/* 59 = x */
	ILLSYS, 0, NORMRET,	/* 60 = x */
	ILLSYS, 0, NORMRET,	/* 61 = x */
	0, 1, NORMRET,		/* 62 = idisys */
	ILLSYS, 0, NORMRET	/* 63 = x */
};
