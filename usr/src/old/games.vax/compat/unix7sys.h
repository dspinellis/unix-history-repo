/*
 * 	unix7sys.h	1.1	82/05/12
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
#define	MKNOD	14
#define	BRK	17
#define	STAT	18
#define	SEEK	19
#define	SETUID	23
#define	GETUID	24
#define	FSTAT	28
#define	PIPE	42
#define	TIMES	43
#define	SETGID	46
#define	GETGID	47
#define	SIG	48
#define	EXECE	59
/*
 *	The numerical entries in the following table are
 *	really composed of 3 parts.
 *	The first entry in each row indicates the number
 *	of register arguments for the system call, while
 *	the 2nd position is the number of memory arguments
 *	the 3rd position is LONGRET if the return is a long (r0 and r1)
 *	or is TWORET if the return is 2 ints ala pipe
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
	0, 3, NORMRET,		/* 16 = chown; now 3 args */
	0, 1, NORMRET,		/* 17 = break */
	0, 2, NORMRET,		/* 18 = stat */
	1, 3, LONGRET,		/* 19 = seek; now 3 args */
	0, 0, NORMRET,		/* 20 = getpid */
	0, 3, NORMRET,		/* 21 = mount */
	0, 1, NORMRET,		/* 22 = umount */
	1, 0, NORMRET,		/* 23 = setuid */
	0, 0, TWORET,		/* 24 = getuid */
	2, 0, NORMRET,		/* 25 = stime */
	1, 3, NORMRET,		/* 26 = ptrace */
	1, 0, NORMRET,		/* 27 = alarm */
	1, 1, NORMRET,		/* 28 = fstat */
	0, 0, NORMRET,		/* 29 = pause */
	0, 2, NORMRET,		/* 30 = utime */
	1, 1, NORMRET,		/* 31 = stty */
	1, 1, NORMRET,		/* 32 = gtty */
	0, 2, NORMRET,		/* 33 = access */
	1, 0, NORMRET,		/* 34 = nice */
	0, 1, NORMRET,		/* 35 = ftime; formerly sleep */
	0, 0, NORMRET,		/* 36 = sync */
	1, 1, NORMRET,		/* 37 = kill */
	0, 0, NORMRET,		/* 38 = switch; inoperative */
	0, 0, NORMRET,		/* 39 = setpgrp (not in yet) */
	ILLSYS, 0, NORMRET,	/* 40 = tell (obsolete) */
	2, 0, NORMRET,		/* 41 = dup */
	0, 0, TWORET,		/* 42 = pipe */
	0, 1, NORMRET,		/* 43 = times */
	0, 4, NORMRET,		/* 44 = prof */
	ILLSYS, 0, NORMRET,	/* 45 = unused */
	1, 0, NORMRET,		/* 46 = setgid */
	0, 0, TWORET,		/* 47 = getgid */
	0, 2, NORMRET,		/* 48 = sig */
	ILLSYS, 0, NORMRET,	/* 49 = reserved for USG */
	ILLSYS, 0, NORMRET,	/* 50 = reserved for USG */
	0, 1, NORMRET,		/* 51 = turn acct off/on */
	0, 3, NORMRET,		/* 52 = set user physical addresses */
	0, 1, NORMRET,		/* 53 = lock user in core */
	0, 3, NORMRET,		/* 54 = ioctl */
	ILLSYS, 0, NORMRET,	/* 55 = readwrite (in abeyance) */
	0, 4, NORMRET,		/* 56 = creat mpx comm channel */
	ILLSYS, 0, NORMRET,	/* 57 = reserved for USG */
	ILLSYS, 0, NORMRET,	/* 58 = reserved for USG */
	0, 3, NORMRET,		/* 59 = exece */
	0, 1, NORMRET,		/* 60 = umask */
	0, 1, NORMRET,		/* 61 = chroot */
	ILLSYS, 0, NORMRET,	/* 62 = x */
	ILLSYS, 0, NORMRET	/* 63 = used internally */
};
