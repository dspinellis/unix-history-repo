#
static char sccsid[] = "	unixtraps.c	4.1	82/05/12	";
/*	Function to execute version 6 and version 7 UNIX system calls from
 *	compatability mode on UNIX-32V.
 *	Art Wetzel	August 1979
 */
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef V6UNIX
#ifdef TRACE
#define	RTSNAME	"/../../../../usr/local/v6trc"
#else
#define	RTSNAME	"/../../../../usr/local/v6run"
#endif
#include "unix6sys.h"
#ifdef TRACE
#include "unix6sysn.h"
#endif
#endif
#ifdef V7UNIX
#ifdef TRACE
#define	RTSNAME	"/../../../../usr/local/v7trc"
#else
#define	RTSNAME	"/../../../../usr/local/v7run"
#endif
#include "unix7sys.h"
#ifdef TRACE
#include "unix7sysn.h"
#endif
#endif
#include "defs.h"
#define	CARRY	1
#define	MAXSARGS	25
#ifdef V6UNIX
#define	ARGVLEN	512
#define	ENVLEN	0
#endif
#ifdef V7UNIX
#define	ARGVLEN	5120
#define	ENVLEN	1000
#endif
char	argvs[ARGVLEN+ENVLEN];
int	args[MAXSARGS];
/* 32v type stat structure */
extern struct	stat	stat32v;
/* place for times data so we can reverse the longs */
struct timebuf {
	long	t1;
	long	t2;
	long	t3;
	long	t4;
} timebuf;
/* place for pipe file descriptors */
int	pipes[2];
/* wait status */
int	wstatus;
#ifdef	V6UNIX
/* version 6 style stat structure */
struct v6nod {
	dev_t	majmin;
	ino_t	inumber;
	unsigned short	flags;
	unsigned char	nlinks;
	unsigned char	uid;
	unsigned char	gid;
	unsigned char	size0;
	unsigned short	size1;
	unsigned short	addr[8];
	long	actime;
	long	modtime;
} *v6stat;
#endif
/* do the trap stuff for the trap with code */
dotrap(code) int code; {
	register unsigned short *argp, *savp, *savep;
	register int i, j, indirflg;
	register char *avp, *oavp;
	extern sigcatch();
	extern errno;
	/* clear out condition codes of psl */
	psl &= ~017;
	/* special case of indirect sys call */
	if(code == 0) {
		/* remember this was indirect */
		indirflg = 1;
		/* point to args */
		argp = (unsigned short *)*(pc++);
		/* code for indirect sys call */
		code = *argp++;
		/* is it legit */
		if(code>>8 != TRAPS) {
			fprintf(stderr,"Bad indirect sys call at 0x%x\n",pc-2);
			pc++;
			/* set carry flag */
			psl |= CARRY;
			regs[0] = -1;
			return(-1);
		}
		code &= 0377;
	}
	else {
		/* remember this was not indirect */
		indirflg = 0;
		/* point to args */
		argp = pc;
	}
	/* check if code too high or bad sys code */
	if(code >= NSYSTRAPS || sysargs[code][0] == ILLSYS) {
		fprintf(stderr,"Unimplimented trap %d at 0x%x\n",code,argp);
		/* set carry bit */
		psl |= CARRY;
		regs[0] = -1;
		return(-1);
	}
	/* copy args to known locations */
	i=0;
	for(j=0; j<sysargs[code][0]; j++) args[i++] = regs[j];
	for(j=0; j<(sysargs[code][1]); j++) args[i++] = *argp++;
#ifdef TRACE
	fprintf(stderr,"pid %d ",getpid());
	if(indirflg) fprintf(stderr,"indirect ");
	fprintf(stderr,"%s (%d) from 0%o with %d args",sysnames[code],code,pc-1,i);
	for(j=0; j<i; j++)
		fprintf(stderr," 0%o",args[j]);
	if(code==OPEN||code==STAT||code==CREAT||code==EXEC||code==UNLNK||code==LINK||code==CHDIR||code==MKNOD)
		fprintf(stderr," (%s)",args[0]);
#ifdef V7UNIX
	if(code==EXECE)
		fprintf(stderr," (%s)",args[0]);
#endif
	if(code==LINK)
		fprintf(stderr," (%s)",args[1]);
#endif
	/* go do whatever sys call it is */
	switch(code) {
	case	FORK:
		/* indirect forks return pids on both sides - must do here */
		/* this is possibly a bug in 32V */
		i = fork();
		break;
	case	WAIT:
		i = wait(&wstatus);
		args[0] = i;
		args[1] = wstatus;
		break;
	case	EXEC:
#ifdef V7UNIX
	case	EXECE:
#endif
		/*
		 *  have to do a lot of junk here to fix up an argv
		 *  for execute since (1) the pdp-11 argv consists of 16
		 *  bit pointers and (2) the argv itself is in the
		 *  pdp-11 program space where it would get clobbered
		 *  when a new program is read in and before its
		 *  argv is set up.
		 */
		avp = &argvs[0];
		savp = (unsigned short *)args[1];
#ifdef	V6UNIX
		for(i=1; args[i] = *savp++; i++)
			if(args[i] == 0177777) break;
#ifdef	TRACE
			else fprintf(stderr,"argv[%d]%s ",i-1,args[i]);
#endif
#endif
#ifdef	V7UNIX
		savep = (unsigned short *)args[2];
		for(i=1; args[i] = *savp++; i++)
#ifdef	TRACE
			fprintf(stderr,"argv[%d]%s ",i-1,args[i]);
#else
			;
#endif
#endif
		if(stat(args[0], &stat32v)) {
			/* return error here if file does not exist */
#ifdef	TRACE
			fprintf(stderr," does not exist\n");
#endif
			i = -1;
			break;
		}
		/* must have execute permission */
		if(stat32v.st_mode & (S_IEXEC>>6)) goto experm;
		if(stat32v.st_mode & (S_IEXEC>>3)) {
			if(stat32v.st_gid == getegid()) goto experm;
			if(geteuid() == 0) goto experm;
		}
		if(stat32v.st_mode & S_IEXEC) {
			if(stat32v.st_uid == geteuid()) goto experm;
			if(geteuid() == 0) goto experm;
		}
		/* return failure if no exec permision allowed */
		i = -1;
experm:
		/* can't exec a directory */
		if(stat32v.st_mode & S_IFDIR)
			i = -1;
		if(i == -1) break;
		args[i] = 0;
		for(j=0; j<i; j++) {
			oavp = (char *)args[j];
			args[j] = (int)avp;
			while(*avp++ = *oavp++) ;
		}
#ifdef V7UNIX
		if(code == EXECE) {
			for(j = ++i; args[j] = *savep++; j++) ;
			for( ; j>i; j--) {
				oavp = (char *)args[j];
				args[j] = (int)avp;
				while(*avp++ = *oavp++) ;
			}
		}
#endif
		/* SETUID and SETGID files must be started with a fresh RTS */
		if(stat32v.st_mode & S_ISGID || stat32v.st_mode & S_ISUID) {
			/* should add a check here for good magic # in header */
			args[1] = args[0];
			args[0] = (int)RTSNAME;
#ifdef TRACE
			fprintf(stderr," SETUID-GID");
#endif
			if(args[i])
				i = execve(args[0], &args[0], &args[i]);
			else
				i = execv(args[0], &args[0]);
			fprintf(stderr,"can't exec %s\n",RTSNAME);
			break;
		}
		i = execute(args[0], &args[1], &args[i]);
		/* shouldn't get here if exec works */
		break;
	case	SEEK:
#ifdef	V6UNIX
		/* fix up negative offsets */
		if(args[2] != 0 && args[2] != 3)
			if(args[1] >= 32768) args[1] -= 65536;
		if(args[2] <= 2)
			i = lseek(args[0], args[1], args[2]);
		else
			i = lseek(args[0], args[1]*512, args[2]-3);
		if(i != -1) i = 0;
#endif
#ifdef	V7UNIX
		i = lseek(args[0], (args[1]<<16)|(args[2]&0177777), args[3]);
#endif
		break;
#ifdef	V6UNIX
	case	MKNOD:
		/* version 6 uses allocated bit which means regular file here */
		if(args[1] & S_IFBLK)
			args[1] &= ~S_IFREG;
		i = mknod(args[0], args[1], args[2]);
		break;
#endif	
	case	PIPE:
		i = pipe(pipes);
		args[0] = pipes[0];
		args[1] = pipes[1];
		break;
#ifdef	V6UNIX
	case	TELL:
		i = lseek(args[0], 0L, 1);
		break;
#endif
	case	STAT:
	case	FSTAT:
		/* do the syscall to a local stat buffer */
		i = syscall(code, args[0], &stat32v);
		/* reverse the longs */
		stat32v.st_size = longrev(stat32v.st_size);
		stat32v.st_atime = longrev(stat32v.st_atime);
		stat32v.st_mtime = longrev(stat32v.st_mtime);
		stat32v.st_ctime = longrev(stat32v.st_ctime);
#ifdef V7UNIX
		/* copy out otherwise unchanged stat buffer */
		/* in two pieces with st_size as the breaking point */
		/* note that st_rdev is a short but due to alingnmemt */
		/* problems the rest of the structure is out of sync */
		j = (int)((char *)(&stat32v.st_size)-(char *)(&stat32v.st_dev));
		bcopy(&stat32v, args[1], j);
		bcopy(&stat32v.st_size, args[1]+j-2, sizeof(struct stat)-j);
#endif
#ifdef	V6UNIX
		/* point to user area as v6stat structure */
		v6stat = (struct v6nod *)args[1];
		/* copy out piece by piece */
		v6stat->majmin = stat32v.st_dev;
		v6stat->inumber = stat32v.st_ino;
		v6stat->flags = stat32v.st_mode;
		v6stat->nlinks = (unsigned char)stat32v.st_nlink;
		v6stat->uid = (unsigned char)stat32v.st_uid;
		v6stat->gid = (unsigned char)stat32v.st_gid;
		/* note size already reversed */
		v6stat->size0 = (unsigned char)(stat32v.st_size & 0377);
		v6stat->size1 = (unsigned short)(stat32v.st_size>>16);
		v6stat->actime = stat32v.st_atime;
		v6stat->modtime = stat32v.st_mtime;
		/* patch up flags */
		/* for now just set 100000 bit if not a plain file */
		if(v6stat->flags & 060000)
			v6stat->flags |= 0100000;
#endif
		break;
	case	TIMES:
		i = times(&timebuf);
		timebuf.t2 = longrev(timebuf.t2) + timebuf.t1;
		timebuf.t3 = longrev(timebuf.t3);
		timebuf.t4 = longrev(timebuf.t4);
		bcopy(&timebuf.t2,args[0],sizeof(struct timebuf)-sizeof(long));
		break;
#ifdef	V6UNIX
	case	SLEEP:
		/* do a sleep function - what about pwb which has alarm? */
		sleep(args[0]);
		break;
#endif
	case	GETUID:
		args[0] = getuid();
		args[1] = geteuid();
#ifdef V6UNIX
		i = args[1]<<8 | args[0];
#endif
		break;
	case	GETGID:
		args[0] = getgid();
		args[1] = getegid();
#ifdef V6UNIX
		i = args[1]<<8 | args[0];
#endif
		break;
#ifdef V6UNIX
	case	SETUID:
	case	SETGID:
		/* uids and gids are 8 bits in version 6 */
		i = syscall(code,args[0]&0377);
		break;
#endif
	case	SIG:
		/* if it is a good signal code */
		if(args[0] <= NSIG) {
			/* get the current signal value */
			i = sigvals[args[0]];
			/* reset the signal to the new value */
			sigvals[args[0]] = args[1];
			/* actually do signal except don't reset SIGILL */
			if(args[0] != SIGILL) {
				if(args[1] == (int)SIG_DFL || args[1] & (int)SIG_IGN) {
					if((int)signal(args[0],args[1]) == -1)
						i = -1;
				} else {
					if((int)signal(args[0], sigcatch) == -1)
						i = -1;
				}
			}
		}
		else i = -1;
		break;
	case	BRK:
		/* brk is successful unless we run over the stack */
		i = 0;
		if(args[0] >= regs[6]) i = -1;
		break;
#ifdef	V6UNIX
	case	PWBSYS:
		/* ignore pwbsys for now */
		switch(args[2]) {
		case	UNAME:
#ifdef	TRACE
			fprintf(stderr,"UNAME with %d %d\n",args[0],args[1]);
#endif
			strcpy(args[0],"pwbname");
			i = 0;
			break;
		case	UDATA:
#ifdef	TRACE
			fprintf(stderr,"UDATA with %d %d\n",args[0],args[1]);
#endif
			i = 0;
			break;
		case	USTAT:
fprintf(stderr,"USTAT with %d %d\n",args[0],args[1]);
			i = 0;
			break;
		case	UTIME:
fprintf(stderr,"UTIME with %d %d\n",args[0],args[1]);
			i = 0;
			break;
		default:
fprintf(stderr,"bad PWBSYS %d\n",args[3]);
			i = -1;
			break;
		}
		break;
#endif
	default:
		/*
		 *	Many sys calls are easily done here since most
		 *	system call codes are the same on version 6 and 7 UNIX
		 *	as they are here.
		 */
		i = syscall(code,args[0],args[1],args[2],args[3],args[4]);
#ifdef V6UNIX
		/* allow read write access to created files for(IDIS v6 mod) */
		if(code==CREAT) {
			/* get actual file mode after create */
			fstat(i, &stat32v);
			close(i);
			/* ensure read/write access to owner */
			chmod(args[0], 0644);
			i = open(args[0], 2);
			/* change mode back the way it was */
			chmod(args[0], stat32v.st_mode);
		}
#endif
		break;
	}
#ifdef TRACE
	fprintf(stderr," sys val -> 0%o\n",i);
#endif
	/* set carry bit if sys error */
	if(i == -1)
		psl |= CARRY;
	/* if not an indirect sys call, adjust the pc */
	if(indirflg == 0)
		pc = argp;
	/* do alternate return on one side of fork */
	if(code == FORK && i != 0)
		pc++;
	/* do the various return value formats */
	switch(sysargs[code][2]) {
	case	NORMRET:
		/* normal case only one return value in r0 */
		regs[0] = i;
		break;
	case	LONGRET:
		/* return a long in r0 - r1 as in time */
		regs[1] = i;
		regs[0] = i >> 16;
		break;
	case	TWORET:
		/* return two ints in r0 - r1 as in pipe */
		if(i == -1)
			regs[0] = i;
		else {
			regs[1] = args[1];
			regs[0] = args[0];
		}
		break;
	}
	if(i== -1)
		regs[0] = errno;
}
long longrev(l) long l; {
	/* function to reverse the halves of a long */
	union {
		long	lng;
		short	s[2];
	} u;
	register short t;
	u.lng = l;
	t = u.s[0];
	u.s[0] = u.s[1];
	u.s[1] = t;
	return(u.lng);
}
