/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)init_sysent.c	7.3 (Berkeley) 7/10/87
 */

/*
 * System call switch table.
 */

#include "param.h"
#include "systm.h"

int	nosys();

/* 1.1 processes and protection */
int	sethostid(),gethostid(),sethostname(),gethostname(),getpid();
int	fork(),rexit(),execv(),execve(),wait();
int	getuid(),setreuid(),getgid(),getgroups(),setregid(),setgroups();
int	getpgrp(),setpgrp();

/* 1.2 memory management */
int	sbrk(),sstk();
int	getpagesize(),smmap(),msync(),munmap(),mprotect(),madvise(),mincore();

/* 1.3 signals */
int	sigvec(),sigblock(),sigsetmask(),sigpause(),sigstack(),sigreturn();
int	kill(), killpg();

/* 1.4 timing and statistics */
int	gettimeofday(),settimeofday();
int	getitimer(),setitimer();
int 	adjtime();

/* 1.5 descriptors */
int	getdtablesize(),dup(),dup2(),close();
int	select(),getdopt(),setdopt(),fcntl(),flock();

/* 1.6 resource controls */
int	getpriority(),setpriority(),getrusage(),getrlimit(),setrlimit();
int	setquota(),qquota();

/* 1.7 system operation support */
int	umount(),smount(),swapon();
int	sync(),reboot(),sysacct();

/* 2.1 generic operations */
int	read(),write(),readv(),writev(),ioctl();

/* 2.2 file system */
int	chdir(),chroot();
int	mkdir(),rmdir();
int	creat(),open(),mknod(),unlink(),stat(),fstat(),lstat();
int	chown(),fchown(),chmod(),fchmod(),utimes();
int	link(),symlink(),readlink(),rename();
int	lseek(),truncate(),ftruncate(),saccess(),fsync();

/* 2.3 communications */
int	socket(),bind(),listen(),accept(),connect();
int	socketpair(),sendto(),send(),recvfrom(),recv();
int	sendmsg(),recvmsg(),shutdown(),setsockopt(),getsockopt();
int	getsockname(),getpeername(),pipe();

int	umask();		/* XXX */

/* 2.4 processes */
int	ptrace();

/* 2.5 terminals */

#ifdef COMPAT
/* emulations for backwards compatibility */
#define	compat(n, name)	n, o/**/name

int	owait();		/* now receive message on channel */
int	otime();		/* now use gettimeofday */
int	ostime();		/* now use settimeofday */
int	oalarm();		/* now use setitimer */
int	outime();		/* now use utimes */
int	opause();		/* now use sigpause */
int	onice();		/* now use setpriority,getpriority */
int	oftime();		/* now use gettimeofday */
int	osetpgrp();		/* ??? */
int	otimes();		/* now use getrusage */
int	ossig();		/* now use sigvec, etc */
int	ovlimit();		/* now use setrlimit,getrlimit */
int	ovtimes();		/* now use getrusage */
int	osetuid();		/* now use setreuid */
int	osetgid();		/* now use setregid */
int	ostat();		/* now use stat */
int	ofstat();		/* now use fstat */
#else
#define	compat(n, name)	0, nosys
#endif

/* BEGIN JUNK */
#ifdef vax
int	resuba();
#ifdef TRACE
int	vtrace();
#endif
#endif
int	profil();		/* 'cuz sys calls are interruptible */
int	vhangup();		/* should just do in exit() */
int	vfork();		/* awaiting fork w/ copy on write */
int	obreak();		/* awaiting new sbrk */
int	ovadvise();		/* awaiting new madvise */
/* END JUNK */

/*
 * Reserved/unimplemented system calls in the range 0-150 inclusive
 * are reserved for use in future Berkeley releases.
 * Additional system calls implemented in vendor and other
 * redistributions should be placed in the reserved range at the end
 * of the current calls.
 */
struct sysent sysent[] = {
	0, nosys,			/*   0 = indir or out-of-range */
	1, rexit,			/*   1 = exit */
	0, fork,			/*   2 = fork */
	3, read,			/*   3 = read */
	3, write,			/*   4 = write */
	3, open,			/*   5 = open */
	1, close,			/*   6 = close */
	compat(0,wait),			/*   7 = old wait */
	2, creat,			/*   8 = creat */
	2, link,			/*   9 = link */
	1, unlink,			/*  10 = unlink */
	2, execv,			/*  11 = execv */
	1, chdir,			/*  12 = chdir */
	compat(0,time),			/*  13 = old time */
	3, mknod,			/*  14 = mknod */
	2, chmod,			/*  15 = chmod */
	3, chown,			/*  16 = chown; now 3 args */
	1, obreak,			/*  17 = old break */
	compat(2,stat),			/*  18 = old stat */
	3, lseek,			/*  19 = lseek */
	0, getpid,			/*  20 = getpid */
	3, smount,			/*  21 = mount */
	1, umount,			/*  22 = umount */
	compat(1,setuid),		/*  23 = old setuid */
	0, getuid,			/*  24 = getuid */
	compat(1,stime),		/*  25 = old stime */
	4, ptrace,			/*  26 = ptrace */
	compat(1,alarm),		/*  27 = old alarm */
	compat(2,fstat),		/*  28 = old fstat */
	compat(0,pause),		/*  29 = opause */
	compat(2,utime),		/*  30 = old utime */
	0, nosys,			/*  31 = was stty */
	0, nosys,			/*  32 = was gtty */
	2, saccess,			/*  33 = access */
	compat(1,nice),			/*  34 = old nice */
	compat(1,ftime),		/*  35 = old ftime */
	0, sync,			/*  36 = sync */
	2, kill,			/*  37 = kill */
	2, stat,			/*  38 = stat */
	compat(2,setpgrp),		/*  39 = old setpgrp */
	2, lstat,			/*  40 = lstat */
	2, dup,				/*  41 = dup */
	0, pipe,			/*  42 = pipe */
	compat(1,times),		/*  43 = old times */
	4, profil,			/*  44 = profil */
	0, nosys,			/*  45 = nosys */
	compat(1,setgid),		/*  46 = old setgid */
	0, getgid,			/*  47 = getgid */
	compat(2,ssig),			/*  48 = old sig */
	0, nosys,			/*  49 = reserved for USG */
	0, nosys,			/*  50 = reserved for USG */
	1, sysacct,			/*  51 = turn acct off/on */
	0, nosys,			/*  52 = old set phys addr */
	0, nosys,			/*  53 = old lock in core */
	3, ioctl,			/*  54 = ioctl */
	1, reboot,			/*  55 = reboot */
	0, nosys,			/*  56 = old mpxchan */
	2, symlink,			/*  57 = symlink */
	3, readlink,			/*  58 = readlink */
	3, execve,			/*  59 = execve */
	1, umask,			/*  60 = umask */
	1, chroot,			/*  61 = chroot */
	2, fstat,			/*  62 = fstat */
	0, nosys,			/*  63 = reserved */
	0, getpagesize,			/*  64 = getpagesize */
	2, msync,			/*  65 = msync */
	0, vfork,			/*  66 = vfork */
	0, read,			/*  67 = old vread */
	0, write,			/*  68 = old vwrite */
	1, sbrk,			/*  69 = sbrk */
	1, sstk,			/*  70 = sstk */
	6, smmap,			/*  71 = mmap */
	1, ovadvise,			/*  72 = old vadvise */
	2, munmap,			/*  73 = munmap */
	3, mprotect,			/*  74 = mprotect */
	3, madvise,			/*  75 = madvise */
	1, vhangup,			/*  76 = vhangup */
	compat(2,vlimit),		/*  77 = old vlimit */
	3, mincore,			/*  78 = mincore */
	2, getgroups,			/*  79 = getgroups */
	2, setgroups,			/*  80 = setgroups */
	1, getpgrp,			/*  81 = getpgrp */
	2, setpgrp,			/*  82 = setpgrp */
	3, setitimer,			/*  83 = setitimer */
	0, wait,			/*  84 = wait */
	1, swapon,			/*  85 = swapon */
	2, getitimer,			/*  86 = getitimer */
	2, gethostname,			/*  87 = gethostname */
	2, sethostname,			/*  88 = sethostname */
	0, getdtablesize,		/*  89 = getdtablesize */
	2, dup2,			/*  90 = dup2 */
	2, getdopt,			/*  91 = getdopt */
	3, fcntl,			/*  92 = fcntl */
	5, select,			/*  93 = select */
	2, setdopt,			/*  94 = setdopt */
	1, fsync,			/*  95 = fsync */
	3, setpriority,			/*  96 = setpriority */
	3, socket,			/*  97 = socket */
	3, connect,			/*  98 = connect */
	3, accept,			/*  99 = accept */
	2, getpriority,			/* 100 = getpriority */
	4, send,			/* 101 = send */
	4, recv,			/* 102 = recv */
	1, sigreturn,			/* 103 = sigreturn */
	3, bind,			/* 104 = bind */
	5, setsockopt,			/* 105 = setsockopt */
	2, listen,			/* 106 = listen */
	compat(2,vtimes),		/* 107 = old vtimes */
	3, sigvec,			/* 108 = sigvec */
	1, sigblock,			/* 109 = sigblock */
	1, sigsetmask,			/* 110 = sigsetmask */
	1, sigpause,			/* 111 = sigpause */
	2, sigstack,			/* 112 = sigstack */
	3, recvmsg,			/* 113 = recvmsg */
	3, sendmsg,			/* 114 = sendmsg */
#ifdef TRACE
	2, vtrace,			/* 115 = vtrace */
#else
	0, nosys,			/* 115 = nosys */
#endif
	2, gettimeofday,		/* 116 = gettimeofday */
	2, getrusage,			/* 117 = getrusage */
	5, getsockopt,			/* 118 = getsockopt */
#ifdef vax
	1, resuba,			/* 119 = resuba */
#else
	0, nosys,			/* 119 = nosys */
#endif
	3, readv,			/* 120 = readv */
	3, writev,			/* 121 = writev */
	2, settimeofday,		/* 122 = settimeofday */
	3, fchown,			/* 123 = fchown */
	2, fchmod,			/* 124 = fchmod */
	6, recvfrom,			/* 125 = recvfrom */
	2, setreuid,			/* 126 = setreuid */
	2, setregid,			/* 127 = setregid */
	2, rename,			/* 128 = rename */
	2, truncate,			/* 129 = truncate */
	2, ftruncate,			/* 130 = ftruncate */
	2, flock,			/* 131 = flock */
	0, nosys,			/* 132 = nosys */
	6, sendto,			/* 133 = sendto */
	2, shutdown,			/* 134 = shutdown */
	5, socketpair,			/* 135 = socketpair */
	2, mkdir,			/* 136 = mkdir */
	1, rmdir,			/* 137 = rmdir */
	2, utimes,			/* 138 = utimes */
	0, nosys,			/* 139 = internal (4.2 sigreturn) */
	2, adjtime,			/* 140 = adjtime */
	3, getpeername,			/* 141 = getpeername */
	0, gethostid,			/* 142 = gethostid */
	1, sethostid,			/* 143 = sethostid */
	2, getrlimit,			/* 144 = getrlimit */
	2, setrlimit,			/* 145 = setrlimit */
	2, killpg,			/* 146 = killpg */
	0, nosys,			/* 147 = nosys */
	2, setquota,			/* 148 = quota */
	4, qquota,			/* 149 = qquota */
	3, getsockname,			/* 150 = getsockname */
	/*
	 * Syscalls 151-180 inclusive are reserved for vendor-specific
	 * system calls.  (This includes various calls added for compatibity
	 * with other Unix variants.)
	 */
};
int	nsysent = sizeof (sysent) / sizeof (sysent[0]);
