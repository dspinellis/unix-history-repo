/*	init_sysent.c	4.34	83/01/13	*/

/*
 * System call switch table.
 */

#include "../h/param.h"
#include "../h/systm.h"

int	nosys();

/* 1.1 processes and protection */
int	sethostid(),gethostid(),sethostname(),gethostname(),getpid();
int	fork(),rexit(),execv(),execve(),wait();
int	getuid(),setreuid(),getgid(),getgroups(),setregid(),setgroups();
int	getpgrp(),setpgrp();
/* 1.2 memory management */
int	sbrk(),sstk();
int	getpagesize(),smmap(),mremap(),munmap(),mprotect(),madvise(),mincore();

/* 1.3 signals XXX still old XXX */
int	sigvec(),sigblock(),sigsetmask(),sigpause(),sigstack();
#ifdef notdef
int	kill();
#endif
int	killpg();

/* 1.4 timing and statistics */
int	gettimeofday(),settimeofday();
int	getitimer(),setitimer();

/* 1.5 descriptors */
int	getdtablesize(),getdprop(),dup(),dup2(),close();
int	select(),getdopt(),setdopt(),wrap();

/* 1.6 resource controls */
int	getpriority(),setpriority(),getrusage(),getrlimit(),setrlimit();
#ifdef QUOTA
int	setquota(),qquota();
#endif
#ifdef MUSH
int	mu_msg();
#endif

/* 1.7 system operation support */
int	unmount();
int	smount();		/* XXX */
#ifdef notdef
int	swapon(),swapoff();
#endif
int	sync(),reboot(),sysacct(),revoke();

/* 2.1 generic operations */
int	read(),write(),readv(),writev(),ioctl();

/* 2.2 file system */
int	chdir(),chroot();
int	mkdir(),rmdir();
int	open(),mknod(),portal(),unlink(),stat(),fstat(),lstat();
int	chown(),fchown(),chmod(),fchmod(),utimes();
int	link(),symlink(),readlink(),rename();
int	lseek(),truncate(),ftruncate(),saccess(),flock(),fsync();

/* 2.3 communications */
int	socket(),bind(),listen(),accept(),connect();
int	socketpair(),sendto(),send(),recvfrom(),recv();
int	sendmsg(),recvmsg(),shutdown(),setsockopt(),getsockopt();
int	pipe();

int	ssocketaddr();		/* XXX */
int	umask();		/* XXX */

/* 2.4 processes */
int	ptrace();

/* 2.5 terminals */

/* emulations for backwards compatibility */
int	owait();		/* now receive message on channel */
int	ocreat();		/* now use 3 arg open */
int	otime();		/* now use gettimeofday */
int	obreak();		/* now use sbrk */
int	oumount();		/* now use unmount */
int	ostime();		/* now use settimeofday */
int	oalarm();		/* now use setitimer */
int	outime();		/* now use utimes */
int	ostty(),ogtty();	/* now use ioctl */
int	opause();		/* now use sigpause */
int	onice();		/* now use setpriority,getpriority */
int	oftime();		/* now use gettimeofday */
int	okill();
int	osetpgrp();		/* ??? */
int	otimes();		/* now use getrusage */
int	ossig();		/* now use sigvec, etc */
int	vfork();
int	ovadvise();		/* now use madvise */
int	ovlimit();		/* now use setlimit,getlimit */
int	oswapon();		/* now pass more information */
int	ovtimes();		/* now use getrusage */
int	osetuid();		/* now use setreuid */
int	osetgid();		/* now use setregid */
int	ostat();		/* now use stat */
int	ofstat();		/* now use fstat */

/* mpxchan: now emulated yields core dump */

/* BEGIN JUNK */
#ifdef vax
int	resuba();
#endif
int	profil();
int	vhangup();		/* should just do in exit() */
#ifdef TRACE
int	vtrace();
#endif
/* END JUNK */

struct sysent sysent[] = {
	0, nosys,			/*   0 = indir */
	1, rexit,			/*   1 = exit */
	0, fork,			/*   2 = fork */
	3, read,			/*   3 = read */
	3, write,			/*   4 = write */
	3, open,			/*   5 = open */
	1, close,			/*   6 = close */
	0, owait,			/*   7 = old wait */
	2, ocreat,			/*   8 = old creat */
	2, link,			/*   9 = link */
	1, unlink,			/*  10 = unlink */
	2, execv,			/*  11 = execv */
	1, chdir,			/*  12 = chdir */
	0, otime,			/*  13 = old time */
	3, mknod,			/*  14 = mknod */
	2, chmod,			/*  15 = chmod */
	3, chown,			/*  16 = chown; now 3 args */
	1, obreak,			/*  17 = old break */
	2, ostat,			/*  18 = old stat */
	3, lseek,			/*  19 = lseek */
	0, getpid,			/*  20 = getpid */
	3, smount,			/*  21 = mount */
	1, oumount,			/*  22 = umount */
	1, osetuid,			/*  23 = old setuid */
	0, getuid,			/*  24 = getuid */
	1, ostime,			/*  25 = old stime */
	4, ptrace,			/*  26 = ptrace */
	1, oalarm,			/*  27 = old alarm */
	2, ofstat,			/*  28 = old fstat */
	0, opause,			/*  29 = opause */
	2, outime,			/*  30 = old utime */
	2, ostty,			/*  31 = old stty */
	2, ogtty,			/*  32 = old gtty */
	2, saccess,			/*  33 = access */
	1, onice,			/*  34 = old nice */
	1, oftime,			/*  35 = old ftime */
	0, sync,			/*  36 = sync */
	2, okill,			/*  37 = old kill */
	2, stat,			/*  38 = stat */
	2, osetpgrp,			/*  39 = old setpgrp */
	2, lstat,			/*  40 = lstat */
	2, dup,				/*  41 = dup */
	0, pipe,			/*  42 = pipe */
	1, otimes,			/*  43 = old times */
	4, profil,			/*  44 = profil */
	0, nosys,			/*  45 = nosys */
	1, osetgid,			/*  46 = old setgid */
	0, getgid,			/*  47 = getgid */
	2, ossig,			/*  48 = old sig */
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
	0, nosys,			/*  63 = used internally */
	1, getpagesize,			/*  64 = getpagesize */
	5, mremap,			/*  65 = mremap */
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
	2, ovlimit,			/*  77 = old vlimit */
	3, mincore,			/*  78 = mincore */
	2, getgroups,			/*  79 = getgroups */
	2, setgroups,			/*  80 = setgroups */
	1, getpgrp,			/*  81 = getpgrp */
	2, setpgrp,			/*  82 = setpgrp */
	3, setitimer,			/*  83 = setitimer */
	0, wait,			/*  84 = wait */
	1, oswapon,			/*  85 = old swapon */
	2, getitimer,			/*  86 = getitimer */
	2, gethostname,			/*  87 = gethostname */
	2, sethostname,			/*  88 = sethostname */
	0, getdtablesize,		/*  89 = getdtablesize */
	1, dup2,			/*  90 = dup2 */
	2, getdopt,			/*  91 = getdopt */
	2, wrap,			/*  92 = wrap */
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
	2, ssocketaddr,			/* 103 = socketaddr */
	3, bind,			/* 104 = bind */
	5, setsockopt,			/* 105 = setsockopt */
	2, listen,			/* 106 = listen */
	2, ovtimes,			/* 107 = old vtimes */
	2, sigvec,			/* 108 = sigvec */
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
	3, flock,			/* 131 = flock */
	7, portal,			/* 132 = portal */
	6, sendto,			/* 133 = sendto */
	2, shutdown,			/* 134 = shutdown */
	5, socketpair,			/* 135 = socketpair */
	2, mkdir,			/* 136 = mkdir */
	1, rmdir,			/* 137 = rmdir */
	2, utimes,			/* 138 = utimes */
	2, getdprop,			/* 139 = getdprop */
	1, revoke,			/* 140 = revoke */
	2, unmount,			/* 141 = unmount */
	2, gethostid,			/* 142 = gethostid */
	2, sethostid,			/* 143 = sethostid */
	2, getrlimit,			/* 144 = getrlimit */
	2, setrlimit,			/* 145 = setrlimit */
	2, killpg,			/* 146 = killpg */
#ifdef MUSH
	3, mu_msg,			/* 147 = mu_msg */
#else
	0, nosys,			/* 147 = nosys */
#endif
#ifdef QUOTA
	2, setquota,			/* 148 = quota */
	4, qquota,			/* 149 = qquota */
#endif
};
int	nsysent = sizeof (sysent) / sizeof (sysent[0]);
