/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_sysent.c 1.1 90/07/09$
 *
 *	@(#)ultrix_sysent.c	7.4 (Berkeley) %G%
 */

/*
 * System call switch table.
 */

#include "param.h"
#include "systm.h"

int	nosys(),notimp();

int	rexit();
int	fork();
int	read();
int	write();
int	open();
int	close();
int	ocreat();
int	link();
int	unlink();
int	chdir();
int	mknod();
int	chmod();
int	chown();
int	obreak();
int	lseek();
int	getpid();
int	getuid();
int	ptrace();
int	saccess();
int	sync();
int	kill();
int	ostat();
int	olstat();
int	dup();
int	pipe();
int	profil();
int	ultrixtobsd();
int	getgid();
int	ioctl();
int	reboot();
int	symlink();
int	readlink();
int	execve();
int	umask();
int	chroot();
int	ofstat();
int	getpagesize();
int	vfork();
int	sbrk();
int	sstk();
int	getgroups();
int	setgroups();
int	ultrixgetpgrp();
int	ultrixsetpgrp();
int	setitimer();
int	ultrixwait3();
int	getitimer();
int	gethostname();
int	sethostname();
int	getdtablesize();
int	dup2();
int	fcntl();
int	select();
int	fsync();
int	setpriority();
int	socket();
int	connect();
int	oaccept();
int	getpriority();
int	osend();
int	orecv();
int	sigreturn();
int	bind();
int	setsockopt();
int	listen();
int	ultrixsigvec();
int	osigblock();
int	osigsetmask();
int	sigsuspend();
int	osigstack();
int	orecvmsg();
int	osendmsg();
int	gettimeofday();
int	getrusage();
int	getsockopt();
int	readv();
int	writev();
int	settimeofday();
int	fchown();
int	fchmod();
int	orecvfrom();
int	osetreuid();
int	osetregid();
int	rename();
int	truncate();
int	ftruncate();
int	flock();
int	sendto();
int	shutdown();
int	socketpair();
int	mkdir();
int	rmdir();
int	utimes();
int	adjtime();
int	ogetpeername();
int	gethostid();
int	sethostid();
int	getrlimit();
int	setrlimit();
int	okillpg();
int	ogetsockname();
int	ogetdirentries();
int	ultrixgetdomainname();
int	ultrixsetdomainname();
int	ultrixgetsysinfo();

/*
 * Reserved/unimplemented system calls in the range 0-150 inclusive
 * are reserved for use in future Berkeley releases.
 * Additional system calls implemented in vendor and other
 * redistributions should be placed in the reserved range at the end
 * of the current calls.
 */
struct sysent ultrixsysent[] = {
	0, nosys,			/*   0 = out of range */
	1, rexit,			/*   1 = exit */
	0, fork,			/*   2 = fork */
	3, read,			/*   3 = read */
	3, write,			/*   4 = write */
	3, open,			/*   5 = open */
	1, close,			/*   6 = close */
	0, nosys,			/*   7 = old wait */
	2, ocreat,			/*   8 = creat */
	2, link,			/*   9 = link */
	1, unlink,			/*  10 = unlink */
	0, nosys,			/*  11 = old execv */
	1, chdir,			/*  12 = chdir */
	0, nosys,			/*  13 = old time */
	3, mknod,			/*  14 = mknod */
	2, chmod,			/*  15 = chmod */
	3, chown,			/*  16 = chown */
	1, obreak,			/*  17 = old break */
	0, nosys,			/*  18 = old stat */
	3, lseek,			/*  19 = lseek */
	0, getpid,			/*  20 = getpid */
	3, notimp,			/*  21 = mount */
	1, notimp,			/*  22 = umount */
	0, nosys,			/*  23 = old setuid */
	0, getuid,			/*  24 = getuid */
	0, nosys,			/*  25 = old stime */
	4, ptrace,			/*  26 = ptrace */
	0, nosys,			/*  27 = old alarm */
	0, nosys,			/*  28 = old fstat */
	0, nosys,			/*  29 = old pause */
	0, nosys,			/*  30 = old utime */
	0, nosys,			/*  31 = old stty */
	0, nosys,			/*  32 = old gtty */
	2, saccess,			/*  33 = access */
	0, nosys,			/*  34 = old nice */
	0, nosys,			/*  35 = old ftime */
	0, sync,			/*  36 = sync */
	2, kill,			/*  37 = kill */
	2, ostat,			/*  38 = stat */
	0, nosys,			/*  39 = old setpgrp */
	2, olstat,			/*  40 = lstat */
	1, dup,				/*  41 = dup */
	1, pipe,			/*  42 = pipe */
	0, nosys,			/*  43 = old times */
	4, profil,			/*  44 = profil */
	0, ultrixtobsd,			/*  45 = unused */
	0, nosys,			/*  46 = old setgid */
	0, getgid,			/*  47 = getgid */
	0, nosys,			/*  48 = old sigsys */
	0, nosys,			/*  49 = reserved for USG */
	0, nosys,			/*  50 = reserved for USG */
	1, notimp,			/*  51 = acct */
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
	2, ofstat,			/*  62 = fstat */
	0, nosys,			/*  63 = unused */
	0, getpagesize,			/*  64 = getpagesize */
	0, notimp,			/*  65 = mremap */
	0, vfork,			/*  66 = vfork */
	0, nosys,			/*  67 = old vread */
	0, nosys,			/*  68 = old vwrite */
	1, sbrk,			/*  69 = sbrk */
	1, sstk,			/*  70 = sstk */
	0, nosys,			/*  71 = mmap */
	0, nosys,			/*  72 = old vadvise */
	0, notimp,			/*  73 = munmap */
	0, notimp,			/*  74 = mprotect */
	0, notimp,			/*  75 = madvise */
	0, notimp,			/*  76 = vhangup */
	0, nosys,			/*  77 = old vlimit */
	0, nosys,			/*  78 = mincore */
	2, getgroups,			/*  79 = getgroups */
	2, setgroups,			/*  80 = setgroups */
	1, ultrixgetpgrp,		/*  81 = getpgrp */
	2, ultrixsetpgrp,		/*  82 = setpgrp */
	3, setitimer,			/*  83 = setitimer */
	3, ultrixwait3,			/*  84 = wait3 */
	1, notimp,			/*  85 = swapon */
	2, getitimer,			/*  86 = getitimer */
	2, gethostname,			/*  87 = gethostname */
	2, sethostname,			/*  88 = sethostname */
	0, getdtablesize,		/*  89 = getdtablesize */
	2, dup2,			/*  90 = dup2 */
	0, notimp,			/*  91 = getdopt */
	3, fcntl,			/*  92 = fcntl */
	5, select,			/*  93 = select */
	0, notimp,			/*  94 = setdopt */
	1, fsync,			/*  95 = fsync */
	3, setpriority,			/*  96 = setpriority */
	3, socket,			/*  97 = socket */
	3, connect,			/*  98 = connect */
	3, oaccept,			/*  99 = accept */
	2, getpriority,			/* 100 = getpriority */
	4, osend,			/* 101 = send */
	4, orecv,			/* 102 = recv */
	1, sigreturn,			/* 103 = sigreturn */
	3, bind,			/* 104 = bind */
	5, setsockopt,			/* 105 = setsockopt */
	2, listen,			/* 106 = listen */
	0, nosys,			/* 107 = old vtimes */
	4, ultrixsigvec,		/* 108 = sigvec */
	1, osigblock,			/* 109 = sigblock */
	1, osigsetmask,			/* 110 = sigsetmask */
	1, sigsuspend,			/* 111 = sigpause */
	2, osigstack,			/* 112 = sigstack */
	3, orecvmsg,			/* 113 = recvmsg */
	3, osendmsg,			/* 114 = sendmsg */
	0, nosys,			/* 115 = old vtrace */
	2, gettimeofday,		/* 116 = gettimeofday */
	2, getrusage,			/* 117 = getrusage */
	5, getsockopt,			/* 118 = getsockopt */
	0, nosys,			/* 119 = old resuba */
	3, readv,			/* 120 = readv */
	3, writev,			/* 121 = writev */
	2, settimeofday,		/* 122 = settimeofday */
	3, fchown,			/* 123 = fchown */
	2, fchmod,			/* 124 = fchmod */
	6, orecvfrom,			/* 125 = recvfrom */
	2, osetreuid,			/* 126 = setreuid */
	2, osetregid,			/* 127 = setregid */
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
	1, sigreturn,			/* 139 = internal (4.2 sigreturn) */
	2, adjtime,			/* 140 = adjtime */
	3, ogetpeername,		/* 141 = getpeername */
	2, gethostid,			/* 142 = gethostid */
	2, sethostid,			/* 143 = sethostid */
	2, getrlimit,			/* 144 = getrlimit */
	2, setrlimit,			/* 145 = setrlimit */
	2, okillpg,			/* 146 = killpg */
	0, nosys,			/* 147 = nosys */
	0, notimp,			/* 148 = setquota */
	0, notimp,			/* 149 = quota */
	3, ogetsockname,			/* 150 = getsockname */
	/*
	 * ULTRIX specific syscalls
	 */
	0, notimp,			/* 151 = sysmips */
	0, notimp,			/* 152 = cacheflush */
	0, notimp,			/* 153 = cachectl */
	0, notimp,			/* 154 = debug */
	0, nosys,			/* 155 = nosys */
	0, nosys,			/* 156 = nosys */
	0, nosys,			/* 157 = nosys */
	0, notimp,			/* 158 = nfs_svc */
	4, ogetdirentries,		/* 159 = getdirentries */
	0, nosys,			/* 160 = nosys */
	0, nosys,			/* 161 = nosys */
	0, nosys,			/* 162 = nosys */
	0, notimp,			/* 163 = nfs_biod */
	0, notimp,			/* 164 = nfs_getfh */
	0, ultrixgetdomainname,		/* 165 = getdomainname */
	0, ultrixsetdomainname,		/* 166 = setdomainname */
	0, nosys,			/* 167 = nosys */
	0, nosys,			/* 168 = nosys */
	0, notimp,			/* 169 = exportfs */
	0, nosys,			/* 170 = nosys */
	0, nosys,			/* 171 = nosys */
	0, notimp,			/* 172 = msgctl */
	0, notimp,			/* 173 = msgget */
	0, notimp,			/* 174 = msgrcv */
	0, notimp,			/* 175 = msgsnd */
	0, notimp,			/* 176 = semctl */
	0, notimp,			/* 177 = semget */
	0, notimp,			/* 178 = semop */
	0, notimp,			/* 179 = uname */
	0, notimp,			/* 180 = shmsys */
	0, notimp,			/* 181 = plock */
	0, notimp,			/* 182 = lockf */
	0, notimp,			/* 183 = ustat */
	0, notimp,			/* 184 = getmnt */
	0, notimp,			/* 185 = mount */
	0, notimp,			/* 186 = umount */
	0, notimp,			/* 187 = sigpending */
	0, nosys,			/* 188 = nosys */
	0, nosys,			/* 189 = nosys */
	0, nosys,			/* 190 = nosys */
	0, nosys,			/* 191 = nosys */
	0, nosys,			/* 192 = nosys */
	0, nosys,			/* 193 = nosys */
	0, nosys,			/* 194 = nosys */
	0, nosys,			/* 195 = nosys */
	0, nosys,			/* 196 = nosys */
	0, nosys,			/* 197 = nosys */
	0, nosys,			/* 198 = nosys */
	0, nosys,			/* 199 = nosys */
	0, nosys,			/* 200 = nosys */
	0, nosys,			/* 201 = nosys */
	0, nosys,			/* 202 = nosys */
	0, nosys,			/* 203 = nosys */
	0, nosys,			/* 204 = nosys */
	0, nosys,			/* 205 = nosys */
	0, nosys,			/* 206 = nosys */
	0, nosys,			/* 207 = nosys */
	0, nosys,			/* 208 = nosys */
	0, nosys,			/* 209 = nosys */
	0, nosys,			/* 210 = nosys */
	0, nosys,			/* 211 = nosys */
	0, nosys,			/* 212 = nosys */
	0, nosys,			/* 213 = nosys */
	0, nosys,			/* 214 = nosys */
	0, nosys,			/* 215 = nosys */
	0, nosys,			/* 216 = nosys */
	0, nosys,			/* 217 = nosys */
	0, nosys,			/* 218 = nosys */
	0, nosys,			/* 219 = nosys */
	0, nosys,			/* 220 = nosys */
	0, nosys,			/* 221 = nosys */
	0, nosys,			/* 222 = nosys */
	0, nosys,			/* 223 = nosys */
	0, nosys,			/* 224 = nosys */
	0, nosys,			/* 225 = nosys */
	0, nosys,			/* 226 = nosys */
	0, nosys,			/* 227 = nosys */
	0, nosys,			/* 228 = nosys */
	0, nosys,			/* 229 = nosys */
	0, nosys,			/* 230 = nosys */
	0, nosys,			/* 231 = nosys */
	0, nosys,			/* 232 = nosys */
	0, notimp,			/* 233 = nosys */
	0, notimp,			/* 234 = nosys */
	0, notimp,			/* 235 = nosys */
	0, nosys,			/* 236 = nosys */
	0, notimp,			/* 237 = nosys */
	0, notimp,			/* 238 = nosys */
	0, nosys,			/* 239 = nosys */
	0, nosys,			/* 240 = nosys */
	0, nosys,			/* 241 = nosys */
	0, nosys,			/* 242 = nosys */
	0, nosys,			/* 243 = nosys */
	0, nosys,			/* 244 = nosys */
	0, nosys,			/* 245 = nosys */
	0, nosys,			/* 246 = nosys */
	0, nosys,			/* 247 = nosys */
	0, nosys,			/* 248 = nosys */
	0, nosys,			/* 249 = nosys */
	0, nosys,			/* 250 = nosys */
	0, nosys,			/* 251 = nosys */
	0, nosys,			/* 252 = nosys */
	0, nosys,			/* 253 = nosys */
	0, nosys,			/* 254 = nosys */
	0, nosys,			/* 255 = nosys */
	0, ultrixgetsysinfo,		/* 256 = getsysinfo */
	0, notimp,			/* 257 = setsysinfo */
};
int	ultrixnsysent = sizeof(ultrixsysent) / sizeof (ultrixsysent[0]);
