/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ops.tahoe.c	5.5 (Berkeley) %G%";
#endif not lint

/*
 * Machine operators.
 */

#include "defs.h"
#include "ops.h"

#ifndef public
typedef unsigned char Opcode;

/*
 * Opcode definitions.
 */

/*
 * Argument access types
 */
#define ACCA	(8<<3)	/* address only */
#define ACCR	(1<<3)	/* read */
#define ACCW	(2<<3)	/* write */
#define ACCM	(3<<3)	/* modify */
#define ACCB	(4<<3)	/* branch displacement */
#define ACCI	(5<<3)	/* XFC code */

/*
 * Argument data types
 */
#define TYPB	0	/* byte */
#define TYPW	1	/* word */
#define TYPL	2	/* long */
#define TYPQ	3	/* quad */
#define	TYPF	4	/* float */
#define	TYPD	5	/* double */

/*
 * Addressing modes.
 */
#define LITSHORT    0x0	/* short literals */
#define LITUPTO31   0x1
#define LITUPTO47   0x2
#define LITUPTO63   0x3
#define INDEX       0x4 /* i[r] */
#define REG	    0x5 /* r */
#define REGDEF      0x6 /* (r) */
#define AUTODEC     0x7 /* -(r) */
#define AUTOINC     0x8 /* (r)+ */
#define AUTOINCDEF  0x9 /* *(r)+ */
#define BYTEDISP    0xA /* BD(r) */
#define BYTEDISPDEF 0xB /* *BD(r) */
#define WORDDISP    0xC /* WD(r) */
#define WORDDISPDEF 0xD /* *WD(r) */
#define LONGDISP    0xE /* LD(r) */
#define LONGDISPDEF 0xF /* *LD(r) */

#define is_branch_disp(arg) ((arg & ACCB) != 0)
#define typelen(arg)        (arg & 07)
#define regnm(mode)	    (mode & 0xF)
#define addrmode(mode)      (mode >> 4)

/*
 * Certain opcodes values are used either in calculating
 * the next address a process will go to, or for other
 * random reasons -- these are defined here.
 */
#define	O_AOBLEQ	0x3f
#define	O_AOBLSS	0x2f
#define	O_BBC		0x1e
#define	O_BBS		0x0e
#define	O_BBSSI		0x5f
#define	O_BCC		0xf1
#define	O_BCS		0xe1
#define	O_BEQL		0x31
#define	O_BGEQ		0x81
#define	O_BGEQU		0xe1
#define	O_BGTR		0x41
#define	O_BGTRU		0xa1
#define	O_BLEQ		0x51
#define	O_BLEQU		0xb1
#define	O_BLSS		0x91
#define	O_BLSSU		0xf1
#define	O_BNEQ		0x21
#define	O_BPT		0x30
#define	O_BRB		0x11
#define	O_BRW		0x13
#define	O_BTCS		0xce
#define	O_BVC		0xc1
#define	O_BVS		0xd1
#define	O_CALLF		0xfe
#define	O_CALLS		0xbf
#define	O_CASEL		0xfc
#define	O_JMP		0x71
#define	O_KCALL		0xcf
#define	O_RET		0x40

/*
 * Operator information structure.
 */
typedef struct {
    char *iname;
    char val;
    char numargs;
    char argtype[6];
} Optab;

#define	SYSSIZE	151		/* # of system calls */
#endif

#ifndef ADBINSTRS
#define ADBINSTRS "../../bin/adb/adb.tahoe/instrs.adb"
#endif

public Optab optab[] = {
#define OP(a,b,c,d,e,f,g,h,i) {a,b,c,d,e,f,g,h,i}
#include ADBINSTRS
0};

/*
 * Register names.
 */

public String regname[] = {
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
    "r8", "r9", "r10","r11","r12", "fp", "sp", "pc"
};

public String systab[SYSSIZE] = {
	"indir",	"exit",		"fork",		"read",
	"write",	"open",		"close",	"owait",
	"creat",	"link",		"unlink",	"execv",
	"chdir",	"otime",	"mknod",	"chmod",
	"chown",	"obreak",	"ostat",	"lseek",
	"getpid",	"mount",	"umount",	"osetuid",
	"getuid",	"ostime",	"ptrace",	"oalarm",
	"ofstat",	"opause",	"outime",	"ostty",
	"ogtty",	"access",	"onice",	"oftime",
	"sync",		"kill",		"stat",		"osetpgrp",
	"lstat",	"dup",		"pipe",		"otimes",
	"profil",	0,		"osetgid",	"getgid",
	"osig",		0,		0,		"acct",
	"ophys",	"olock",	"ioctl",	"reboot",
	"ompxchan",	"symlink",	"readlink",	"execve",
	"umask",	"chroot",	"fstat",	0,
	"getpagesize",	"mremap",	"vfork",	"ovread",
	"ovwrite",	"sbrk",		"sstk",		"mmap",
	"ovadvise",	"munmap",	"mprotect",	"madvise",
	"vhangup",	"ovlimit",	"mincore",	"getgroups",
	"setgroups",	"getpgrp",	"setpgrp",	"setitimer",
	"wait",		"swapon",	"getitimer",	"gethostname",
	"sethostname",	"getdtablesize","dup2",		"getdopt",
	"fcntl",	"select",	"setdopt",	"fsync",
	"setpriority",	"socket",	"connect",	"accept",
	"getpriority",	"send",		"recv",		"osocketaddr",
	"bind",		"setsockopt",	"listen",	"ovtimes",
	"sigvec",	"sigblock",	"sigsetmask",	"sigpause",
	"sigstack",	"recvmsg",	"sendmsg",	"vtrace",
	"gettimeofday",	"getrusage",	"getsockopt",	"resuba",
	"readv",	"writev",	"settimeofday",	"fchown",
	"fchmod",	"recvfrom",	"setreuid",	"setregid",
	"rename",	"truncate",	"ftruncate",	"flock",
	0,		"sendto",	"shutdown",	"socketpair",
	"mkdir",	"rmdir",	"utimes",	0,
	0,		"getpeername",	"gethostid",	"sethostid",
	"getrlimit",	"setrlimit",	"killpg",	0,
	"quota",	"qquota",	"getsockname",
};
