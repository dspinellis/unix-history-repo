#include "defs.h"
static	char sccsid[] = "@(#)optab.c 4.2 %G%";
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
#define TYPF	4	/* floating */
#define TYPD	5	/* double floating */


TYPE	struct optab	*OPTAB;
struct optab {
	char *iname;
	char val;
	char nargs;
	char argtype[6];
} optab[] = {
#define OP(a,b,c,d,e,f,g,h,i) {a,b,c,d,e,f,g,h,i}
#include "instrs"
0};

#define SYSTAB struct systab
SYSTAB {
	int	argc;
	char	*sname;
} systab[] = {
	1, "indir",
	0, "exit",
	0, "fork",
	2, "read",
	2, "write",
	2, "open",
	0, "close",
	0, "wait",
	2, "creat",
	2, "link",
	1, "unlink",
	2, "exec",
	1, "chdir",
	0, "time",
	3, "mknod",
	2, "chmod",
	2, "chown",
	1, "break",
	2, "stat",
	2, "seek",
	0, "getpid",
	3, "mount",
	1, "umount",
	0, "setuid",
	0, "getuid",
	0, "stime",
	3, "ptrace",
	0, "alarm",
	1, "fstat",
	0, "pause",
	1, "30",
	1, "stty",
	1, "gtty",
	0, "access",
	0, "nice",
	0, "sleep",
	0, "sync",
	1, "kill",
	0, "csw",
	0, "setpgrp",
	0, "tell",
	0, "dup",
	0, "pipe",
	1, "times",
	4, "profil",
	0, "tiu",
	0, "setgid",
	0, "getgid",
	2, "signal",
	0, "49",
	0, "50",
	0, "51",
	0, "52",
	0, "53",
	0, "54",
	0, "55",
	0, "56",
	0, "57",
	0, "58",
	0, "59",
	0, "60",
	0, "61",
	0, "62",
	0, "63",
};

STRING	regname[] = { "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
					"r8", "r9", "r10","r11","ap", "fp", "sp", "pc"};
STRING	fltimm[] = {
"0.5", "0.5625", "0.625", "0.6875", "0.75", "0.8125", "0.875", "0.9375",
"1.0", "1.125", "1.25", "1.375", "1.5", "1.625", "1.75", "1.875",
"2.0", "2.25", "2.5", "2.75", "3.0", "3.25", "3.5", "3.75",
"4.0", "4.5", "5.0", "5.5", "6.0", "6.5", "7.0", "7.5",
"8.0", "9.0", "10.0", "11.0", "12.0", "13.0", "14.0", "15.0",
"16.0", "18.0", "20.0", "22.0", "24.0", "26.0", "28.0", "30.0",
"32.0", "36.0", "40.0", "44.0", "48.0", "52.0", "56.0", "60.0",
"64.0", "72.0", "80.0", "88.0", "96.0", "104.0", "112.0", "120.0"
};

char *fmtr = {"%r"};
char *fmtR = {"%R"};
