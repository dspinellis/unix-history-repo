/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)builtins.h	1.1 */

/* table of shell builtins */
/* currently can not be more than SYSMAX */

#define SYSMAX		255
/* The following commands up to SYSSPECIAL treat parameter lists specially */
/* They do not evaluate in-line with command substitution */
/* Also, commands below SYSNULL cause a script to abort on errors */
#define SYSEXEC		1
#define SYSLOGIN	2
#define SYSFC		3
#define SYSEVAL		4
#define SYSDOT 		5
#define SYSRETURN	6
#define SYSRDONLY	7
#define SYSXPORT 	8
#define SYSTYPESET	9
#define SYSNULL 	10
#define	SYSSHFT		11
#define SYSCD 		12
#define SYSSPECIAL	12	/* end of special commands */
#define SYSEXIT		13
#define SYSULIMIT	14
#define SYSCONT 	15
#define SYSBREAK 	16
#define SYSTRAP 	17
#define SYSTIMES 	18
#define SYSUMASK	19
#define SYSWAIT		20
#define SYSREAD		21
#define SYSSET		22
#define SYSUNSET 	23
#define SYSLET		24
#define SYSALIAS	25
#define SYSUNALIAS	26
#define SYSWHENCE	27
#define SYSTEST		28
#define SYSPRINT	29
#define SYSECHO		30
#define SYSPWD		31
#define SYSFG		32
#define SYSBG		33
#define	SYSJOBS		34
#define SYSKILL		35
#ifdef apollo
#define	SYSINLIB	36
#define	SYSINPROCESS	37
#endif	/* apollo */

/* structure for builtin shell variable names and aliases */
struct name_value
{
#ifdef apollo
	/* you can't readonly pointers */
	char	nv_name[12];
	char	nv_value[20];
#else
	char	*nv_name;
	char	*nv_value;
#endif	/* apollo */
};

/* The following defines are coordinated with data in msg.c */

#define	PATHNOD		(bltin_nodes)
#define PS1NOD		(bltin_nodes+1)
#define PS2NOD		(bltin_nodes+2)
#define IFSNOD		(bltin_nodes+3)
#define PWDNOD		(bltin_nodes+4)
#define HOME		(bltin_nodes+5)
#define MAILNOD		(bltin_nodes+6)
#define REPLYNOD	(bltin_nodes+7)
#define SHELLNOD	(bltin_nodes+8)
#define EDITNOD		(bltin_nodes+9)
#define MCHKNOD		(bltin_nodes+10)
#define RANDNOD		(bltin_nodes+11)
#define ENVNOD		(bltin_nodes+12)
#define HISTFILE	(bltin_nodes+13)
#define HISTSIZE	(bltin_nodes+14)
#define FCEDNOD		(bltin_nodes+15)
#define CDPNOD		(bltin_nodes+16)
#define MAILPNOD	(bltin_nodes+17)
#define PS3NOD		(bltin_nodes+18)
#define OLDPWDNOD	(bltin_nodes+19)
#define VISINOD		(bltin_nodes+20)
#define COLUMNS		(bltin_nodes+21)
#define LINES		(bltin_nodes+22)
#define PPIDNOD		(bltin_nodes+23)
#define L_ARGNOD	(bltin_nodes+24)
#define TMOUTNOD	(bltin_nodes+25)
#define SECONDS		(bltin_nodes+26)
#ifdef ACCT
#define ACCTNOD 	(bltin_nodes+27)
#define NNODES	28
#else
#define NNODES	27
#endif	/* ACCT */
#ifdef BSD
#define NALIAS	29
#else
#define NALIAS	28
#endif	/* BSD */
extern struct Namnod *bltin_nodes;
extern struct name_value node_names[];
extern struct name_value alias_names[];
#ifdef BSD
extern char	*limit_names[];
#endif	/* BSD */

