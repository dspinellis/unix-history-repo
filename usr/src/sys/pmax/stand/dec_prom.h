/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dec_prom.h	7.1 (Berkeley) %G%
 *
 * machMon.h --
 *
 *	Structures, constants and defines for access to the pmax prom.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/mach/ds3100.md/RCS/machMon.h,
 *	v 9.3 90/02/20 14:34:07 shirriff Exp $ SPRITE (Berkeley)
 */

#ifndef _DEC_PROM
#define _DEC_PROM

/*
 * This file was created based on information from the document
 * "TURBOchannel Firmware Specification" (EK-TCAAD-FS-003)
 * by Digital Equipment Corporation.
 */

#ifndef LOCORE
#include <sys/types.h>
#include <sys/cdefs.h>

/*
 * Programs loaded by the new PROMs pass the following arguments:
 *	a0	argc
 *	a1	argv
 *	a2	DEC_PROM_MAGIC
 *	a3	The callback vector defined below
 */

#define DEC_PROM_MAGIC	0x30464354

typedef struct {
	int	pagesize;	/* system page size */
	u_char	*bitmap;	/* bit for each page indicating safe to use */
} memmap;

typedef struct {
	int	revision;	/* hardware revision level */
	int	clk_period;	/* clock period in nano seconds */
	int	slot_size;	/* slot size in magabytes */
	int	io_timeout;	/* I/O timeout in cycles
	int	dma_range;	/* DMA address range in megabytes */
	int	max_dma_burst;	/* maximum DMA burst length */
	int	parity;		/* true if system module supports T.C. parity */
	int	reserved[4];
} tcinfo;

typedef int jmp_buf[12];
typedef void (*psig_t)(int);

struct callback {
	void	*(*memcpy) __P((void *s1, void *s2, int n));		/* 00 */
	void	*(*memset) __P((void *s1, int c, int n));		/* 04 */
	char	*(*strcat) __P((char *s1, char *s2));			/* 08 */
	int	(*strcmp) __P((char *s1, char *s2));			/* 0c */
	char	*(*strcpy) __P((char *s1, char *s2));			/* 10 */
	int	(*strlen) __P((char *s1));				/* 14 */
	char	*(*strncat) __P((char *s1, char *s2, int n));		/* 18 */
	char	*(*strncpy) __P((char *s1, char *s2, int n));		/* 1c */
	int	(*strncmp) __P((char *s1, char *s2, int n));		/* 20 */
	int	(*getchar) __P((void));					/* 24 */
	char	*(*gets) __P((char *s));				/* 28 */
	int	(*puts) __P((char *s));					/* 2c */
	int	(*printf) __P((char *fmt, ...));			/* 30 */
	int	(*sprintf) __P((char *s, char *fmt, ...));		/* 34 */
	int	(*io_poll) __P((void));					/* 38 */
	long	(*strtol) __P((char *s, char **endptr, int base));	/* 3c */
	psig_t	(*signal) __P((int sig, psig_t func));			/* 40 */
	int	(*raise) __P((int sig));				/* 44 */
	long	(*time) __P((long *tod));				/* 48 */
	int	(*setjmp) __P((jmp_buf env));				/* 4c */
	void	(*longjmp) __P((jmp_buf env, int value));		/* 50 */
	int	(*bootinit) __P((void));				/* 54 */
	int	(*bootread) __P((int b, void *buffer, int n));		/* 58 */
	int	(*bootwrite) __P((int b, void *buffer, int n));		/* 5c */
	int	(*setenv) __P((char *name, char *value));		/* 60 */
	char	*(*getenv) __P((char *name));				/* 64 */
	int	(*unsetenv) __P((char *name));				/* 68 */
	u_long	(*slot_address) __P((int sn));				/* 6c */
	void	(*wbflush) __P((void));					/* 70 */
	void	(*msdelay) __P((int delay));				/* 74 */
	void	(*leds) __P((int value));				/* 78 */
	void	(*clear_cache) __P((void));				/* 7c */
	int	(*getsysid) __P((void));				/* 80 */
	int	(*getbitmap) __P((memmap *map));			/* 84 */
	int	(*disableintr) __P((int sn));				/* 88 */
	int	(*enableintr) __P((int sn));				/* 8c */
	int	(*testintr) __P((int sn));				/* 90 */
	void	*reserved_data;						/* 94 */
	int	(*console_init) __P((void));				/* 98 */
	void	(*halt) __P((int *v, int cnt));				/* 9c */
	void	(*showfault) __P((void));				/* a0 */
	tcinfo	*(*gettcinfo) __P(());					/* a4 */
	int	(*execute_cmd) __P((char *cmd));			/* a8 */
	void	(*rex) __P((char cmd));					/* ac */
	/* b0 to d4 reserved */
};

extern const struct callback *callv;
extern const struct callback callvec;

/*
 * The prom routines use the following structure to hold strings.
 */
typedef struct {
	char	*argPtr[16];	/* Pointers to the strings. */
	char	strings[256];	/* Buffer for the strings. */
	char	*end;		/* Pointer to end of used buf. */
	int 	num;		/* Number of strings used. */
} MachStringTable;

#endif /* LOCORE */

/*
 * The prom has a jump table at the beginning of it to get to its
 * functions.
 */
#define DEC_PROM_JUMP_TABLE_ADDR	0xBFC00000

/*
 * Each entry in the jump table is 8 bytes - 4 for the jump and 4 for a nop.
 */
#define DEC_PROM_FUNC_ADDR(funcNum)	(DEC_PROM_JUMP_TABLE_ADDR+((funcNum)*8))

/*
 * The functions:
 *
 *	DEC_PROM_RESET		Run diags, check bootmode, reinit.
 *	DEC_PROM_EXEC		Load new program image.
 *	DEC_PROM_RESTART	Re-enter monitor command loop.
 *	DEC_PROM_REINIT		Re-init monitor, then cmd loop.
 *	DEC_PROM_REBOOT		Check bootmode, no config.
 *	DEC_PROM_AUTOBOOT	Autoboot the system.
 *
 * The following routines access PROM saio routines and may be used by
 * standalone programs that would like to use PROM I/O:
 *
 *	DEC_PROM_OPEN		Open a file.
 *	DEC_PROM_READ		Read from a file.
 *	DEC_PROM_WRITE		Write to a file.
 *	DEC_PROM_IOCTL		Iocontrol on a file.
 *	DEC_PROM_CLOSE		Close a file.
 *	DEC_PROM_LSEEK		Seek on a file.
 *	DEC_PROM_GETCHAR	Get character from console.
 *	DEC_PROM_PUTCHAR	Put character on console.
 *	DEC_PROM_SHOWCHAR	Show a char visibly.
 *	DEC_PROM_GETS		gets with editing.
 *	DEC_PROM_PUTS		Put string to console.
 *	DEC_PROM_PRINTF		Kernel style printf to console.
 *
 * The following are other prom routines:
 *	DEC_PROM_FLUSHCACHE	Flush entire cache ().
 *	DEC_PROM_CLEARCACHE	Clear I & D cache in range (addr, len).
 *	DEC_PROM_SAVEREGS	Save registers in a buffer.
 *	DEC_PROM_LOADREGS	Get register back from buffer.
 *	DEC_PROM_JUMPS8		Jump to address in s8.
 *	DEC_PROM_GETENV2	Gets a string from system environment.
 *	DEC_PROM_SETENV2	Sets a string in system environment.
 *	DEC_PROM_ATONUM		Converts ascii string to number.
 *	DEC_PROM_STRCMP		Compares strings (strcmp).
 *	DEC_PROM_STRLEN		Length of string (strlen).
 *	DEC_PROM_STRCPY		Copies string (strcpy).
 *	DEC_PROM_STRCAT		Appends string (strcat).
 *	DEC_PROM_GETCMD		Gets a command.
 *	DEC_PROM_GETNUMS	Gets numbers.
 *	DEC_PROM_ARGPARSE	Parses string to argc,argv.
 *	DEC_PROM_HELP		Help on prom commands.
 *	DEC_PROM_DUMP		Dumps memory.
 *	DEC_PROM_SETENV		Sets a string in system environment.
 *	DEC_PROM_UNSETENV	Unsets a string in system environment
 *	DEC_PROM_PRINTENV	Prints system environment
 *	DEC_PROM_JUMP2S8	Jumps to s8
 *	DEC_PROM_ENABLE		Performs prom enable command.
 *	DEC_PROM_DISABLE	Performs prom disable command.
 *	DEC_PROM_ZEROB		Zeros a system buffer.
 */
#define DEC_PROM_RESET		DEC_PROM_FUNC_ADDR(0)
#define DEC_PROM_EXEC		DEC_PROM_FUNC_ADDR(1)
#define DEC_PROM_RESTART	DEC_PROM_FUNC_ADDR(2)
#define DEC_PROM_REINIT		DEC_PROM_FUNC_ADDR(3)
#define DEC_PROM_REBOOT		DEC_PROM_FUNC_ADDR(4)
#define DEC_PROM_AUTOBOOT	DEC_PROM_FUNC_ADDR(5)
#define DEC_PROM_OPEN		DEC_PROM_FUNC_ADDR(6)
#define DEC_PROM_READ		DEC_PROM_FUNC_ADDR(7)
#define DEC_PROM_WRITE		DEC_PROM_FUNC_ADDR(8)
#define DEC_PROM_IOCTL		DEC_PROM_FUNC_ADDR(9)
#define DEC_PROM_CLOSE		DEC_PROM_FUNC_ADDR(10)
#define DEC_PROM_LSEEK		DEC_PROM_FUNC_ADDR(11)
#define DEC_PROM_GETCHAR	DEC_PROM_FUNC_ADDR(12)
#define DEC_PROM_PUTCHAR	DEC_PROM_FUNC_ADDR(13)
#define DEC_PROM_SHOWCHAR	DEC_PROM_FUNC_ADDR(14)
#define DEC_PROM_GETS		DEC_PROM_FUNC_ADDR(15)
#define DEC_PROM_PUTS		DEC_PROM_FUNC_ADDR(16)
#define DEC_PROM_PRINTF		DEC_PROM_FUNC_ADDR(17)
#define DEC_PROM_FLUSHCACHE	DEC_PROM_FUNC_ADDR(28)
#define DEC_PROM_CLEARCACHE	DEC_PROM_FUNC_ADDR(29)
#define DEC_PROM_SAVEREGS	DEC_PROM_FUNC_ADDR(30)
#define DEC_PROM_LOADREGS	DEC_PROM_FUNC_ADDR(31)
#define DEC_PROM_JUMPS8		DEC_PROM_FUNC_ADDR(32)
#define DEC_PROM_GETENV2	DEC_PROM_FUNC_ADDR(33)
#define DEC_PROM_SETENV2	DEC_PROM_FUNC_ADDR(34)
#define DEC_PROM_ATONUM		DEC_PROM_FUNC_ADDR(35)
#define DEC_PROM_STRCMP		DEC_PROM_FUNC_ADDR(36)
#define DEC_PROM_STRLEN		DEC_PROM_FUNC_ADDR(37)
#define DEC_PROM_STRCPY		DEC_PROM_FUNC_ADDR(38)
#define DEC_PROM_STRCAT		DEC_PROM_FUNC_ADDR(39)
#define DEC_PROM_GETCMD		DEC_PROM_FUNC_ADDR(40)
#define DEC_PROM_GETNUMS	DEC_PROM_FUNC_ADDR(41)
#define DEC_PROM_ARGPARSE	DEC_PROM_FUNC_ADDR(42)
#define DEC_PROM_HELP		DEC_PROM_FUNC_ADDR(43)
#define DEC_PROM_DUMP		DEC_PROM_FUNC_ADDR(44)
#define DEC_PROM_SETENV		DEC_PROM_FUNC_ADDR(45)
#define DEC_PROM_UNSETENV	DEC_PROM_FUNC_ADDR(46)
#define DEC_PROM_PRINTENV	DEC_PROM_FUNC_ADDR(47)
#define DEC_PROM_JUMP2S8	DEC_PROM_FUNC_ADDR(48)
#define DEC_PROM_ENABLE		DEC_PROM_FUNC_ADDR(49)
#define DEC_PROM_DISABLE	DEC_PROM_FUNC_ADDR(50)
#define DEC_PROM_ZEROB		DEC_PROM_FUNC_ADDR(51)

/*
 * The nonvolatile ram has a flag to indicate it is usable.
 */
#define MACH_USE_NON_VOLATILE 	((char *)0xbd0000c0)
#define MACH_NON_VOLATILE_FLAG	0x02

#endif /* _DEC_PROM */
