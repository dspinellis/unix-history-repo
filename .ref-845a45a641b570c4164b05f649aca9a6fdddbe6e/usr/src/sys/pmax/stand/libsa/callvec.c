/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)callvec.c	7.1 (Berkeley) %G%
 */

#include <pmax/stand/dec_prom.h>

const struct callback callvec = {
	(void *(*) __P((void *s1, void *s2, int n)))0,
	(void *(*) __P((void *s1, int c, int n)))0,
	(char *(*) __P((char *s1, char *s2)))DEC_PROM_STRCAT,
	(int (*) __P((char *s1, char *s2)))DEC_PROM_STRCMP,
	(char *(*) __P((char *s1, char *s2)))DEC_PROM_STRCPY,
	(int (*) __P((char *s1)))DEC_PROM_STRLEN,
	(char *(*) __P((char *s1, char *s2, int n)))0,
	(char *(*) __P((char *s1, char *s2, int n)))0,
	(int (*) __P((char *s1, char *s2, int n)))0,
	(int (*) __P((void)))DEC_PROM_GETCHAR,
	(char *(*) __P((char *s)))DEC_PROM_GETS,
	(int (*) __P((char *s)))DEC_PROM_PUTS,
	(int (*) __P((char *fmt, ...)))DEC_PROM_PRINTF,
	(int (*) __P((char *s, char *fmt, ...)))0,
	(int (*) __P((void)))0,
	(long (*) __P((char *s, char **endptr, int base)))0,
	(psig_t (*) __P((int sig, psig_t func)))0,
	(int (*) __P((int sig)))0,
	(long (*) __P((long *tod)))0,
	(int (*) __P((jmp_buf env)))0,
	(void (*) __P((jmp_buf env, int value)))0,
	(int (*) __P((void)))0,
	(int (*) __P((int b, void *buffer, int n)))0,
	(int (*) __P((int b, void *buffer, int n)))0,
	(int (*) __P((char *name, char *value)))DEC_PROM_SETENV2,
	(char *(*) __P((char *name)))DEC_PROM_GETENV2,
	(int (*) __P((char *name)))DEC_PROM_UNSETENV,
	(u_long (*) __P((int sn)))0,
	(void (*) __P((void)))0,
	(void (*) __P((int delay)))0,
	(void (*) __P((int value)))0,
	(void (*) __P((void)))0,
	(int (*) __P((void)))0,
	(int (*) __P((memmap *map)))0,
	(int (*) __P((int sn)))0,
	(int (*) __P((int sn)))0,
	(int (*) __P((int sn)))0,
	(void *)0,
	(int (*) __P((void)))0,
	(void (*) __P((int *v, int cnt)))0,
	(void (*) __P((void)))0,
	(tcinfo *(*) __P(()))0,
	(int (*) __P((char *cmd)))0,
	(void (*) __P((char cmd)))0,
};
