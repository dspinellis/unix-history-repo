/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)callvec.c	8.1 (Berkeley) 6/10/93
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
