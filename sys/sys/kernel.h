/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	from: @(#)kernel.h	7.4 (Berkeley) 2/15/91
 *	$Id: kernel.h,v 1.5 1993/11/25 01:37:57 wollman Exp $
 */

#ifndef _SYS_KERNEL_H_
#define _SYS_KERNEL_H_ 1

/* Global variables for the kernel. */
long rmalloc();

/* 1.1 */
extern long hostid;
extern char hostname[MAXHOSTNAMELEN];
extern int hostnamelen;
extern char domainname[MAXHOSTNAMELEN];
extern int domainnamelen;

/* 1.2 */
extern struct timeval boottime;
extern struct timeval time;
extern struct timezone tz;	/* XXX */

extern int hz;			/* clock frequency */
extern int phz;			/* alternate clock's frequency */
extern int tick;		/* usec per clock tick */
extern int lbolt;		/* once a second sleep address */

extern fixpt_t	averunnable[3];
#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
extern double	avenrun[3];
#endif /* COMPAT_43 */

#ifdef GPROF
extern u_long s_textsize;
extern int profiling;
extern u_short *kcount;
extern char *s_lowpc;
#endif

extern const char *panicstr;	/* panic message */
extern char version[];		/* system version */
extern char copyright[];	/* system copyright */

extern int nblkdev;		/* number of entries in bdevsw */
extern int nchrdev;		/* number of entries in cdevsw */
extern int nswdev;		/* number of swap devices */
extern int nswap;		/* size of swap space */

extern int maxmem;		/* max memory per process */
extern int physmem;		/* physical memory */

extern dev_t dumpdev;		/* dump device */
extern long dumplo;		/* offset into dumpdev */

extern dev_t rootdev;		/* root device */
extern struct vnode *rootvp;	/* vnode equivalent to above */

extern dev_t swapdev;		/* swapping device */
extern struct vnode *swapdev_vp;/* vnode equivalent to above */

extern int boothowto;		/* reboot flags, from console subsystem */
#ifdef	KADB
extern char *bootesym;		/* end of symbol info from boot */
#endif

extern u_char curpri;		/* priority of current process */

#endif /* _SYS_KERNEL_H_ */
