/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kvm.h	1.1 (Berkeley) 6/26/90
 */

#include <sys/kinfo.h>

/*
 * type byte prepending key
 */
#define	KVMDB_NLIST	1
#define	KVMDB_DEVS	2

#define	KVMDBDIR	"/var/run"

#if __STDC__ || c_plusplus
extern	struct proc *kvm_nextproc(void);
extern	struct eproc *kvm_geteproc(const struct proc *);
extern	struct user *kvm_getu(const struct proc *);
extern	char *kvm_getargs(const struct proc *, const struct user *);
extern	char *kvm_geterr(void);
#else
extern	struct proc *kvm_nextproc();
extern	struct eproc *kvm_geteproc();
extern	struct user *kvm_getu();
extern	char *kvm_getargs();
extern	char *kvm_geterr();
#endif
