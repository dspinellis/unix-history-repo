/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kvm.h	1.1 (Berkeley) %G%
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
