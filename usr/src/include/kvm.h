/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kvm.h	5.1 (Berkeley) %G%
 */

#include <sys/kinfo.h>
#include <sys/cdefs.h>

/*
 * type byte prepending key
 */
#define	KVMDB_NLIST	1
#define	KVMDB_DEVS	2

#define	KVMDBDIR	"/var/run"

__BEGIN_DECLS
struct proc *kvm_nextproc __P((void));
struct eproc *kvm_geteproc __P((const struct proc *));
struct user *kvm_getu __P((const struct proc *));
char *kvm_getargs __P((const struct proc *, const struct user *));
char *kvm_geterr __P((void));
__END_DECLS
