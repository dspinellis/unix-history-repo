/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fsinfo.h	5.4 (Berkeley) %G%
 *
 * $Id: fsinfo.h,v 5.2.2.1 1992/02/09 15:09:51 jsp beta $
 *
 */

/*
 * Get this in now so that OS_HDR can use it
 */
#ifdef __STDC__
#define	P(x) x
#define	P_void void
#define Const const
#else
#define P(x) ()
#define P_void /* as nothing */
#define Const /* as nothing */
#endif /* __STDC__ */

#ifdef __GNUC__
#define INLINE /* __inline */
#else
#define	INLINE
#endif /* __GNUC__ */

/*
 * Pick up target dependent definitions
 */
#include "os-defaults.h"
#include OS_HDR

#ifdef VOIDP
typedef void *voidp;
#else
typedef char *voidp;
#endif /* VOIDP */

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>

/*
 * Bogosity to deal with ether { ... }
 */
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netinet/if_ether.h>

#include "fsi_data.h"

extern char* strchr P((Const char*, int)); /* C */
extern char* strrchr P((Const char*, int)); /* C */
extern char *strdup P((char*)); /* C */
extern void fatal();
extern void warning();
extern void error();
extern void analyze_automounts P((qelem*));
extern void analyze_hosts P((qelem*));
extern void compute_automount_point P((char*, host*, char*));
extern automount *new_automount P((char*));
extern auto_tree *new_auto_tree P((char*, qelem*));
extern host *new_host P((void));
extern disk_fs *new_disk_fs P((void));
extern void set_disk_fs P((disk_fs*, int, char*));
extern ether_if *new_ether_if P((void));
extern mount *new_mount P((void));
extern void set_mount P((mount*, int, char*));
extern fsmount *new_fsmount P((void));
extern void set_fsmount P((fsmount*, int, char*));
extern qelem *new_que P((void));
extern void init_que P((qelem*));
extern void ins_que P((qelem*, qelem*));
extern void rem_que P((qelem*));
extern dict *new_dict P((void));
extern dict_ent *dict_locate P((dict*, char*));
extern void dict_add P((dict*, char*, char*));
extern int dict_iter P((dict*, int (*)()));
extern void info_hdr();
extern void gen_hdr();
extern FILE *pref_open();
extern int pref_close();
extern ioloc *current_location();

extern char *disk_fs_strings[];
extern char *mount_strings[];
extern char *fsmount_strings[];
extern char *host_strings[];
extern char *ether_if_strings[];
extern char *autodir;
extern char *progname;
extern char hostname[];
extern char *username;
extern char **g_argv;
extern char *fstab_pref;
extern char *exportfs_pref;
extern char *mount_pref;
extern char *dumpset_pref;
extern char *bootparams_pref;
extern char idvbuf[];

extern int file_io_errors;
extern int parse_errors;
extern int errors;
extern int verbose;

extern dict *dict_of_hosts;
extern dict *dict_of_volnames;

extern char *xcalloc();
extern char *xmalloc();
#define	ALLOC(x)	((struct x *) xcalloc(1, sizeof(struct x)))
#define	STREQ(s,t)	(*(s) == *(t) && strcmp((s)+1,(t)+1) == 0)
#define	ISSET(m,b)	((m) & (1<<(b)))
#define	BITSET(m,b)	((m) |= (1<<(b)))

#define	FIRST(ty, q)	((ty *) ((q)->q_forw))
#define	LAST(ty, q)	((ty *) ((q)->q_back))
#define	NEXT(ty, q)	((ty *) (((qelem *) q)->q_forw))
#define	HEAD(ty, q)	((ty *) q)
#define	ITER(v, ty, q) \
	for ((v) = FIRST(ty,(q)); (v) != HEAD(ty,(q)); (v) = NEXT(ty,(v)))
