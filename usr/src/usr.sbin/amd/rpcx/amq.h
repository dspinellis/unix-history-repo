/*
 * $Id: amq.h,v 5.2 90/06/23 22:20:13 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
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
 *	@(#)amq.h	5.1 (Berkeley) 7/19/90
 */

#define AMQ_STRLEN 1024

typedef char *amq_string;
bool_t xdr_amq_string();


typedef long *time_type;
bool_t xdr_time_type();


struct amq_mount_tree {
	amq_string mt_mountinfo;
	amq_string mt_directory;
	amq_string mt_mountpoint;
	amq_string mt_type;
	time_type mt_mounttime;
	u_short mt_mountuid;
	int mt_getattr;
	int mt_lookup;
	int mt_readdir;
	int mt_readlink;
	int mt_statfs;
	struct amq_mount_tree *mt_next;
	struct amq_mount_tree *mt_child;
};
typedef struct amq_mount_tree amq_mount_tree;
bool_t xdr_amq_mount_tree();


typedef amq_mount_tree *amq_mount_tree_p;
bool_t xdr_amq_mount_tree_p();


struct amq_mount_info {
	amq_string mi_type;
	amq_string mi_mountpt;
	amq_string mi_mountinfo;
	amq_string mi_fserver;
	int mi_error;
	int mi_refc;
	int mi_up;
};
typedef struct amq_mount_info amq_mount_info;
bool_t xdr_amq_mount_info();


typedef struct {
	u_int amq_mount_info_list_len;
	amq_mount_info *amq_mount_info_list_val;
} amq_mount_info_list;
bool_t xdr_amq_mount_info_list();


typedef struct {
	u_int amq_mount_tree_list_len;
	amq_mount_tree_p *amq_mount_tree_list_val;
} amq_mount_tree_list;
bool_t xdr_amq_mount_tree_list();


struct amq_mount_stats {
	int as_drops;
	int as_stale;
	int as_mok;
	int as_merr;
	int as_uerr;
};
typedef struct amq_mount_stats amq_mount_stats;
bool_t xdr_amq_mount_stats();


enum amq_opt {
	AMOPT_DEBUG = 0,
	AMOPT_LOGFILE = 1,
	AMOPT_XLOG = 2,
	AMOPT_FLUSHMAPC = 3
};
typedef enum amq_opt amq_opt;
bool_t xdr_amq_opt();


struct amq_setopt {
	amq_opt as_opt;
	amq_string as_str;
};
typedef struct amq_setopt amq_setopt;
bool_t xdr_amq_setopt();


#define AMQ_PROGRAM ((u_long)300019)
#define AMQ_VERSION ((u_long)1)
#define AMQPROC_NULL ((u_long)0)
extern voidp amqproc_null_1();
#define AMQPROC_MNTTREE ((u_long)1)
extern amq_mount_tree_p *amqproc_mnttree_1();
#define AMQPROC_UMNT ((u_long)2)
extern voidp amqproc_umnt_1();
#define AMQPROC_STATS ((u_long)3)
extern amq_mount_stats *amqproc_stats_1();
#define AMQPROC_EXPORT ((u_long)4)
extern amq_mount_tree_list *amqproc_export_1();
#define AMQPROC_SETOPT ((u_long)5)
extern int *amqproc_setopt_1();
#define AMQPROC_GETMNTFS ((u_long)6)
extern amq_mount_info_list *amqproc_getmntfs_1();

