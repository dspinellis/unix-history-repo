/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)find.h	5.3 (Berkeley) %G%
 */

/* node definition */
typedef struct _plandata {
	struct _plandata *next;			/* next node */
	int (*eval)();				/* node evaluation function */
	int flags;				/* private flags */
	int type;				/* plan node type */
	union {
		gid_t _g_data;			/* gid */
		ino_t _i_data;			/* inode */
		mode_t _m_data;			/* mode mask */
		nlink_t _l_data;		/* link count */
		off_t _o_data;			/* file size */
		time_t _t_data;			/* time value */
		uid_t _u_data;			/* uid */
		struct _plandata *_p_data[2];	/* PLAN trees */
		struct _ex {
			char **_e_argv;		/* argv array */
			char **_e_orig;		/* original strings */
			int *_e_len;		/* allocated length */
		} ex;
		char *_a_data[2];		/* array of char pointers */
		char *_c_data;			/* char pointer */
	} p_un;
#define	a_data	p_un._a_data
#define	c_data	p_un._c_data
#define	i_data	p_un._i_data
#define	g_data	p_un._g_data
#define	l_data	p_un._l_data
#define	m_data	p_un._m_data
#define	o_data	p_un._o_data
#define	p_data	p_un._p_data
#define	t_data	p_un._t_data
#define	u_data	p_un._u_data
#define	e_argv	p_un.ex._e_argv
#define	e_orig	p_un.ex._e_orig
#define	e_len	p_un.ex._e_len
} PLAN;

/* node types */
#define	T_AND		1		/* must start at >0 */
#define	T_ATIME		2
#define	T_CLOSEPAREN	3
#define	T_CTIME		4
#define	T_DEPTH		5
#define	T_EXEC		6
#define	T_EXPR		7
#define	T_FOLLOW	8
#define	T_FSTYPE	9
#define	T_GROUP		10
#define	T_INUM		11
#define	T_LINKS		12
#define	T_LS		13
#define	T_MTIME		14
#define	T_NAME		15
#define	T_NEWER		16
#define	T_NOGROUP	17
#define	T_NOT		18
#define	T_NOUSER	19
#define	T_OK    	20
#define	T_OPENPAREN	21
#define	T_OR		22
#define	T_PERM		23
#define	T_PRINT		24
#define	T_PRUNE		25
#define	T_SIZE		26
#define	T_TYPE		27
#define	T_USER		28
#define	T_XDEV		29

extern int ftsoptions;
extern int isdeprecated, isdepth, isoutput, isrelative, isstopdnx;
void *emalloc();
