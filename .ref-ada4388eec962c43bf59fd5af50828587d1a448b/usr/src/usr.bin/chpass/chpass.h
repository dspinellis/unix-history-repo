/*
 * Copyright (c) 1988, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)chpass.h	8.4 (Berkeley) %G%
 */

struct passwd;

typedef struct _entry {
	char *prompt;
	int (*func)(), restricted, len;
	char *except, *save;
} ENTRY;

/* Field numbers. */
#define	E_BPHONE	8
#define	E_HPHONE	9
#define	E_LOCATE	10
#define	E_NAME		7
#define	E_SHELL		12

extern ENTRY list[];
extern uid_t uid;

int	 atot __P((char *, time_t *));
void	 display __P((int, struct passwd *));
void	 edit __P((struct passwd *));
char    *ok_shell __P((char *));
int	 p_change __P((char *, struct passwd *, ENTRY *));
int	 p_class __P((char *, struct passwd *, ENTRY *));
int	 p_expire __P((char *, struct passwd *, ENTRY *));
int	 p_gecos __P((char *, struct passwd *, ENTRY *));
int	 p_gid __P((char *, struct passwd *, ENTRY *));
int	 p_hdir __P((char *, struct passwd *, ENTRY *));
int	 p_login __P((char *, struct passwd *, ENTRY *));
int	 p_login __P((char *, struct passwd *, ENTRY *));
int	 p_passwd __P((char *, struct passwd *, ENTRY *));
int	 p_shell __P((char *, struct passwd *, ENTRY *));
int	 p_uid __P((char *, struct passwd *, ENTRY *));
char    *ttoa __P((time_t));
int	 verify __P((struct passwd *));
