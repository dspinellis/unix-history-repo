/* aliasbr.h - definitions for the aliasing system */
/* $Id: aliasbr.h,v 1.2 1992/05/12 22:03:31 jromine Exp $ */


extern char *AliasFile;		/* mh-alias(5) */

#define	PASSWD	"/etc/passwd"	/* passwd(5) */

#define GROUP   "/etc/group"	/* group(5) */

#define EVERYONE 200		/* lowest uid for everyone */

struct aka {
    char   *ak_name;		/* name to match against */
    struct adr *ak_addr;	/* list of addresses that it maps to */
    struct aka *ak_next;	/* next aka in list */
    char    ak_visible;		/* should be visible in headers */
};

struct adr {
    char   *ad_text;		/* text of this address in list */
    struct adr *ad_next;	/* next adr in list */
    char    ad_local;		/* text is local (check for expansion) */
};

struct home {			/* incore version of /etc/passwd */
    char   *h_name;		/* user name */
    int     h_uid;		/* user id */
    int     h_gid;		/* user's group */
    char   *h_home;		/* user's home directory */
    char   *h_shell;		/* user's shell */
#ifdef	BSD42
    int	    h_ngrps;		/* number of groups this user belongs to */
#endif /* BSD42 */
    struct home *h_next;	/* next home in list */
};

#ifndef	MMDFMTS
struct  home *seek_home ();
#endif /* MMDFMTS */

int     alias (), akvisible (), init_pw ();
char   *akresult (), *akvalue (), *akerror ();

/* codes returned by alias() */

#define	AK_OK		0	/* file parsed ok 	 */
#define	AK_NOFILE	1	/* couldn't read file 	 */
#define	AK_ERROR	2	/* error parsing file 	 */
#define	AK_LIMIT	3	/* memory limit exceeded */
#define	AK_NOGROUP	4	/* no such group 	 */


/* should live here, not in mts.c */

extern int Everyone;
extern char *NoShell;
