/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)krb.h	5.1 (Berkeley) %G%
 */

/*
 * XXX
 * These should be in a kerberos include file.
 */
void	 des_clear_key __P(());
int	 des_read __P((int, char *, int));
void	 des_set_key __P((C_Block, Key_schedule));
int	 des_write __P((int, char *, int));
int	 krb_net_read __P((int, char *, int));
char	*krb_realmofhost __P((char *));
int	 krb_sendauth __P((long, int, KTEXT, char *, char *, char *,
	    u_long, MSG_DAT *, CREDENTIALS *, Key_schedule,
	    struct sockaddr_in *, struct sockaddr_in *, char *));
int	 krcmd __P((char **, u_short, char *, char *, int *, char *));
int	 krcmd_mutual __P((char **, u_short, char *, char *, int *,
	    char *, CREDENTIALS *, Key_schedule));
