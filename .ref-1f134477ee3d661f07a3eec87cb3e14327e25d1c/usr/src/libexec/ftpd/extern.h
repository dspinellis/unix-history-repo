/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

void	  blkfree __P((char **));
char	**copyblk __P((char **));
void	  cwd __P((char *));
void	  delete __P((char *));
void	  dologout __P((int));
void	  fatal __P((char *));
int	  ftpd_pclose __P((FILE *));
FILE	 *ftpd_popen __P((char *, char *));
char	**ftpglob __P((char *));
char	  *getline __P((char *, int, FILE *));
void	  logwtmp __P((char *, char *, char *));
void	  lreply __P((int, const char *, ...));
void	  makedir __P((char *));
void	  nack __P((char *));
void	  pass __P((char *));
void	  passive __P((void));
void	  perror_reply __P((int, char *));
void	  pwd __P((void));
void	  removedir __P((char *));
void	  renamecmd __P((char *, char *));
char	 *renamefrom __P((char *));
void	  reply __P((int, const char *, ...));
void	  retrieve __P((char *, char *));
void	  send_file_list __P((char *));
void	  setproctitle __P((const char *, ...));
void	  statcmd __P((void));
void	  statfilecmd __P((char *));
void	  store __P((char *, char *, int));
void	  upper __P((char *));
void	  user __P((char *));
char	 *yyerror __P((char *));
