/*
 *	everything that should (or must) be declared in the main body
 *	of a notefile program. Not all of these get used in all
 *	applications, but I thought it good to put them here just in
 *	case.
 *
 *	Ray Essick	December 1981
 */

char	*myhome;
char	*mymailer;
char	*myterm;
char	*myeditor;
char	*myshell;
char	*mywrite;
char	*mypager;
int     nrows = 24;				/* number of rows on screen */
int     ncols = 80;				/* width of screen */
int     intflag = 0;				/* DEL hit recently */
char   *SYSTEM = Sysname;			/* system name */
int	msk = NOTESUMASK;
int     globuid = ANONUID;			/* his true user id */
char   *notesrc = NULL;				/* users notesrc file */
int	nindex  = NINDEX;			/* number of index lines */
struct  grp group[MAXGROUPS];
int	last_group;
