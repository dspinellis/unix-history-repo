/* $Header: intrp.h,v 4.3 85/05/01 11:41:48 lwall Exp $
 *
 * $Log:	intrp.h,v $
 * Revision 4.3  85/05/01  11:41:48  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *lib INIT(Nullch);		/* news library */
EXT char *rnlib INIT(Nullch);		/* private news program library */
EXT char *origdir INIT(Nullch);		/* cwd when rn invoked */
EXT char *homedir INIT(Nullch);		/* login directory */
EXT char *dotdir INIT(Nullch);		/* where . files go */
EXT char *logname INIT(Nullch);		/* login id */
EXT char *sitename INIT(Nullch);	/* host name */

#ifdef NEWSADMIN
    EXT char newsadmin[] INIT(NEWSADMIN);/* news administrator */
    EXT int newsuid INIT(0);
#endif

void    intrp_init();
char	*filexp();
char	*dointerp();
void	interp();
void	refscpy();
char	*getrealname();
