/* $Header: rn.h,v 4.3 85/05/01 11:48:19 lwall Exp $
 *
 * $Log:	rn.h,v $
 * Revision 4.3  85/05/01  11:48:19  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *ngname INIT(Nullch);		/* name of current newsgroup */
EXT int ngnlen INIT(0);			/* current malloced size of ngname */
EXT char *ngdir INIT(Nullch);		/* same thing in directory name form */
EXT int ngdlen INIT(0);			/* current malloced size of ngdir */

EXT NG_NUM ng INIT(0);		/* current newsgroup index into rcline and toread */
EXT NG_NUM current_ng INIT(0);	/* stable current newsgroup so we can ditz with ng */
EXT NG_NUM starthere INIT(0);   /* set to the first newsgroup with unread news on startup */
EXT char *spool INIT(Nullch);		/* public news spool directory */

void	rn_init();
void	main();
void	set_ngname();
char	*getngdir();
