/* $Header: kfile.h,v 4.3 85/05/01 11:42:00 lwall Exp $
 *
 * $Log:	kfile.h,v $
 * Revision 4.3  85/05/01  11:42:00  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#define KF_GLOBAL 0
#define KF_LOCAL 1

#ifdef KILLFILES
EXT FILE *globkfp INIT(Nullfp);		/* global article killer file */
EXT FILE *localkfp INIT(Nullfp);	/* local (for this newsgroup) */
					/*  article killer file */
#endif

void	kfile_init();
int	do_kfile();
void	kill_unwanted();
int	edit_kfile();
void	open_kfile();
void    kf_append();
void	setthru();

