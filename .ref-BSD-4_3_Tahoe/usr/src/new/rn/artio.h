/* $Header: artio.h,v 4.3 85/05/01 11:35:43 lwall Exp $
 *
 * $Log:	artio.h,v $
 * Revision 4.3  85/05/01  11:35:43  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT ART_POS artpos INIT(0);	/* byte position in article file */

EXT ART_LINE artline INIT(0);		/* current line number in article file */
EXT FILE *artfp INIT(Nullfp);		/* current article file pointer */
EXT ART_NUM openart INIT(0);		/* what is the currently open article number? */
#ifdef LINKART
    EXT char *linkartname INIT(nullstr);/* real name of article for Eunice */
#endif

void	artio_init();
FILE	*artopen();			/* open an article unless already opened */
