/* $Id: nntp.h,v 3.0 1992/12/14 00:14:55 davison Trn $
*/ 

#ifdef USE_NNTP

bool	nntp_group _((char*));
bool	nntp_stat _((ART_NUM));
bool	nntp_header _((ART_NUM));
FILE	*nntp_body _((ART_NUM));
time_t	nntp_time _((void));
bool	nntp_newgroups _((time_t));
bool	nntp_listgroup _((void));
char	*nntp_get_a_line _((char*, int));
char	*nntp_artname _((void));
void	nntp_cleanup _((void));

#ifdef USE_XTHREAD
long	nntp_readcheck _((void));
long	nntp_read _((char*,long));
#endif

#include "nntpclient.h"

#endif /* USE_NNTP */
