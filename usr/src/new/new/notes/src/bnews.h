/*
 * bnews.h - B news article header format
 */

#define NUNREC 10

/* article header */
struct	hbuf {
	char	path[WDLEN];		/* source string	*/
	char	nbuf[WDLEN];		/* newsgroup line	*/
	char	title[WDLEN];		/* title		*/
	char	ident[WDLEN];		/* article I.D.		*/
	char	replyto[WDLEN];		/* reply address	*/
	char	followid[WDLEN];	/* artid in followup to	*/
	char	subdate[DATELEN];	/* submittal date	*/
	time_t	subtime;		/* subdate in secs	*/
	char	recdate[DATELEN];	/* receival date	*/
	time_t	rectime;		/* recdate in secs	*/
	char	expdate[DATELEN];	/* expiration date	*/
	time_t	exptime;		/* expdate in secs	*/
	char	ctlmsg[WDLEN];		/* control message	*/
	char	unrec[NUNREC][WDLEN];	/* unrecognized lines	*/
};
