/*
 * header.h - Article header format
 */

/*	@(#)header.h	2.21	10/7/87	*/

#define NUNREC 50

/* article header */
struct	hbuf {
	char	from[BUFLEN];		/* From:		*/
	char	path[PATHLEN];		/* Path:		*/
	char	nbuf[LBUFLEN];		/* Newsgroups:		*/
	char	title[BUFLEN];		/* Subject:		*/
	char	ident[BUFLEN];		/* Message-ID:		*/
	char	replyto[BUFLEN];	/* Reply-To:		*/
	char	followid[BUFLEN];	/* References:		*/
	char	subdate[DATELEN];	/* Date: (submission)	*/
	time_t	subtime;		/* subdate in secs	*/
	char	expdate[DATELEN];	/* Expires:		*/
	char	ctlmsg[PATHLEN];	/* Control:		*/
	char	sender[BUFLEN];		/* Sender:		*/
	char	followto[BUFLEN];	/* Followup-to:		*/
	char	distribution[BUFLEN];	/* Distribution:	*/
	char	organization[BUFLEN];	/* Organization:	*/
	char	numlines[8];		/* Lines:		*/
	int	intnumlines;		/* Integer version	*/
	char	keywords[BUFLEN];	/* Keywords:		*/
	char	summary[BUFLEN];	/* Summary:		*/
	char	approved[BUFLEN];	/* Approved:		*/
	char	nf_id[BUFLEN];		/* Nf-ID:		*/
	char	nf_from[BUFLEN];	/* Nf-From:		*/
	char 	supersedes[BUFLEN];	/* Supersedes:		*/
#ifdef DOXREFS
	char 	xref[BUFLEN];		/* Xref:		*/
#endif /* DOXREFS */
	char	*unrec[NUNREC];		/* unrecognized lines	*/
};

#define hwrite(hp,fp)	ihwrite(hp,fp,0)
#define lhwrite(hp,fp)	ihwrite(hp,fp,1)

char *oident();
