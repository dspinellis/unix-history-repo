/*
 * Copyright (c) 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)bug.h	5.9 (Berkeley) 6/1/90
 */

#define BUGS_HOME	"owner-bugs@ucbvax.Berkeley.EDU"
#define BUGS_ID		"bugs"

/*
 * the METOO definition has the bugfiler exit with an error (-1) status
 * if there's a problem.  This causes sendmail to send off a copy of the
 * report (as failed mail) to the "owner" of the mail alias that executed
 * the bugfiler.  This is great if you would have otherwise lost the bug
 * report.  It's not so great if you get a whole bunch of mail that you
 * really don't want.
 */
#define METOO

/* files */
#define ACK_FILE	"bug:ack"		/* acknowledge file */
#define DIST_FILE	"bug:redist"		/* redistribution file */
#define ERROR_FILE	"log"			/* error file */
#define LOCK_FILE	"bug:lock"		/* lock file name */
#define SUMMARY_FILE	"summary"		/* summary file */
#define TMP_BUG		"errors/BUG_XXXXXX"	/* tmp bug report */
#define TMP_DIR		"errors"		/* tmp directory */

#define CHN		(char *)NULL	/* null arg string */
#define COMMENT		'#'		/* comment in redist file */
#define EOS		(char)NULL	/* end of string */
#define ERR		-1		/* error return */
#define MAXLINELEN	200		/* max line length in message */
#define NO		0		/* no/false */
#define OK		0		/* okay return */
#define YES		1		/* yes/true */

typedef struct {
	short	found,			/* line number if found */
		redist;			/* if part of redist headers */
	int	(*valid)();		/* validation routine */
	short	len;			/* length of tag */
	char	*tag,			/* leading tag */
		*line;			/* actual line */
} HEADER;
extern HEADER	mailhead[];

#define DATE_TAG	0		/* "Date:" offset */
#define FROM_TAG	1		/* "From " offset */
#define CFROM_TAG	2		/* "From:" offset */
#define INDX_TAG	3		/* "Index:" offset */
#define MSG_TAG		4		/* "Message-Id:" offset */
#define RPLY_TAG	5		/* "Reply-To:" offset */
#define RET_TAG		6		/* "Return-Path:" offset */
#define SUBJ_TAG	7		/* "Subject:" offset */
#define TO_TAG		8		/* "To:" offset */
#define APPAR_TO_TAG	9		/* "Apparently-To:" offset */

/* so sizeof doesn't return 0 */
#include <sys/param.h>
#include <sys/dir.h>
extern char	bfr[MAXBSIZE],			/* general I/O buffer */
		dir[MAXNAMLEN],			/* subject and folder */
		folder[MAXNAMLEN],
		tmpname[sizeof(TMP_BUG) + 5];	/* temp bug file */
