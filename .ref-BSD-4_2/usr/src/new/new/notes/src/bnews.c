static char *sccsid = "@(#)bnews.c	1.1\t1/23/83";

/*
 * bnews.c - routines to parse the bnews header
 */

static char *SccsId = "@(#) header.c	2.9	6/5/82";

#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>
#include "parms.h"
#include "structs.h"
#include "newsgate.h"
#include "bnews.h"

char bfr[]; 
char *index();
extern time_t defexp;

/*
 * Read header from file dir/name into *hp.
 * Return (FILE *) if header okay, else NULL.
 */
FILE *hread(hp, fp)
struct hbuf *hp;
FILE *fp;
{
	register int len;

	/* clear the necessary parts of hp */
	strcpy(hp->path,"\0");
	strcpy(hp->nbuf,"\0");
	strcpy(hp->title,"\0");
	strcpy(hp->ident,"\0");
	strcpy(hp->subdate,"\0");

	if (((fgets(bfr, WDLEN, fp) != NULL &&
		*bfr >= 'A' && *bfr <= 'Z') && index(bfr, ':')) ||
		!strncmp(bfr, "From ", 5))
		if (frmread(fp, hp))
				goto strip;
	if (*bfr != PROTO) {
		return(NULL);
	}

	/* handle A news protocol */

	strncpy(hp->ident, &(bfr[1]), WDLEN);	/* file name */
	if (!nstrip(hp->ident))
		return(NULL);
	fgets(hp->nbuf, WDLEN, fp);		/* newsgroup list */
	if (!nstrip(hp->nbuf))
		return(NULL);
	ngcat(hp->nbuf);
	fgets(hp->path, WDLEN, fp);		/* source path */
	if (!nstrip(hp->path))
		return(NULL);
	fgets(hp->subdate, DATELEN, fp);	/* date */
	if (!nstrip(hp->subdate))
		return(NULL);
	fgets(hp->title, WDLEN, fp);		/* title */
	if (!nstrip(hp->title))
		return(NULL);
strip:	/* strip off sys! from front of path. */
	strcpy(bfr, SYSTEM);
	if (strncmp(bfr, hp->path, (len = strlen(bfr))) == 0 && index(NETCHRS,hp->path[len])) {
		strcpy(hp->path, &(hp->path[len+1]));
	}
	lcase(hp->nbuf);
	return(fp);
}

/*
 * Get header info from mail-format file.
 * Return non-zero on success.
 */
#include <ctype.h>
#define FROM 		1
#define NEWSGROUP 	2
#define TITLE 		3
#define SUBMIT		4
#define RECEIVE		5
#define EXPIRE		6
#define ARTICLEID	7
#define REPLYTO		8
#define FOLLOWID	9
#define CONTROL		10
#define OTHER		99

frmread(fp, hp)
register FILE *fp;
register struct hbuf *hp;
{
	int fromflag = FALSE, groupflag = FALSE, subflag = FALSE;
	int titleflag = FALSE, fileflag = FALSE, recflag = FALSE, i;
	int exprflag = FALSE, replyflag = FALSE, followflag = FALSE;
	int ctlflag = FALSE;
	int unreccnt = 0;
	/* long curpos; */
	char wordfrom[100], uname[100], at[100], site[100];

	i = type(bfr);
	do {
		/* curpos = ftell(fp); */
		switch (i) {
			case FROM:
				if (!fromflag) {
#ifdef ATSIGN
				/*
				 * This old code understood the "user at site"
				 * notation but threw away all but the first
				 * word of names (like your full name) so has
				 * been taken out.
				 */
					sscanf(bfr, "%s %s %s %s",
						wordfrom, uname, at, site);
					if (isat(at))
						sprintf(hp->path, "%s@%s",
							uname, site);
					else
						strcpy(hp->path, uname);
#else
					getfield(&fromflag, hp->path);
#endif
					fromflag = TRUE;
				}
				break;
			case NEWSGROUP:
				if (!groupflag)
					getfield(&groupflag, hp->nbuf);
				break;
			case TITLE:
				if (!titleflag)
					getfield(&titleflag, hp->title);
				break;
			case SUBMIT:
				if (!subflag)
					getfield(&subflag, hp->subdate);
				break;
			case RECEIVE:
				if (!recflag)
					getfield(&recflag, hp->recdate);
				break;
			case EXPIRE:
				if (!exprflag)
					getfield(&exprflag, hp->expdate);
				break;
			case ARTICLEID:
				if (!fileflag)
					getfield(&fileflag, hp->ident);
				break;
			case REPLYTO:
				if (!replyflag)
					getfield(&replyflag, hp->replyto);
				break;
			case FOLLOWID:
				if (!followflag)
					getfield(&followflag, hp->followid);
				break;
			case CONTROL:
				if (!ctlflag)
					getfield(&ctlflag, hp->ctlmsg);
				break;
			case OTHER:
				if (unreccnt < NUNREC) {
					strcpy(&hp->unrec[unreccnt][0], bfr);
					unreccnt++;
				}
				break;
		}
	} while ((i=type(fgets(bfr, WDLEN, fp))) > 0);


	/*
	if (*bfr != '\n')
		fseek(fp, curpos, 0);
	*/
	if (fromflag && subflag && fileflag)
		return TRUE;
	return FALSE;
}

isat(str)
char *str;
{
	if (!strcmp(str, "@")) return TRUE;
	if (!strcmp(str, "at")) return TRUE;
	if (!strcmp(str, "AT")) return TRUE;
	return FALSE;
}

getfield(flag, hpfield)
int *flag;
char *hpfield;
{
	char *ptr;

	for (ptr = index(bfr, ':'); isspace(*++ptr); )
		;
	if (*ptr != '\0')
		*flag = TRUE;
	strcpy(hpfield, ptr);
	nstrip(hpfield);
	return;
}

type(ptr)
char *ptr;
{
	char *colon, *space;

	if (!isalpha(*ptr) && strncmp(ptr, "From ", 5))
		return FALSE;
	colon = index(ptr, ':');
	space = index(ptr, ' ');
	if (!colon || colon + 1 != space)
		return FALSE;
	if (!strncmp(ptr, "From ", 5) || !strncmp(ptr, "From: ", 6) || !strncmp(ptr, "Path: ", 6))
		return FROM;
	if (!strncmp(ptr, "To: ", 4) || !strncmp(ptr, "Newsgroups: ", 12))
		return NEWSGROUP;
	if (!strncmp(ptr, "Subject: ", 9) || !strncmp(ptr, "Title: ", 7))
		return TITLE;
	if (!strncmp(ptr, "Posted: ", 8))
		return SUBMIT;
	if (!strncmp(ptr, "Received: ", 10))
		return RECEIVE;
	if (!strncmp(ptr, "Expires: ", 9))
		return EXPIRE;
	if (!strncmp(ptr, "Article-I.D.: ", 14))
		return ARTICLEID;
	if (!strncmp(ptr, "Reply-To: ", 10))
		return REPLYTO;
	if (!strncmp(ptr, "References: ", 12))
		return FOLLOWID;
	if (!strncmp(ptr, "Control: ", 9))
		return CONTROL;
	return OTHER;
}

/*
 * Write header at 'hp' on stream 'fp' in B format.
 */
hwrite(hp, fp)
register struct hbuf *hp;
register FILE *fp;
{
	ihwrite(hp, fp, 0);
}

/*
 * Same as above, except include receival date for local usage and
 * an extra \n for looks.
 */
lhwrite(hp, fp)
register struct hbuf *hp;
register FILE *fp;
{
	ihwrite(hp, fp, 1);
}

/*
 * Write header at 'hp' on stream 'fp' in B format.  Include received date
 * if wr is 1.  Leave off sysname if wr is 2.
 */
ihwrite(hp, fp, wr)
register struct hbuf *hp;
register FILE *fp;
int wr;
{
	int iu;

	ngdel(strcpy(bfr, hp->nbuf));
	if (wr == 2)
		fprintf(fp, "From: %s\nNewsgroups: %s\n", hp->path, bfr);
	else
		fprintf(fp, "From: %s!%s\nNewsgroups: %s\n", SYSTEM, hp->path, bfr);
	fprintf(fp, "Title: %s\nArticle-I.D.: %s\n", hp->title, hp->ident);
	fprintf(fp, "Posted: %s\n", hp->subdate); 
	if (wr == 1)
		fprintf(fp, "Received: %s\n", hp->recdate);
	if (*hp->expdate)
		fprintf(fp, "Expires: %s\n", hp->expdate);
	if (*hp->replyto)
		fprintf(fp, "Reply-To: %s\n", hp->replyto); 
	if (*hp->followid)
		fprintf(fp, "References: %s\n", hp->followid); 
	if (*hp->ctlmsg)
		fprintf(fp, "Control: %s\n", hp->ctlmsg);
	for (iu=0; iu<NUNREC; iu++) {
		if (hp->unrec[iu][0])
			fprintf(fp, "%s", &hp->unrec[iu][0]);
	}
	putc('\n', fp);
}
