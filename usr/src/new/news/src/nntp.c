/*
 * This software is Copyright 1987 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction or this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 * this is a file full of functions used by the various news client programs 
 */
#ifdef SCCSID
static char *SccsId = "@(#)nntp.c	2.2	10/15/87";
#endif	/* SCCSID */

#include "params.h"

static char *nntpserver;
static char ser_line[256];
static char active_file_name[512];
static char article_file_name[512];
static char last_group[256];
/*
 * open_server open a connection to the server 
 */
open_server()
{
	int response;

	/* open connection to nntpserver if appropriate */

	nntpserver = getserverbyfile(SERVER_FILE);
	if (nntpserver == NULL) {
		fprintf(stderr, "Can't get the name of the news server from %s\n",
			SERVER_FILE);
		fprintf(stderr, "Either fix this file, or put NNTPSERVER in your environment.");
		return -1;
	}
	response = server_init(nntpserver);
	if (response < 0) {
		fprintf(stderr, "Couldn't connect to %s news server, try again later.\n",
			nntpserver);
		return -1;
	}
	if (handle_server_response(response, nntpserver) < 0) {
		fprintf(stderr, "Cannot handle response from nntpserver.\n");
		return -1;
	}
	return 0;
}

/*
 * open_active gets the active file and returns an open file descriptor to
 * the calling program 
 */

FILE *
open_active()
{
	FILE *openfp;
	bzero(active_file_name, sizeof(active_file_name));
	put_server("LIST");	/* tell server we want the active file */
	(void) get_server(ser_line, sizeof(ser_line));
	if (*ser_line != CHAR_OK) {	/* and then see if that's ok */
		xerror("Can't get active file from server: \n%s\n", ser_line);
	}
	strcpy(active_file_name, "/tmp/nsact.XXXXXX");
	(void) mktemp(active_file_name);	/* make a temporary name */
	openfp = fopen(active_file_name, "w+");	/* and get ready */
	if (openfp == NULL)
		return NULL;

	while (get_server(ser_line, sizeof(ser_line)) >= 0) {	/* while */
		if (ser_line[0] == '.' && strlen(ser_line) == 1)
			/* there's another line */
			break;	/* get it and write it to */
		fputs(ser_line, openfp);
		putc('\n', openfp);
	}

	fseek(openfp, 0L, 0);	/* just get to the beginning */

	return openfp;
}

/*
 * active_name() returns the name of the temporary file that contains the
 * name of the current active file. 
 */

char *
active_name()
{
	if (active_file_name[0] == '\0')
		return NULL;
	return &active_file_name[0];
}

/*
 * set_group() set the current group returns NULL if failure "string" if
 * successful 
 */

char *
set_group(newsgroup)
char *newsgroup;
{
	char nntpbfr[256];
	if (newsgroup == NULL || *newsgroup == '\0')
		return NULL;
	if (strcmp(newsgroup, last_group)) {
		(void) sprintf(nntpbfr, "GROUP %s", newsgroup);
		put_server(nntpbfr);
		(void) get_server(ser_line, sizeof(ser_line));
		if (*ser_line != CHAR_OK)
			return NULL;
		strcpy(last_group, newsgroup);
	}
	return &ser_line[0];
}

/*
 * getarticle() returns an open file descriptor to the requested article. 
 */

FILE *
getarticle(newsgroup, number, command)
char *newsgroup, *command;
int number;
{
	FILE *fp;
	char nntpbfr[256];
	bzero(article_file_name, sizeof(article_file_name));
	if (set_group(newsgroup) == NULL)
		return NULL;
	strcpy(article_file_name, "/tmp/nsart.XXXXXX");
	if (mktemp(article_file_name) == NULL)
		return NULL;
	(void) sprintf(nntpbfr, "%s %ld", command, number);
	put_server(nntpbfr);
	(void) get_server(ser_line, sizeof(ser_line));
	if (*ser_line != CHAR_OK)
		return NULL;
	if ((fp = fopen(article_file_name, "w+")) == NULL) {
		/* and get ready */
		sync_server();
		return NULL;
	}
	while (get_server(ser_line, sizeof(ser_line)) >= 0) {	/* while */
		if (ser_line[0] == '.' && strlen(ser_line) == 1)
			/* there's another line */
			break;	/* get it and write it to */
		fputs(ser_line, fp);	/* the temp file */
		putc('\n', fp);
	}
	fseek(fp, 0L, 0);	/* just get to the beginning */
	return fp;
}
/*
 * article_name() returns the name of the temporary file that contains the
 * name of the current article file. 
 */

char *
article_name()
{
	if (article_file_name[0] == '\0')
		return NULL;
	return &article_file_name[0];
}

/*
 * group_name() returns the name of the last group accessed from nntp 
 */

char *
group_name()
{
	if (last_group[0] == '\0')
		return NULL;
	return &last_group[0];
}

/*
 * getartbyid retrieves an article by id number and returns an open file
 * descriptor for that article 
 */

FILE *
getartbyid(id)
char *id;
{
	FILE *fp;
	char nntpbfr[256];
	(void) sprintf(nntpbfr, "ARTICLE %s", id);
	put_server(nntpbfr);
	(void) get_server(ser_line, sizeof(ser_line));
	if (*ser_line != CHAR_OK) {
		fprintf(stderr, "Cannot fetch article %s\n", id);
		return NULL;
	}
	strcpy(article_file_name, "/tmp/nsart.XXXXXX");
	if (mktemp(article_file_name) == NULL)
		return NULL;
	if ((fp = fopen(article_file_name, "w+")) == NULL) {
		/* and get ready */
		sync_server();
		return NULL;
	}
	while (get_server(ser_line, sizeof(ser_line)) >= 0) {	/* while */
		if (ser_line[0] == '.' && strlen(ser_line) == 1)
			/* there's another line */
			break;	/* get it and write it to */
		fputs(ser_line, fp);	/* the temp file */
		putc('\n', fp);
	}
	fseek(fp, 0L, 0);	/* just get to the beginning */
	return fp;
}

/*
 * sync_server gobbles up the rest of the server output until it sees the .
 * on the beginning of a line by itself 
 */

void
sync_server()
{
	while (get_server(ser_line, sizeof(ser_line)) >= 0) {	/* while */
		if (ser_line[0] == '.' && strlen(ser_line) == 1)
			/* there's another line */
			break;	/* get it and throw it away */
	}

}

/*
 * strindex returns location of tx in sx 
 */
int
strindex(sx, tx)
char *sx, *tx;
{
	int i, n;
	n = strlen(tx);
	for (i = 0; sx[i] != '\0'; i++)
		if (strncmp(sx + i, tx, n) == 0)
			return i;
	return -1;
}
