/* $Id: nntp.c,v 3.0 1991/11/22 04:12:21 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "init.h"
#include "trn.h"
#include "ngdata.h"
#include "rcln.h"
#include "cache.h"
#include "bits.h"
#include "head.h"
#include "final.h"
#include "nntp.h"

#ifdef USE_NNTP

/* try to access the specified group */

bool
nntp_group(group)
char *group;
{
    sprintf(ser_line, "GROUP %s", group);
    nntp_command(ser_line);
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	int ser_int = atoi(ser_line);
	if (ser_int != NNTP_NOSUCHGROUP_VAL
	 && ser_int != NNTP_SYNTAX_VAL) {
	    if (ser_int != NNTP_AUTH_NEEDED_VAL && ser_int != NNTP_ACCESS_VAL
	     && ser_int != NNTP_AUTH_REJECT_VAL) {
		fprintf(stderr, "\nServer's response to GROUP %s:\n%s\n",
			group, ser_line);
		finalize(1);
	    }
	}
	return FALSE;
    }
    return TRUE;
}

/* check on an article's existence */

bool
nntp_stat(artnum)
ART_NUM artnum;
{
    sprintf(ser_line, "STAT %ld", (long)artnum);
    nntp_command(ser_line);		/* ask the server for the header */
    return (nntp_check(TRUE) == NNTP_CLASS_OK);
}

/* prepare to get the header */

bool
nntp_header(artnum)
ART_NUM artnum;
{
    sprintf(ser_line, "HEAD %ld", (long)artnum);
    nntp_command(ser_line);		/* ask the server for the header */
    return (nntp_check(TRUE) == NNTP_CLASS_OK);
}

/* copy the body of an article to a temporary file */

FILE *
nntp_body(artnum)
ART_NUM artnum;
{
    char *artname;
    FILE *fp;

    if (!parseheader(artnum))
	return Nullfp;
    artname = nntp_artname();
    if (!(fp = fopen(artname, "w+"))) {
	fprintf(stderr, "\nUnable to write temporary file: '%s'.\n",
		artname);
	finalize(1);
    }
    sprintf(ser_line, "BODY %ld", (long)artnum);
    nntp_command(ser_line);		/* ask the server for the article */
    if (nntp_check(TRUE) != NNTP_CLASS_OK) {	/* and get it's reaction */
	fclose(fp);
	errno = ENOENT;			/* Simulate file-not-found */
	return Nullfp;
    }
    fwrite(headbuf, 1, strlen(headbuf), fp);
    for (;;) {
	nntp_gets(ser_line, sizeof ser_line);
	if (ser_line[0] == '.' && ser_line[1] == '\0')
	    break;
	fputs((ser_line[0] == '.' ? ser_line + 1 : ser_line), fp);
	putc('\n', fp);
    }
    fseek(fp, 0L, 0);
    return fp;
}

/* This is a 1-relative list */
static int maxdays[] = { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

time_t
nntp_time()
{
    char *s;
    int year, month, day, hh, mm, ss;
#ifdef DEBUG
    struct tm *ts;
    char ch;
#endif

    nntp_command("DATE");
    if (nntp_check(FALSE) != NNTP_CLASS_INF)
	return time((time_t*)NULL);

    s = ser_line + strlen(ser_line) - 1;

    ss = (*s - '0') + (*--s - '0') * 10;
    mm = (*--s - '0') + (*--s - '0') * 10;
    hh = (*--s - '0') + (*--s - '0') * 10;
    day = (*--s - '0') + (*--s - '0') * 10;
    month = (*--s - '0') + (*--s - '0') * 10;
#ifdef DEBUG
    ch = *s;
#endif
    *s = '\0';
    year = atoi(s-4);
#ifdef DEBUG
    *s = ch;
#endif

    /* This simple algorithm will be valid until the year 2400 */
    if (year % 4)
	maxdays[2] = 28;
    else
	maxdays[2] = 29;
    if (month < 1 || month > 12 || day < 1 || day > maxdays[month]
     || hh < 0 || hh > 23 || mm < 0 || mm > 59
     || ss < 0 || ss > 59)
	return time((time_t*)NULL);

    for (month--; month; month--)
	day += maxdays[month];

    ss = ((((year-1970) * 365 + (year-1968)/4 + day - 1) * 24L + hh) * 60
	  + mm) * 60 + ss;

#ifdef DEBUG
    ts = gmtime(&ss);
    sprintf(buf,"19%02d%02d%02d%02d%02d%02d",
	    ts->tm_year % 100, ts->tm_mon+1, ts->tm_mday,
	    ts->tm_hour, ts->tm_min, ts->tm_sec) FLUSH;
    if (strNE(ser_line+4,buf))
	printf("\n** Tell Wayne:  %s != %s **\n",ser_line+4,buf);
#endif
    return ss;
}

bool
nntp_newgroups(t)
time_t t;
{
    struct tm *ts;

    ts = gmtime(&t);
    sprintf(ser_line, "NEWGROUPS %02d%02d%02d %02d%02d%02d GMT",
	ts->tm_year % 100, ts->tm_mon+1, ts->tm_mday,
	ts->tm_hour, ts->tm_min, ts->tm_sec);
    nntp_command(ser_line);
    return (nntp_check(TRUE) == NNTP_CLASS_OK);
}

bool
nntp_listgroup()
{
    static bool listgroup_works = TRUE;

    if (!listgroup_works)
	return FALSE;
    nntp_command("LISTGROUP");
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	listgroup_works = FALSE;
	return FALSE;
    }
    return TRUE;
}

/* similar to nntp_gets, but will make the buffer bigger if necessary */

char *
nntp_get_a_line(original_buffer,buffer_length)
char *original_buffer;
register int buffer_length;
{
    register int bufix = 0;
    register int nextch;
    register char *some_buffer_or_other = original_buffer;

    do {
	if (bufix >= buffer_length) {
	    buffer_length *= 2;
	    if (some_buffer_or_other == original_buffer) {
					/* currently static? */
		some_buffer_or_other = safemalloc((MEM_SIZE)buffer_length+1);
		strncpy(some_buffer_or_other,original_buffer,buffer_length/2);
					/* so we must copy it */
	    }
	    else {			/* just grow in place, if possible */
		some_buffer_or_other = saferealloc(some_buffer_or_other,
		    (MEM_SIZE)buffer_length+1);
	    }
	}
	if ((nextch = getc(ser_rd_fp)) == EOF)
	    return Nullch;
	some_buffer_or_other[bufix++] = (char) nextch;
    } while (nextch && nextch != '\n');
    some_buffer_or_other[bufix] = '\0';
    len_last_line_got = bufix;
    buflen_last_line_got = buffer_length;
    return some_buffer_or_other;
}

char *
nntp_artname()
{
    static char artname[20];
    sprintf(artname,"rrn.%ld",our_pid);
    return artname;
}

/* cleanup the odds and ends associated with NNTP usage */

void
nntp_cleanup()
{
    UNLINK(nntp_artname());
    if (*active_name)
	UNLINK(active_name);
    nntp_close();
}

#ifdef USE_XTHREAD

static long rawbytes = -1;	/* bytes remaining to be transfered */

/* nntp_readcheck -- get a line of text from the server, interpreting
** it as a status message for a binary command.  Call this once
** before calling nntp_read() for the actual data transfer.
*/
long
nntp_readcheck()
{
    /* try to get the status line and the status code */
    if (nntp_check(FALSE) != NNTP_CLASS_OK)
	return rawbytes = -1;

    /* try to get the number of bytes being transfered */
    if (sscanf(ser_line, "%*d%ld", &rawbytes) != 1)
	return rawbytes = -1;
    return rawbytes;
}

/* nntp_read -- read data from the server in binary format.  This call must
** be preceeded by an appropriate binary command and an nntp_readcheck call.
*/
long
nntp_read(buf, n)
char *buf;
long n;
{
    /* if no bytes to read, then just return EOF */
    if (rawbytes < 0)
	return 0;

#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif

    /* try to read some data from the server */
    if (rawbytes) {
	n = fread(buf, 1, n > rawbytes ? rawbytes : n, ser_rd_fp);
	rawbytes -= n;
    } else
	n = 0;

    /* if no more left, then fetch the end-of-command signature */
    if (!rawbytes) {
	char buf[5];	/* "\r\n.\r\n" */

	fread(buf, 1, 5, ser_rd_fp);
	rawbytes = -1;
    }
#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    return n;
}
#endif /* USE_XTHREAD */

#endif /* USE_NNTP */
