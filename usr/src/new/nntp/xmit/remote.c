#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include "get_tcp_conn.h"
#include "response_codes.h"
#include "nntpxmit.h"

#define	TRUE	1
#define	FALSE	0

static	jmp_buf	SFGstack;
FILE	*rmt_rd;
FILE	*rmt_wr;
char	*sfgets();
char	*rfgets();

extern	int	errno;
extern	char	*Pname;
extern	char	Debug;
extern	char	*errmsg();

/*
** send cmd to remote, terminated with a CRLF.
*/
sendcmd(cmd)
char	*cmd;
{
	dprintf(stderr, "<<< %s\n", cmd);	/* DEBUG */
	(void) fprintf(rmt_wr, "%s\r\n", cmd);
	(void) fflush(rmt_wr);
	return(ferror(rmt_wr));
}

/*
** read a reply line from the remote server and return the code number
** as an integer, and the message in a buffer supplied by the caller.
** Returns FAIL if something went wrong.
*/
readreply(buf, size)
register char	*buf;
int	size;
{
	register char	*cp;
	register int	len;

	/*
	** make sure it's invalid, unless we say otherwise
	*/
	buf[0] = '\0';

	/*
	** read one line from the remote
	*/
	if (sfgets(buf, size, rmt_rd) == NULL)
		return(FAIL);	/* error reading from remote */

	/*
	** Make sure that what the remote sent us had a CRLF at the end
	** of the line, and then null it out.
	*/
	if ((len = strlen(buf)) > 2 && *(cp = &buf[len - 2]) == '\r' &&
		*(cp + 1) == '\n')
	{
		*cp = '\0';
	} else
		return(FAIL);	/* error reading from remote */

	dprintf(stderr, ">>> %s\n", buf);	/* DEBUG */
	/*
	** Skip any non-digits leading the response code 
	** and then convert the code from ascii to integer for
	** return from this routine.
	*/
	cp = buf;
	while(*cp != '\0' && isascii(*cp) && !isdigit(*cp))
		cp++;	/* skip anything leading */

	if (*cp == '\0' || !isascii(*cp))
		return(FAIL);	/* error reading from remote */

	return(atoi(cp));
}

/*
** send a command to the remote, and wait for a response
** returns the response code, and the message in the buffer
*/
converse(buf, size)
char	*buf;
int	size;
{
	register int	resp;

	if (sendcmd(buf))
		return(FAIL);	/* Ooops! Something went wrong in xmit */
	/*
	** Skip the silly 100 series messages, since they're not the
	** final response we can expect
	*/
	while((resp = readreply(buf, size)) >= 100 && resp < 200)
		continue;
	return(resp);
}

/*
** Contact the remote server and set up the two global FILE pointers
** to that socket.
*/
hello(host)
char	*host;
{
	int	socket0, socket1;	/* to me (bad pun) */
	static char	*service = "nntp";
	char	buf[BUFSIZ];

	switch(socket0 = get_tcp_conn(host, service)) {
	case NOHOST:
		fprintf(stderr,"%s: no such host <%s>\n", Pname, host);
		return(FAIL);
	case NOSERVICE:
		fprintf(stderr,"%s: no such service <%s>\n", Pname, service);
		return(FAIL);
	case FAIL:
		fprintf(stderr,"%s: %s: %s\n", Pname, host, errmsg(errno));
		return(FAIL);
	}

	if ((socket1 = dup(socket0)) < 0) {
		close(socket0);
		fprintf(stderr,"%s: dup(2): %s\n", Pname, errmsg(errno));
		return(FAIL);
	}

	if ((rmt_rd = fdopen(socket0, "r")) == (FILE *)NULL) {
		close(socket0);
		close(socket1);
		fprintf(stderr,"%s: fdopen(3): %s\n", Pname, errmsg(errno));
		return(FAIL);
	}

	if ((rmt_wr = fdopen(socket1, "w")) == (FILE *)NULL) {
		fclose(rmt_rd);
		rmt_rd = (FILE *)NULL;
		close(socket1);
		fprintf(stderr,"%s: fdopen(3): %s\n", Pname, errmsg(errno));
		return(FAIL);
	}

	switch(readreply(buf, sizeof(buf))) {
	case OK_CANPOST:
	case OK_NOPOST:
		if (ferror(rmt_rd)) {
			goodbye(DONT_WAIT);
			return(FAIL);
		}
		break;
	default:
		if (buf[0] != '\0')
			fprintf(stderr, "%s: %s\n", Pname, buf);
		goodbye(DONT_WAIT);
		return(FAIL);
	}
	return(NULL);
}

/*
** Say goodbye to the nice remote server.
*/
goodbye(wait)
int	wait;
{
	if (sendcmd("QUIT"))
		wait = FALSE;	/* override, something's wrong. */
	/*
	** I don't care what they say to me; this is just being polite.
	*/
	if (wait) {
		char	buf[BUFSIZ];

		(void) readreply(buf, sizeof(buf));
	}
	(void) fclose(rmt_rd);
	rmt_rd = (FILE *)NULL;
	(void) fclose(rmt_wr);
	rmt_wr = (FILE *)NULL;
}

static
to_sfgets()
{
	longjmp(SFGstack, 1);
}

/*
** `Safe' fgets, ala sendmail. This fgets will timeout after some
** period of time, on the assumption that if the remote did not
** return, they're gone.
** WARNING: contains a possibly unportable reference to stdio
** error macros.
*/
char *
sfgets(buf, size, fp)
char	*buf;
int	size;
FILE	*fp;
{
	register char	*ret;

	if (setjmp(SFGstack)) {
		alarm(0);			/* reset alarm clock */
		signal(SIGALRM, SIG_DFL);	/* reset SIGALRM */
		fp->_flag |= _IOERR;		/* set stdio error */
		return(NULL);			/* bad read, remote time out */
	}
	signal(SIGALRM, to_sfgets);
	alarm(TIMEOUT);
	ret = fgets(buf, size, fp);
	alarm(0);			/* reset alarm clock */
	signal(SIGALRM, SIG_DFL);	/* reset SIGALRM */
	return(ret);
}

/*
** Remote fgets - converts CRLF to \n, and returns NULL on `.' EOF from
** the remote. Otherwise it returns its first argument, like fgets(3).
*/
char *
rfgets(buf, size, fp)
char	*buf;
int	size;
FILE	*fp;
{
	register char	*cp = buf;
	register int	len;

	*cp = '\0';
	if (sfgets(buf, size, fp) == NULL)
		return(NULL);

	/* <CRLF> => '\n' */
	if ((len = strlen(buf)) > 2 && *(cp = &buf[len - 2]) == '\r') {
		*cp++ = '\n';
		*cp = '\0';
	}

	/* ".\n" => EOF */
	cp = buf;
	if (*cp++ == '.' && *cp == '\n') {
		return(NULL);	/* EOF */
	}

	/* Dot escaping */
	if (buf[0] == '.') strcpy(&buf[0], &buf[1]);
	return(buf);
}

/*
** send the contents of an open file descriptor to the remote,
** with appropriate RFC822 filtering (e.g. CRLF line termination,
** and dot escaping). Return FALSE if something went wrong.
*/
sendfile(fp)
FILE	*fp;
{
	register int	c;
	register int	nl = TRUE;	/* assume we start on a new line */

	if (fp == (FILE *)NULL)
		return(FALSE);

	while((c = fgetc(fp)) != EOF) {
		switch(c) {
		case '\n':
			(void) fputc('\r', rmt_wr);	/* \n -> \r\n */
			(void) fputc(c, rmt_wr);
			nl = TRUE;		/* for dot escaping */
			break;
		case '.':
			if (nl) {
				(void) fputc(c, rmt_wr);	/* add a dot */
				nl = FALSE;
			}
			(void) fputc(c, rmt_wr);
			break;
		default:
			(void) fputc(c, rmt_wr);
			nl = FALSE;
			break;
		}
	}
	if (!nl) {
		(void) fputs("\r\n", rmt_wr);
	}
	(void) fputs(".\r\n", rmt_wr);	/* <CRLF>.<CRLF> termination */
	(void) fflush(rmt_wr);
	if (ferror(fp) || ferror(rmt_wr))	/* any errors? */
		return(FALSE);
	return(TRUE);
}
