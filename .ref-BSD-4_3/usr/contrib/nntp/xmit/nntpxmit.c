/*
** nntpxmit - transmit netnews articles across the internet with nntp
**
** This program is for transmitting netnews between sites that offer the
** NNTP service, internet style.  Ideally, there are two forms of
** transmission that can be used in this environment, since the
** communication is interactive (and relatively immediate, when compared
** with UUCP).  They are:  passive poll (what have you gotten lately?) and
** active send (I have `x', do you want it?).  The USENET as a whole
** uniformly uses active send, and where the communication is batched
** (e.g. UUCP, or electronic mail) the software sends without even asking,
** unless it can determine that the article has already been to some site.
**
** It turns out that when you implement passive poll, you have to be
** *very* careful about what you (the server) tell the client, because
** something that you might wish to restrict distribution of (either
** internal newsgroups, or material posted in the international groups in
** local distributions) will otherwise leak out.  It is the case that if
** the server doesn't tell the client that article `x' is there, the
** client won't ask for it.  If the server tells about an article which
** would otherwise stay within some restricted distribution, the onus is
** then on the client to figure out that the article is not appropriate to
** post on its local system.  Of course, at that point, we have already
** wasted the network bandwidth in transferring the article...
**
** This is a roundabout way of saying that this program only implements
** active send.  There will have to be once-over done on the NNTP spec
** before passive poll goes in, because of the problems that I have cited.
**
** Erik E. Fair	<ucbvax!fair>, Oct 14, 1985
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <syslog.h>
#include <sysexits.h>
#include "defs.h"
#include "header.h"
#include "response_codes.h"
#include "nntpxmit.h"

char	*pname;
char	Debug = FALSE;
char	Do_Stats = TRUE;
char	*USAGE = "USAGE: nntpxmit [-s] hostname:file [hostname:file ...]";
FILE	*getfp();

struct Stats {
	u_long	offered;
	u_long	accepted;
	u_long	rejected;
	u_long	failed;
} Stats = {0L, 0L, 0L, 0L};

extern	int	errno;
extern	char	*rindex();
extern	char	*index();
extern	char	*errmsg();

main(ac,av)
int	ac;
char	*av[];
{
	register int	i;
	char	*host, *file;

	pname = ((pname = rindex(av[0],'/')) ? pname + 1 : av[0]);
	
	if (ac < 2) {
		fprintf(stderr,"%s: %s\n", pname, USAGE);
		exit(EX_USAGE);
	}

	/* note that 4.2 BSD openlog has only two args */
	(void) openlog(pname, LOG_PID, LOG_LOCAL7);

	for(i = 1; i < ac; i++) {
		if (av[i][0] == '-') {
			switch(av[i][1]) {
			case 's':
				Do_Stats = FALSE;
				break;
			case 'd':
				Debug++;
				break;
			default:
				fprintf(stderr,"%s: no such option: -%c\n",
					pname, av[i][1]);
				fprintf(stderr,"%s: %s\n", pname, USAGE);
				exit(EX_USAGE);
			}
			continue;
		}

		/*
		** OK, it wasn't an option, therefore it must be a
		** hostname, filename pair.
		*/
		host = av[i];
		if ((file = index(host, ':')) != (char *)NULL) {
			*file++ = '\0';
		} else {
			fprintf(stderr,"%s: illegal hostname:file pair: <%s>\n",
				pname, host);
			continue;
		}

		bzero(&Stats, sizeof(Stats));
		if (sendnews(host, file) && Do_Stats) {
			syslog(LOG_INFO,
				"%s stats %d offered %d accepted %d rejected %d failed\n",
				host, Stats.offered, Stats.accepted, Stats.rejected, Stats.failed);
		}
	}
	exit(EX_OK);
}

/*
** Given a hostname to connect to, and a file of filenames (which contain
** netnews articles), send those articles to the named host using NNTP.
*/
sendnews(host, file)
char	*host, *file;
{
	register int	code;
	register FILE	*filefile = fopen(file, "r");
	register FILE	*fp;
	char	buf[BUFSIZ];

	/*
	** if no news to send, return
	*/
	if (filefile == (FILE *)NULL) {
		dprintf(stderr, "%s: %s: %s\n", pname, file, errmsg(errno));
		return(FALSE);
	}

	if (hello(host) == FAIL) {
		fclose(filefile);
		return(FALSE);
	}

	while((fp = getfp(filefile)) != (FILE *)NULL) {
		switch(code = ihave(fp)) {
		case CONT_XFER:
			if (!sendfile(fp)) {
				fprintf(stderr, "%s: %s: article transmission failed.\n", pname, host);
				if (Do_Stats) Stats.failed++;
				fclose(filefile);
				fclose(fp);
				goodbye(DONT_WAIT);
				return(TRUE);
			}
			fclose(fp);
			/*
			** Here I read the reply from the remote about the
			** transferred article, and I throw it away. I
			** should probably try and record the article
			** filename and append it back to the batchfile
			** again in the name of reliability, but that's
			** messy, and it's easier to assume that the guy
			** will have redundant feeds.
			*/
			code = readreply(buf, sizeof(buf));
			if (Do_Stats && code != OK_XFERED) Stats.failed++;
			break;
		case ERR_GOTIT:
			fclose(fp);
			break;
		default:
			fprintf(stderr,"%s: %s gave an improper response to IHAVE: %d\n", pname, host, code);
			fclose(filefile);
			fclose(fp);
			goodbye(DONT_WAIT);
			return(TRUE);
		}
	}
	fclose(filefile);
	if (unlink(file) < 0) {
		fprintf(stderr,"%s: unable to unlink(%s): %s\n", pname, file, errmsg(errno));
	}
	goodbye(WAIT);
	return(TRUE);
}

/*
** Read the header of a netnews article, snatch the message-id therefrom,
** and ask the remote if they have that one already.
*/
ihave(fp)
FILE	*fp;
{
	struct hbuf	header;
	char	scr[LBUFLEN];

	bzero(&header, sizeof(header));
	if (rfc822read(&header, fp, scr)) {
		register int	code;
		char	buf[BUFSIZ];

		/*
		** If an article shows up without a message-id,
		** we scream bloody murder. That's one in
		** the `can't ever happen' category.
		*/
		if (header.ident[0] == '\0') {
			fprintf(stderr, "%s: article w/o message-id!\n", pname);
			return(ERR_GOTIT);
		}
		sprintf(buf, "IHAVE %s", header.ident);
		if (Do_Stats) Stats.offered++;

		switch(code = converse(buf, sizeof(buf))) {
		case CONT_XFER:
			if (Do_Stats) Stats.accepted++;
			rewind(fp);
			return(code);
		default:
			if (Do_Stats) Stats.rejected++;
			return(code);
		}
	}
	/*
	** something botched locally with the article
	** so we don't send it, but we don't break off
	** communications with the remote either.
	*/
	return(ERR_GOTIT);
}

/*
** Given that fp points to an open file containing filenames,
** open and return a file pointer to the next filename in the file.
** Don't you love indirection?
*/
FILE *
getfp(fp)
FILE	*fp;
{
	register FILE	*newfp = NULL;
	register char	*cp;
	char	filename[BUFSIZ];

	while(newfp == NULL) {
		if (fgets(filename, sizeof(filename), fp) == NULL)
			return(NULL);		/* EOF, tell caller */
		
		/* zap \n char */
		if (*(cp = &filename[strlen(filename) - 1]) == '\n')
			*cp = '\0';

		if ((newfp = fopen(filename, "r")) == NULL)
			perror(filename);	/* tell 'em why it failed */
	}
	dprintf(stderr, "FILE: %s\n", filename);	/* DEBUG */
	return(newfp);
}
