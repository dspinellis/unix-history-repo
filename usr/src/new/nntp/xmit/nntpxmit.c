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
**
** Changed to exit on unusual errors in opening articles, and now produces
** CPU usage statistics to syslog.
**
** Erik E. Fair <ucbvax!fair>, April 26, 1986
*/

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/file.h>
#include <syslog.h>
#include <sysexits.h>
#include "defs.h"
#include "header.h"
#include "response_codes.h"
#include "nntpxmit.h"

char	*Pname;
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

struct tms	Prev_times, Cur_times;
time_t		Tbegin, Tend;

extern	int	errno;
extern	char	*rindex();
extern	char	*index();
extern	char	*errmsg();
extern	char	*sp_strip();
extern	int	msgid_ok();

main(ac,av)
int	ac;
char	*av[];
{
	register int	i;
	char	*host, *file;

	(void) time(&Tbegin);

	Pname = ((Pname = rindex(av[0],'/')) ? Pname + 1 : av[0]);
	
	if (ac < 2) {
		fprintf(stderr,"%s: %s\n", Pname, USAGE);
		exit(EX_USAGE);
	}

	/* note that 4.2 BSD openlog has only two args */
#ifdef LOG_LOCAL7
	(void) openlog(Pname, LOG_PID, LOG_LOCAL7);
#else
	(void) openlog(Pname, LOG_PID);
#endif

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
					Pname, av[i][1]);
				fprintf(stderr,"%s: %s\n", Pname, USAGE);
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
				Pname, host);
			continue;
		}

		bzero(&Stats, sizeof(Stats));
		if (sendnews(host, file) && Do_Stats) {
			struct tms	delta;
			time_t		elapsed;
			syslog(LOG_INFO,
				"%s stats %lu offered %lu accepted %lu rejected %lu failed\n",
				host, Stats.offered, Stats.accepted, Stats.rejected, Stats.failed);

			click(&delta, &elapsed);

			syslog(LOG_INFO, "%s xmit user %lu system %lu elapsed %lu\n",
				host, delta.tms_utime, delta.tms_stime, elapsed);
		}
	}
	exit(EX_OK);
}

/*
** Calculate how much time we've used.
**
** The HZ constant is from times(3C) man page, and is probably wrong
** for anything other than a VAX.
**
** Why `click'? Well, imagine that I've got a stopwatch in my hand...
*/
#define	HZ	60L

click(cpu, elapsed)
register struct tms	*cpu;
time_t	*elapsed;
{
	(void) times(&Cur_times);
	(void) time(&Tend);

	/* delta T */
	*elapsed = Tend - Tbegin;
	cpu->tms_utime = Cur_times.tms_utime - Prev_times.tms_utime;
	cpu->tms_stime = Cur_times.tms_stime - Prev_times.tms_stime;
	cpu->tms_cutime = Cur_times.tms_cutime - Prev_times.tms_cutime;
	cpu->tms_cstime = Cur_times.tms_cstime - Prev_times.tms_cstime;

	/* reset reference point */
	Tbegin = Tend;	
	Prev_times = Cur_times;

	/* aggregate the children with the parent */
	cpu->tms_utime += cpu->tms_cutime;
	cpu->tms_stime += cpu->tms_cstime;

	/* adjust these to seconds */
	cpu->tms_utime /= HZ;
	cpu->tms_stime /= HZ;
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
	char	article[BUFSIZ];

	/*
	** if no news to send, return
	*/
	if (filefile == (FILE *)NULL) {
		dprintf(stderr, "%s: %s: %s\n", Pname, file, errmsg(errno));
		return(FALSE);
	}

	if (hello(host) == FAIL) {
		fclose(filefile);
		return(FALSE);
	}

	while((fp = getfp(filefile, article, sizeof(article))) != (FILE *)NULL) {
		switch(code = ihave(fp, article)) {
		case CONT_XFER:
			if (!sendfile(fp)) {
				fprintf(stderr, "%s: %s: article transmission failed while sending %s\n", Pname, host, article);
				Stats.failed++;
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
			if (code != OK_XFERED) Stats.failed++;
			break;
		case ERR_GOTIT:
			fclose(fp);
			break;
		default:
			fprintf(stderr,"%s: %s gave an improper response to IHAVE: %d\n", Pname, host, code);
			fprintf(stderr,"%s: while sending article %s\n", Pname, article);
			fclose(filefile);
			fclose(fp);
			goodbye(DONT_WAIT);
			return(TRUE);
		}
	}
	fclose(filefile);
	if (unlink(file) < 0) {
		fprintf(stderr,"%s: unlink(%s): %s\n", Pname, file, errmsg(errno));
	}
	goodbye(WAIT);
	return(TRUE);
}

/*
** Read the header of a netnews article, snatch the message-id therefrom,
** and ask the remote if they have that one already.
*/
ihave(fp, article)
FILE	*fp;
char	*article;
{
	register int	code;
	struct hbuf	header;
	char	scr[LBUFLEN];
	char	buf[BUFSIZ];

	bzero(&header, sizeof(header));
	if (!rfc822read(&header, fp, scr)) {
		/*
		** something botched locally with the article
		** so we don't send it, but we don't break off
		** communications with the remote either.
		*/
		return(ERR_GOTIT);
	}

	/*
	** If an article shows up without a message-id,
	** or with a bogus message-id,
	** we scream bloody murder. That's one in
	** the `can't ever happen' category.
	*/
	if (header.ident[0] == '\0') {
		fprintf(stderr, "%s: %s missing message-id!\n", Pname, article);
		return(ERR_GOTIT);
	} else {
		(void) strcpy(scr, sp_strip(header.ident));
	}

	if (!msgid_ok(scr)) {
		fprintf(stderr, "%s: %s message-id syntax error!\n", Pname, article);
		return(ERR_GOTIT);
	}

	sprintf(buf, "IHAVE %s", scr);
	Stats.offered++;

	switch(code = converse(buf, sizeof(buf))) {
	case CONT_XFER:
		Stats.accepted++;
		rewind(fp);
		return(code);
	default:
		Stats.rejected++;
		return(code);
	}
}

/*
** Given that fp points to an open file containing filenames,
** open and return a file pointer to the next filename in the file.
** Don't you love indirection?
**
** Returns a valid FILE pointer or NULL if end of file.
*/

FILE *
getfp(fp, filename, fnlen)
register FILE	*fp;
char	*filename;
register unsigned	fnlen;
{
	register FILE	*newfp = (FILE *)NULL;
	register char	*cp;

	while(newfp == (FILE *)NULL) {
		if (fgets(filename, fnlen, fp) == (char *)NULL)
			return((FILE *)NULL);		/* EOF, tell caller */

		filename[fnlen - 1] = '\0';	/* make sure */

		if (*(cp = &filename[strlen(filename) - 1]) == '\n')
			*cp = '\0';

		if ((newfp = fopen(filename, "r")) == (FILE *)NULL) {
			register int	save = errno;

			fprintf(stderr, "%s: fopen(%s, \"r\"): %s\n", Pname, filename, errmsg(errno));
			/*
			** The only permissible error is `file non-existant'
			** anything else indicates something is seriously
			** wrong, and we should go away to let the shell
			** script clean up.
			*/
			if (save != ENOENT)
				exit(EX_OSERR);
		}
	}
	return(newfp);
}
