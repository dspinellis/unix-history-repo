#

/*
 * Mail -- a mail program
 *
 * Generally useful tty stuff.
 */

#include "rcv.h"
#include <sgtty.h>

static	int	c_erase;		/* Current erase char */
static	int	c_kill;			/* Current kill char */

/*
 * Read all relevant header fields.
 */

grabh(hp, gflags)
	struct header *hp;
{
	struct sgttyb ttybuf;
	register int s;
	int (*savesigs[2])(), errs, set;

	errs = 0;
	set = 0;
	if (gtty(fileno(stdin), &ttybuf) < 0) {
		perror("gtty");
		return(-1);
	}
	c_erase = ttybuf.sg_erase;
	c_kill = ttybuf.sg_kill;
	ttybuf.sg_erase = 0;
	ttybuf.sg_kill = 0;
	for (s = SIGINT; s <= SIGQUIT; s++)
		if ((savesigs[s-SIGINT] = signal(s, SIG_IGN)) == SIG_DFL)
			signal(s, SIG_DFL);
	if (gflags & GTO) {
		if (!set && hp->h_to != NOSTR)
			set++, stty(fileno(stdin), &ttybuf);
		hp->h_to = readtty("To: ", hp->h_to);
		if (hp->h_to != NOSTR)
			hp->h_seq++;
	}
	if (gflags & GSUBJECT) {
		if (!set && hp->h_subject != NOSTR)
			set++, stty(fileno(stdin), &ttybuf);
		hp->h_subject = readtty("Subject: ", hp->h_subject);
		if (hp->h_subject != NOSTR)
			hp->h_seq++;
	}
	if (gflags & GCC) {
		if (!set && hp->h_cc != NOSTR)
			set++, stty(fileno(stdin), &ttybuf);
		hp->h_cc = readtty("Cc: ", hp->h_cc);
		if (hp->h_cc != NOSTR)
			hp->h_seq++;
	}
	if (gflags & GBCC) {
		if (!set && hp->h_bcc != NOSTR)
			set++, stty(fileno(stdin), &ttybuf);
		hp->h_bcc = readtty("Bcc: ", hp->h_bcc);
		if (hp->h_bcc != NOSTR)
			hp->h_seq++;
	}
	ttybuf.sg_erase = c_erase;
	ttybuf.sg_kill = c_kill;
	if (set)
		stty(fileno(stdin), &ttybuf);

out:
	for (s = SIGINT; s <= SIGQUIT; s++)
		signal(s, savesigs[s-SIGINT]);
	return(errs);
}

/*
 * Read up a header from standard input.
 * The source string has the preliminary contents to
 * be read.
 *
 */

char *
readtty(pr, src)
	char pr[], src[];
{
	char canonb[BUFSIZ];
	register int c;
	register char *cp, *cp2;

	fputs(pr, stdout);
	if (src != NOSTR && strlen(src) > BUFSIZ - 2) {
		printf("too long to edit\n");
		return(src);
	}
	if (src != NOSTR)
		cp = copy(src, canonb);
	else
		cp = copy("", canonb);
	fputs(canonb, stdout);
	fflush(stdout);
	cp2 = fgets(cp, BUFSIZ - (cp - canonb), stdin);
	canonb[strlen(canonb) - 1] = '\0';
	if (cp2 == NOSTR || *cp2 == '\0')
		return(src);
	cp = canonb;
	cp2 = cp;
	while (*cp != '\0') {
		c = *cp++;
		if (c == c_erase) {
			if (cp2 == canonb)
				continue;
			if (cp2[-1] == '\\') {
				cp2[-1] = c;
				continue;
			}
			cp2--;
			continue;
		}
		if (c == c_kill) {
			if (cp2 == canonb)
				continue;
			if (cp2[-1] == '\\') {
				cp2[-1] = c;
				continue;
			}
			cp2 = canonb;
			continue;
		}
		*cp2++ = c;
	}
	*cp2 = '\0';
	if (equal("", canonb))
		return(NOSTR);
	return(savestr(canonb));
}
