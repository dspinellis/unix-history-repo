/*	remote.c	4.4	81/11/20	*/
# include "tip.h"

/*
 * Attributes to be gleened from remote host description
 *   data base.
 */
static char **caps[] = {
	&AT, &DV, &CM, &CU, &EL, &IE, &OE, &PN
};

static char *capstrings[] = {
	"at", "dv", "cm", "cu", "el", "ie", "oe", "pn", 0
};

char *rgetstr();

static
getremcap(host)
	register char *host;
{
	int stat;
	char tbuf[BUFSIZ];
	static char buf[BUFSIZ/2];
	char *bp = buf;
	register char **p, ***q;

	if ((stat = rgetent(tbuf, host)) <= 0) {
		fprintf(stderr, stat == 0 ?
			"tip: unknown host %s\n" :
			"tip: can't open host description file\n", host);
		exit(3);
	}

	for (p = capstrings, q = caps; *p != NULL; p++, q++)
		**q = rgetstr(*p, &bp);
	if ((BR = rgetnum("br")) < 0)
		BR = DEFBR;
	if ((FS = rgetnum("fs")) < 0)
		FS = DEFFS;
	DU = rgetflag("du");
	HW = rgetflag("hw");
	if (DV == NOSTR) {
		fprintf(stderr, "%s: missing device spec\n", host);
		exit(3);
	}
	if (DU && CU == NOSTR)
		CU = DV;
	if (DU && PN == NOSTR) {
		fprintf(stderr, "%s: missing phone number\n", host);
		exit(3);
	}
	/*
	 * This effectively eliminates the "hw" attribute
	 *   from the description file
	 */
	if (!HW)
		HW = (CU == NOSTR) || (DU && equal(DV, CU));
	HO = host;
}

char *
getremote(host)
	char *host;
{
	register char *cp;
	static char *next;
	static int lookedup = 0;

	if (!lookedup) {
		if (host == NOSTR && (host = getenv("HOST")) == NOSTR) {
			fprintf(stderr, "tip: no host specified\n");
			exit(3);
		}
		getremcap(host);
		next = DV;
		lookedup++;
	}
	/*
	 * We return a new device each time we're called (to allow
	 *   a rotary action to be simulated)
	 */
	if (next == NOSTR)
		return(NOSTR);
	if ((cp = index(next, ',')) == NULL) {
		DV = next;
		next = NOSTR;
	} else {
		*cp++ = '\0';
		DV = next;
		next = cp;
	}
	return(DV);
}
