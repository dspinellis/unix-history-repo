/* $Header: CHdefault.c,v 1.2 86/10/11 15:12:52 jqj Exp $ */

/* contains:
 * CH_NameDefault
 */

/* $Log:	CHdefault.c,v $
 * Revision 1.2  86/10/11  15:12:52  jqj
 * add support for DOMAIN and ORGANIZATION environment variables
 * 
 * Revision 1.1  86/10/11  15:04:48  jqj
 * Initial revision
 * 
 */
#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <xnscourier/Clearinghouse2.h>

/*
 * path for the default names file
 */
#ifndef CHDEFPATH
#define CHDEFPATH "/usr/new/lib/xnscourier/CH.default"
#endif

/*
 * path to look for the addresses file on
 * (I wish I'd picked a shorter name, so we could rendezvous in /etc.
 *  But long file names in /etc are very unpopular!).
 */
char *chaddrpath[] = {
#ifdef CHADDRS
			CHADDRS,
#endif
			"/usr/new/lib/xnscourier/clearinghouse.addresses",
			"/etc/CH.addrs",
			"/usr/local/lib/xnscourier/clearinghouse.addresses",
			0 };


/*
 * Set defaults for organization and domain, based on an environment
 * variable, on the file /usr/new/lib/xnscourier/CH.default, or on
 * /usr/new/lib/xnscourier/clearinghouse.addresses
 * (should get the local clearinghouse and set defaults based on
 * ListDomainsServed
 */
CH_NameDefault(defaultobjnamep)
	Clearinghouse2_ObjectName *defaultobjnamep;
{
	FILE *chfile;
	int i, nmatch;
	char buf[BUFSIZ], *bp;
	static char orgbuf[21], domainbuf[21];
	extern char *getenv();

	defaultobjnamep->object = "";
	if ((bp = getenv("CHDEFAULTS")) != NULL &&
	    sscanf(bp,":%[^:]:%[^\n]", domainbuf, orgbuf) == 2) {
		defaultobjnamep->domain = domainbuf;
		defaultobjnamep->organization = orgbuf;
		return; /* done */
	}
	else if ((defaultobjnamep->domain = getenv("DOMAIN")) != NULL &&
		 (defaultobjnamep->organization = getenv("ORGANIZATION")) !=
			 NULL)
		return; /* done */
	if ((chfile = fopen(CHDEFPATH,"r")) !=
			(FILE *) NULL) {
		while (fgets(buf, BUFSIZ, chfile) != NULL)
		    if (sscanf(buf,":%[^:]:%[^\n]", domainbuf, orgbuf) == 2) {
			defaultobjnamep->domain = domainbuf;
			defaultobjnamep->organization = orgbuf;
			fclose(chfile);
			return;
		}
		fclose(chfile);
	}
	/* oh well.  Use CHADDRS */
	for (i=0; chaddrpath[i] != NULL; i++) {
		chfile = fopen(chaddrpath[i],"r");
		if (chfile == (FILE*)0)
			continue;
		while (fgets(buf, BUFSIZ, chfile) != NULL)
			if ((buf[0] != '#') &&
			    (sscanf(buf,"%*[^ \t\n] \":%[^:]:%[^\"]",
					domainbuf, orgbuf) == 2)) {
				fclose(chfile);
				defaultobjnamep->domain = domainbuf;
				defaultobjnamep->organization = orgbuf;
				return; /* done */
		}
		fclose(chfile);
	}
	defaultobjnamep->organization = "";
	defaultobjnamep->domain = "";
	return;
}
