#include "mh.h"
#include <stdio.h>

int  convdir;
struct msgs *mp;
char *delimp;

m_convert(name)
char *name;
{
	register char *cp;
	register int first, last;
	int found, group, range, err;
	char *bp;

	found = group = 0;
	if(strcmp((cp = name), "all") == 0)
		cp = "first-last";
	if((err = first = m_conv(cp)) <= 0)
		goto badbad;
	if(*(cp = delimp) && *cp != '-' && *cp != ':')  {
	baddel: fprintf(stderr, "Illegal argument delimiter: \"%c\"\n", *delimp);
		return(0);
	}
	if(*cp == '-') {
		group++;  cp++;
		if((err = last = m_conv(cp)) <= 0) {
	  badbad:       if(err == -1)
				fprintf(stderr, "No %s message\n", cp);
			else
	  badlist:              fprintf(stderr, "Bad message list \"%s\".\n", name);
			return(0);
		}
		if(last < first) goto badlist;
		if(*delimp) goto baddel;
		if(first > mp->hghmsg || last < mp->lowmsg) {
	rangerr:        fprintf(stderr, "No messages in range \"%s\".\n", name);
			return(0);
		}
		if(last > mp->hghmsg)
			last = mp->hghmsg;
		if(first < mp->lowmsg)
			first = mp->lowmsg;
	} else if(*cp == ':') {
		cp++;
		if(*cp == '-') {
			convdir = -1;
			cp++;
		} else if(*cp == '+') {
			convdir = 1;
			cp++;
		}
		if((range = atoi(bp = cp)) == 0)
			goto badlist;
		while(*bp >= '0' && *bp <= '9') bp++;
		if(*bp)
			goto baddel;
		if((convdir > 0 && first > mp->hghmsg) ||
		   (convdir < 0 && first < mp->lowmsg))
			goto rangerr;
		if(first < mp->lowmsg)
			first = mp->lowmsg;
		if(first > mp->hghmsg)
			first = mp->hghmsg;
		for(last = first; last >= mp->lowmsg && last <= mp->hghmsg;
						last += convdir)
			if(mp->msgstats[last]&EXISTS)
				if(--range <= 0)
					break;
		if(last < mp->lowmsg)
			last = mp->lowmsg;
		if(last > mp->hghmsg)
			last = mp->hghmsg;
		if(last < first) {
			range = last; last = first; first = range;
		}
	} else {
		if(first > mp->hghmsg || first < mp->lowmsg ||
		   !(mp->msgstats[first]&EXISTS)) {
			fprintf(stderr, "Message %d doesn't exist.\n", first);
			return(0);
		}
		last = first;
	}
	while(first <= last) {
		if(mp->msgstats[first]&EXISTS) {
			if(!(mp->msgstats[first]&SELECTED)) {
				mp->numsel++;
				mp->msgstats[first] |= SELECTED;
				if(first < mp->lowsel)
					mp->lowsel = first;
				if(first > mp->hghsel)
					mp->hghsel = first;
			}
			found++;
		}
		first++;
	}
	if(!found)
		goto rangerr;
	return(1);
}

m_conv(str)
char *str;
{
	register char *cp, *bp;
	register int i;
	char buf[16];

	convdir = 1;
	cp = bp = str;
	if(*cp >= '0' && *cp <= '9')  {
		while(*bp >= '0' && *bp <= '9') bp++;
		delimp = bp;
		return(atoi(cp));
	}
	bp = buf;
	while((*cp >= 'a' && *cp <= 'z') || *cp == '.')
		*bp++ = *cp++;
	*bp++ = 0;
	delimp = cp;
	if(strcmp(buf, "first") == 0)
		return(mp->lowmsg);
	else if(strcmp(buf, "last") == 0) {
		convdir = -1;
		return(mp->hghmsg);
	} else if(strcmp(buf, "cur") == 0 || strcmp(buf, ".") == 0)
		return(mp->curmsg > 0 ? mp->curmsg : -1);
	else if(strcmp(buf, "prev") == 0) {
		convdir = -1;
		for(i = (mp->curmsg<=mp->hghmsg)? mp->curmsg-1: mp->hghmsg;
		    i >= mp->lowmsg; i--) {
			if(mp->msgstats[i]&EXISTS)
				return(i);
		}
		return(-1);                     /* non-existent message */
	} else if(strcmp(buf, "next") == 0)  {
		for(i = (mp->curmsg>=mp->lowmsg)? mp->curmsg+1: mp->lowmsg;
		    i <= mp->hghmsg; i++) {
			if(mp->msgstats[i]&EXISTS)
				return(i);
		}
		return(-1);
	} else
		return(0);                     /* bad message list */
}
