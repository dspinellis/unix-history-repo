#include "mh.h"
#include <stdio.h>

int     m_fldsz;

m_getfld(state, name, buf, bufsz, iob)
int state, bufsz;
char *name, *buf;
FILE *iob;
{
	register char *cp;
	register c;

 top:   while((c = getc(iob)) == '\001' && peekc(iob) == '\001')
		while(getc(iob) != '\n');

	if(c < 0)
		return(FILEEOF);
	m_fldsz = 0;

	switch(state) {

	case FLDEOF:
	case BODYEOF:
	case FLD:
		if(c == '\n' || c == '-')
			goto body;
		cp = name;
		for(;;) {
			if(cp >= name+NAMESZ-1) {
				*cp = 0;
fprintf(stderr, "??Component Name Exceeds %d Chars:\n    \"%s\"\n", NAMESZ-1, name);
				return(LENERR);
			}
			if(c == ':')
				break;
			if(c == '\n' || c < 0) {
				*cp = 0;
fprintf(stderr, "??%s Encountered While Scanning for a colon:\n    \"%s\"\n",
	(c < 0)? "<eof>":"<end of line>", name);
				return(FMTERR);
			}
			*cp++ = c;
			*cp   = 0;
			c = getc(iob);
		}

	case FLDPLUS:
		cp = buf;
		for(;;) {
			if((c = getc(iob)) < 0)
				return(FLDEOF);
			*cp++ = c;
			*cp   = 0;
			m_fldsz++;
			if(c == '\n')
				if((c = peekc(iob)) != ' ' && c != '\t')
					if(c == '\001' || c < 0)
						return(FLDEOF);
					else
						return(FLD);
			if(cp >= buf+bufsz-1)
				return(peekc(iob) < 0? FLDEOF:FLDPLUS);
		}

	body:   if(c == '-')
			while(getc(iob) != '\n') ;
		buf[0] = 0;
		if((c = getc(iob)) == '\001' && peekc(iob) == '\001')
			return(BODYEOF);

	case BODY:
		cp = buf;  *cp = 0;
		for(;;) {
			if(c < 0 || (c == '\001' && peekc(iob) == '\001'))
				return(BODYEOF);
			*cp++ = c;
			*cp   = 0;
			m_fldsz++;
			if(cp >= buf+bufsz-1)
				return(((c=peekc(iob))<0||c=='\001')?
					BODYEOF: BODY);
			c = getc(iob);
		}

	}
}
