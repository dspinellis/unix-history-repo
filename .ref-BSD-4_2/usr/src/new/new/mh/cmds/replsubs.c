#include "mh.h"

char *ltrim(cp)
char *cp;
{
	/* Return pointer to 1st non-blank char in string;
	 * If ptr ==> 0 or ptr ==> '\n'0, return NUL;
	 */

	register char *cp1;

	cp1 = cp;
	while((*cp1 == ' ') || (*cp1 == '\t')) cp1++;
	if((*cp1 == 0) || (*cp1 == '\n' && cp1[1] == 0))
		cp1 = 0;
	return(cp1);
}

char *rtrim(cp)
char *cp;
{
	/* trim newline and blanks from the right */

	register char *cp1;

	cp1 = cp+strlen(cp)-1;
	if(*cp1 == '\n') *cp1 = 0;
	while(*--cp1 == ' ') ;
	*++cp1 = 0;
	return(cp);
}

char *niceadd(this, that)
char *this, *that;
{
	register char *from, *to;

	if(!(from = ltrim(this)))               /* nothing to add */
		return(that);

	if(to = that)
		to = add(",\n    ", rtrim(to)); /* enuf blanks for "cc: " */
	return(add(from, to));
}

#define ADDRLEN (needadr ? addrlen : 0)

char *fix(field, address)
char *field, *address;
{
	/* Appends address to each needy addressee in "field".
	 * Returns pointer to copy of new string. (HUH?)
	 * "field" should never be 0
	 */

	register int len;
	register char *newp;
	int addrlen;
	int needadr;
	int fieldlen;
	char *cp;

	addrlen = strlen(address);
	len = 0;
	newp = "";

	for(cp = field; ;cp += fieldlen + 1) {
		needadr = needsaddr(&cp, &fieldlen); /* cp may be changed */
		if(fieldlen == 0) {
			cndfree(field);
			newp = add("\n", newp);
			return(newp);
		}
		if((len + fieldlen + ADDRLEN) > 70) {
			newp = add(",\n    ", newp);
			len = 4;
		} else if (*newp) {
			newp = add(", ", newp);
			len += 2;
		}
		*(cp + fieldlen) = 0;
		newp = add(cp, newp);
		if(needadr)
			newp = add(address, newp);
		len += fieldlen + ADDRLEN;
	}
}

anychar(fchars, field)
char *fchars, *field;
{
	/* Returns 1 if any fancy char appears in "field"
	 * Returns 0 if either "field" is nul or contains no fancy chars
	 */

	register char *fp;

	if(!field)
		return(0);
	for(fp = fchars; *fp; fp++)
		if(r_any(*fp, field))
			return(1);
	return(0);
}


r_any(chr,stg)
char chr, *stg;
{
	register char c, *s;

	c = chr;
	for (s = stg; (*s) && (*s != ',') && (*s != '\n');)
		if (*s++ == c) return (1);
	return (0);
}

char *addr(text)
char *text;
{
	static char buf[128];
	register char *cp, *bufp;
	int textseen, blankseen;
	char *copyaddr();

	textseen = blankseen = 0;
	bufp = buf;
	if(!text)
		return(0);
	for(cp = text; (*cp == ' ' || *cp == '\t'); cp++);
	for(;;cp++) {
		switch(*cp) {
		default:
			textseen++;
			break;
		case ' ':  case '\t':
			blankseen++;
			break;
		case 'a':
			if(!blankseen || !textseen || !ssequal("at ",cp)){
				textseen++;
				break;
			}
			bufp = copy(" at ", buf);
			copyaddr(cp+3, bufp);
			return(buf);
		case '@':
			if(!textseen)
				return(0);
			bufp = copy(" @ ", buf);
			copyaddr(cp+1, bufp);
			return(buf);
		case ',':  case '\n':  case 0:
			return(0);
		}
	}
}

#define ND1  (*fp) && (*fp != ' ') && (*fp != '\t')
#define ND2  (*fp != '<') && (*fp != '(') && (*fp != '>') && (*fp != ')')
#define ND3  (*fp != '\n') && (*fp!= ',') && (*fp != ':')
#define NOTDELIM  ND1 &&  ND2 && ND3

char *copyaddr(from, to)
char *from, *to;
{
	/* Copies left-trimmed "from" to "to".
	 * Copy terminates on any delimiter.
	 * Returns pointer to NUL terminator in destination string
	 */

	register char *fp, *tp;

	for(fp = from; (*fp == ' ') && (*fp == '\t'); fp++) ;
	for( ; NOTDELIM; *tp++ = *fp++);
	*tp = 0;
	return(tp);
}
#define NOTRELEVANT (*cp == ' ' || *cp == '\t' || *cp == '\n'|| *cp == ',')

needsaddr(field, fieldlen)
char **field;
int *fieldlen;
{
	/* Returns 1 if this field needs an address
	 * Returns 0 if field contains any funny chars or has
	 * an address of the form "xxxx at " or "xxxx[<b>]@"
	 * "field": on input --  addr of pointer to start of field
	 *          on output -- val of ptr moved to 1st meaty char
	 * "fieldlen" returns the length of the new field
	 *  (it terminates on ',' or '\n' or 0)
	 */

	register char *cp;
	int textseen, blankseen;
	int retval;
				   /* find 1st relevant char in field */
	for(cp = *field; NOTRELEVANT ; cp++);

	*field = cp;               /* return it to caller */
	if(anychar("(<:", cp)) {
		retval = 0;
		goto leave;
	}
	for(;;cp++) {
		switch(*cp) {
		default:
			textseen++;
			break;
		case ' ':  case '\t':
			blankseen++;
			break;
		case 'a':
			if(!blankseen || !textseen || !ssequal("at ",cp)){
				textseen++;
				break;
			}
		case '@':
			retval = 0;
			goto leave;
		case ',':  case '\n':  case 0:
			retval = 1;
			goto leave;

		}
	}
 leave:
	for(; (*cp) && (*cp != ',') && (*cp != '\n'); cp++) ;
	*fieldlen = cp- *field;
	return(retval);
}

