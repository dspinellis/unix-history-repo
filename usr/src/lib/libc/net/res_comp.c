#ifndef lint
static char sccsid[] = "@(#)res_comp.c	4.1 (Berkeley) %G%";
#endif

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <nameser.h>


/*
 * Expand compressed domain name format to full domain name.
 * Return size of compressed name or -1 if there was an error.
 */
dn_expand(msg, comp_dn, exp_dn, length)
	char *msg, *comp_dn, *exp_dn;
	int length;
{
	register char *cp, *dn;
	register int n, c;
	char *eom;
	int len = 0;

	dn = exp_dn;
	cp = comp_dn;
	eom = exp_dn + length - 1;
	/*
	 * fetch next label in domain name
	 */
	while (n = *cp++) {
		/*
		 * Check for indirection
		 */
		switch (n & INDIR_MASK) {
		case 0:
			if (dn != exp_dn)
				*dn++ = '.';
			if (dn+n >= eom)
				return (-1);
			while (--n >= 0)
				if (islower(c = *cp++))
					*dn++ = toupper(c);
				else
					*dn++ = c;
			break;

		case INDIR_MASK:
			if (len == 0)
				len = cp - comp_dn + 1;
			cp = msg + (((n & 0x3f) << 8) | (*cp & 0xff));
			break;

		default:
			return (-1);			/* flag error */
		}
	}
	*dn = '\0';
	if (len == 0)
		len = cp - comp_dn;
	return (len);
}

/*
 * Compress domain name. Return the size of the compressed name or -1.
 * Dnptrs is a list of pointers to previous compressed names. dnptrs[0]
 * is a pointer to the beginning of the message. The list ends with NULL.
 */
dn_comp(exp_dn, comp_dn, length, dnptrs, lastdnptr)
	char *exp_dn, *comp_dn;
	int length;
	char **dnptrs, **lastdnptr;
{
	register char *cp, *dn;
	register int c, l;
	char **cpp, **lpp, *sp, *eob;
	char *msg;

	dn = exp_dn;
	cp = comp_dn;
	eob = comp_dn + length;
	if (dnptrs != NULL) {
		if ((msg = *dnptrs++) != NULL) {
			for (cpp = dnptrs; *cpp != NULL; cpp++)
				;
			lpp = cpp;	/* end of list to search */
		}
	} else
		msg = NULL;
	for (c = *dn++; c != '\0'; ) {
		/* look to see if we can use pointers */
		if (msg != NULL) {
			if ((l = dn_find(dn-1, msg, dnptrs, lpp)) >= 0) {
				if (cp+1 >= eob)
					return (-1);
				*cp++ = (l >> 8) | INDIR_MASK;
				*cp++ = l;
				return (cp - comp_dn);
			}
			/* not found, save it */
			if (lastdnptr != NULL && cpp < lastdnptr-1) {
				*cpp++ = cp;
				*cpp = NULL;
			}
		}
		sp = cp++;	/* save ptr to length byte */
		do {
			if (c == '.') {
				c = *dn++;
				break;
			}
			if (cp >= eob)
				return (-1);
			*cp++ = c;
		} while ((c = *dn++) != '\0');
		if ((l = cp - sp - 1) <= 0 || l > MAXLABEL)
			return (-1);
		*sp = l;
	}
	if (cp >= eob)
		return (-1);
	*cp++ = '\0';
	return (cp - comp_dn);
}

/*
 * Skip over a compressed domain name. Return the size.
 */
dn_skip(buf)
	char *buf;
{
	register char *cp;
	register int n;

	cp = buf;
	while (n = *cp++) {
		/*
		 * check for indirection
		 */
		switch (n & INDIR_MASK) {
		case 0:		/* normal case, n == len */
			cp += n;
			continue;
		default:	/* illegal type */
			return (-1);
		case INDIR_MASK:	/* indirection */
			cp++;
		}
		break;
	}
	return (cp - buf);
}

/*
 * Search for expanded name from a list of previously compressed names.
 * Return the offset from msg if found or -1.
 */
dn_find(exp_dn, msg, dnptrs, lastdnptr)
	char *exp_dn, *msg;
	char **dnptrs, **lastdnptr;
{
	register char *dn, *cp, **cpp;
	register int n;
	char *sp;

	for (cpp = dnptrs; cpp < lastdnptr; cpp++) {
		dn = exp_dn;
		sp = cp = *cpp;
		while (n = *cp++) {
			/*
			 * check for indirection
			 */
			switch (n & INDIR_MASK) {
			case 0:		/* normal case, n == len */
				while (--n >= 0)
					if (*dn++ != *cp++)
						goto next;
				if ((n = *dn++) == '\0' && *cp == '\0')
					return (sp - msg);
				if (n == '.')
					continue;
				goto next;

			default:	/* illegal type */
				return (-1);

			case INDIR_MASK:	/* indirection */
				cp = msg + (((n & 0x3f) << 8) | (*cp & 0xff));
			}
		}
		if (*dn == '\0')
			return (sp - msg);
	next:	;
	}
	return (-1);
}
