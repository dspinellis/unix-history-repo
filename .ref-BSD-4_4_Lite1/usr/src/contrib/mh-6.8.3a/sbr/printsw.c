/* printsw.c - print switches */

#include "../h/mh.h"


void printsw (substr, swp, prefix)
register char  *substr,
               *prefix;
register struct swit   *swp;
{
    int     len,
	    optno;
    register int    i;
    register char  *cp,
                   *cp1,
		   *sp;
    char    buf[128];

    len = strlen (substr);
    for (; swp -> sw; swp++) {
	if (!*substr ||		/* null matches all strings */
		(ssequal (substr, swp -> sw) && len >= swp -> minchars)) {
	    optno = 0;
	    if (sp = (&swp[1]) -> sw) /* next switch */
		if (!*substr && sp[0] == 'n' && sp[1] == 'o' &&
			strcmp (&sp[2], swp -> sw) == 0 && (
			((&swp[1]) -> minchars == 0 && swp -> minchars == 0) ||
			((&swp[1]) -> minchars == (swp -> minchars) + 2)))
		    optno++;
	    if (swp -> minchars > 0) {
		cp = buf;
		*cp++ = '(';
		if (optno) {
		    (void) strcpy (cp, "[no]");
		    cp += strlen (cp);
		}
		for (cp1 = swp -> sw, i = 0; i < swp -> minchars; i++)
		    *cp++ = *cp1++;
		*cp++ = ')';
		while (*cp++ = *cp1++);
		printf ("  %s%s\n", prefix, buf);
	    }
	    else
		if (swp -> minchars == 0)
		    printf (optno ? "  %s[no]%s\n" : "  %s%s\n",
			    prefix, swp -> sw);
	    if (optno)
		swp++;	/* skip -noswitch */
	}
    }
}
