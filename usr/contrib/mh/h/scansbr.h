/* scansbr.h - definitions for scan () */

extern char *scanl;

#define	SCNENC	2		/* message just fine, but encrypted(!!) */
#define	SCNMSG	1		/* message just fine */
#define	SCNEOF	0		/* empty message */
#define	SCNERR	(-1)		/* error message */
#define	SCNNUM	(-2)		/* number out of range */


#ifndef	UK
#define	FORMAT	\
"%4(msg)%<(cur)+%| %>%<{replied}-%| %>\
%02(mon{date})/%02(mday{date})%<{date} %|*%>\
%<(mymbox{from})To:%14(friendly{to})%|%17(friendly{from})%>  \
%{subject} %<{body}<<%{body}%>"
#else UK
#define	FORMAT	\
"%4(msg)%<(cur)+%| %>%<{replied}-%| %>\
%02(mday{date})/%02(mon{date})%<{date} %|*%>\
%<(mymbox{from})To:%14(friendly{to})%|%17(friendly{from})%>  \
%{subject} %<{body}<<%{body}%>"
#endif	UK

#define	WIDTH	78

int	scan ();
