/* $Header$ */

/*
 * General macro function definitions
 *
 * Author: Peter J. Nicklin
 */

int strcmp();				/* string comparison */

#undef CHDIR
#define CHDIR(d) \
	(chdir(d) == 0)			/* change directory */
#undef EQUAL
#define EQUAL(s1,s2) \
	(strcmp(s1,s2) == 0)		/* string comparison */
#undef MIN
#define MIN(a,b) \
	(((a) < (b)) ? (a) : (b))	/* minimum of two values */
#undef MAX
#define MAX(a,b) \
	(((a) > (b)) ? (a) : (b))	/* maximum of two values */
#undef WHITESPACE
#define WHITESPACE(c) \
	(c == ' ' || c == '\t')		/* unseen space in a file */
