/*
 * Protocol for a sorting service.
 */

#define SORTPROG	((long) 22855)
#define SORTVERS	((long) 1)
#define SORT		((long) 1)

/*
 * The sort procedure receives an array of strings and returns an array
 * of strings.  This toy service handles a maximum of 64 strings.
 */
#define MAXSORTSIZE	((long) 64)

struct sortstrings {
	long	ns;  /* number of strings in the array */
	char	**s; /* pointer to the array of strings */
};

int xdr_sortstrings();

