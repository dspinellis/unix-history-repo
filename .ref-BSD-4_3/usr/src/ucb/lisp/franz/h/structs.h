/*					-[Sat Jan 29 14:00:31 1983 by jkf]-
 * 	structs.h			$Locker:  $
 * random structure definitions 
 *
 * $Header: /na/franz/franz/h/structs.h,v 1.1 83/01/29 14:07:48 jkf Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */

/* 
 * this file contains auxiliary structure definitions which are used by
 * just a few files.
 */

/* transfer table structures. */

#define TRENTS 510

struct trent
{
    lispval (*fcn)();		/* function to call 			*/
    lispval name;	/* symbol which is the function to call */
};

struct trtab
{
    struct trtab *nxtt;			/* pointer to next transfer table */
    struct trent trentrs[TRENTS];	/* entries			  */
    int	sentinal;			/* must be zero			  */
};



struct heads {
	struct heads *link;
	char *pntr;
};


struct types
{
	char	*next_free;
	int	space_left,
		space,
		type,
		type_len;		/*  note type_len is in units of int */
	lispval *items,
		*pages,
		*type_name;
	struct heads
		*first;
	char 	*next_pure_free;

};

struct str_x
{
	char	*next_free;
	int	space_left;
};
