/* $Header: rule.h,v 1.1 85/05/02 08:01:11 nicklin Exp $ */

/*
 * Rule definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Rule table block struct
 */
typedef struct _ruleblk
	{
	char *r_rule;			/* pointer to rule string */
	struct _ruleblk *r_next;	/* ptr to next rule list block */
	} RULEBLK;
