
#define APDU_OP1	1	/* Simple generic request - no arguements */

#define APDU_ERR	2	/* Generate an error return */
#define APDU_URJ	3	/* Generate a User Reject */
#define APDU_PRJ	3	/* Generate a Provider Reject - not supported */

#define APDU_UNKNOWN	5

#define ERROR_UNKNOWN  6
#define ERROR_MISTYPED	7
#define ERROR_ERROR	8

/*
 * Types of underlying service we use
 */
#define RoRt	0		/* Reliable Transfer */
#define RoP	1		/* Presentation */
#define RoS	2		/* Session */

