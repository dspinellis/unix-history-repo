/* $Header$ */

/*
 * "From " line struct
 *
 * Author: Peter J. Nicklin
 */
typedef struct _from
	{
	char *from;			/* name of the sender */
	char *tty;			/* tty string (if any) */
	char *date;			/* date string */
	DATE bdt;			/* broken down date */
	long m_seek;			/* start of message */
	long m_len;			/* length of message */
	} FROM;
