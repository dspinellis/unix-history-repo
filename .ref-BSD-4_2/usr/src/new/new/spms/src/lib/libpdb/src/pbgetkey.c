/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbgetkey() loads the next key field into key. Returns key or null
 * character if no more key fields.
 */

extern char *KP;			/* next key field */

char *
pbgetkey(key)
	char *key;			/* key string */
{
	char *pbcpykey();		/* copy key */
	char *pbskipkey();		/* skip to next key */

	key = pbcpykey(key, KP);
	KP = pbskipkey(KP);
	return(key);
}
