/*	inet.h	4.1	83/05/28	*/

/*
 * External definitions for
 * functions in inet(3N)
 */
struct	in_addr inet_addr();
char	*inet_ntoa();
struct	in_addr inet_makeaddr();
unsigned long inet_network();
