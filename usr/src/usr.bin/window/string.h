/*
 *	@(#)string.h	3.1 83/11/22
 */

char *str_cpy();
char *str_cat();
char *str_itoa();

#define str_cmp(a, b)	strcmp(a, b)
