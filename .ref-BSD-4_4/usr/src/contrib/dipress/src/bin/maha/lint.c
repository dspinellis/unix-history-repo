/*
 * beta 4.3BSD doesn't have a declaration in its lint library
 * for "execlp" so one is included here.
 */

/*VARARGS*/
execlp(f, a) char *f, *a; {(void) f; (void) a;}


