/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)strings.h	5.7 (Berkeley) %G%
 */

#ifdef __STDC__
extern int memcmp(const char *, const char *, int);
extern int strcasecmp(const char *, const char *);
extern int strcmp(const char *, const char *);
extern int strcspn(const char *, const char *);
extern int strlen(const char *);
extern int strncasecmp(const char *, const char *, int);
extern int strncmp(const char *, const char *, int);
extern int strspn(const char *, const char *);
extern char *index(const char *, int);
extern char *memccpy(char *, const char *, int, int);
extern char *memchr(const char *, int, int);
extern char *memcpy(char *, const char *, int);
extern char *memset(char *, int, int);
extern char *rindex(const char *, int);
extern char *strcat(char *, const char *);
extern char *strchr(const char *, int);
extern char *strcpy(char *, const char *);
extern char *strdup(const char *);
extern char *strerror(int);
extern char *strncat(char *, const char *, int);
extern char *strncpy(char *, const char *, int);
extern char *strpbrk(const char *, const char *);
extern char *strrchr(const char *, int);
extern char *strsep(char *, const char *);
extern char *strtok(char *, const char *);
#else
extern int memcmp();
extern int strcasecmp();
extern int strcmp();
extern int strcspn();
extern int strlen();
extern int strncasecmp();
extern int strncmp();
extern int strspn();
extern char *index();
extern char *memccpy();
extern char *memchr();
extern char *memcpy();
extern char *memset();
extern char *rindex();
extern char *strcat();
extern char *strchr();
extern char *strcpy();
extern char *strdup();
extern char *strerror();
extern char *strncat();
extern char *strncpy();
extern char *strpbrk();
extern char *strrchr();
extern char *strsep();
extern char *strtok();
#endif
