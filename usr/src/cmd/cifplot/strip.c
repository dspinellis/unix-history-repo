/*******************************************************************
*                                                                  *
*    File: CIFPLOT/strip.c                                         *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/


#include <stdio.h>

char *strip(s)
char *s;
{
   char *t;
   while (*s == ' ' || *s == '\t' || *s == '\n') s++;
   t = s;
   while (*t != ' ' && *t != '\t' && *t != '\n' && *t != 0) t++;
   *t = 0;
   return(s);
   }

