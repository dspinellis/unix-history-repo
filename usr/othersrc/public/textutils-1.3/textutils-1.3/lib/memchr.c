/* memchr.c -- compare memory.
   Return address of first C in S, or NULL if not found.
   Stops looking after N characters.  Doesn't stop at nulls.
   In the public domain.
   By David MacKenzie <djm@ai.mit.edu>. */

char *
memchr (s, c, n)
     register char *s;
     register char c;
     register unsigned n;
{
  while (n--)
    {
      if (*s++ == c)
	return s - 1;
    }
  return 0;
}
