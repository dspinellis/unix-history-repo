/* bcopy.c -- copy memory.
   Copy LENGTH bytes from SOURCE to DEST.  Does not null-terminate.
   In the public domain.
   By David MacKenzie <djm@ai.mit.edu>. */

void
bcopy (source, dest, length)
     char *source, *dest;
     unsigned length;
{
  if (source < dest)
    /* Moving from low mem to hi mem; start at end. */
    for (source += length, dest += length; length; --length)
      *--dest = *--source;
  else if (source != dest)
    /* Moving from hi mem to low mem; start at beginning. */
    for (; length; --length)
      *dest++ = *source++;
}
