#ifdef USG

/* SysV version of bcopy from Eric Newton */

void bcopy (void *source, void *dest, int count)
{
  /* No overlapping */
  if (source == dest)
    return;

  if (source < dest && source+count > dest)
  {
     memcpy(dest, source, count);
     return;
  }

  if (dest < source && dest+count < source)
  {
     memcpy(dest, source, count);
     return;
  }

  /* Overlap */
  /* copy forward */
  if (source > dest)
  {
    while (count-- > 0) *((char *)dest)++ = *((char*)source)++;
    return;
  }
  /* copy backward */
  if (source < dest)
  {
    count--;
    dest = &dest[count];
    source = &source[count];
    while (count-- >= 0) *((char *)dest)-- = *((char*)source)--;
    return;
  }
}

#endif
