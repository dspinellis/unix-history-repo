#include "util.h"

int issubstr(str, substr)
     char *str;
     char *substr;
{
  register char *sptr;
  char c;
  int substrlen = strlen(substr);
  int count;

  if (*substr == '\0' || *str == '\0') return(0);

  sptr = str;
  c = *substr;

  while (1) {
    while (*sptr != '\0' && *sptr != c) sptr++;
    if (*sptr == '\0') return(0);
    for (count = 0; count < substrlen; count++) {
      if (sptr[count] == '\0') return(0);
      else if (substr[count] != sptr[count]) break;
    }
    if (count == substrlen) return(1);
    sptr++;
  }
}

int indexstring(string, substring)
     char *string, *substring;
{
  register char *sub, *str;
  char c, s;
  int indx = 0;

  while (1) {
    str = string + indx;;
    if (*str == '\0') return(-1);
    sub = substring;

    if (*str == *sub) {
      s = *str;
      c = *sub;
      while(c == s && c != '\0') {
        c = *++sub;
        s = *++str;
      }

      if (c == '\0') return((int) indx);
      else if(s == '\0') return(-1);
    }
    indx++;
  }
}
