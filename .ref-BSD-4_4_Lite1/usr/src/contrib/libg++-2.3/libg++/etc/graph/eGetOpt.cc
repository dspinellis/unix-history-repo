#pragma implementation
#include "eGetOpt.h"
  
int
eGetOpt :: next_arg (int &i)
{
  int tmp;
  if (0 < sscanf (nargv[optind], "%d", &tmp))
    {
      i = tmp;
      optind++;
      return 1;
    }
  else
    return 0;
}

int
eGetOpt :: next_arg (double &d)
{
  double tmp;
  if (0 < sscanf (nargv[optind], "%lf", &tmp))
    {
      d = tmp;
      optind++;
      return 1;
    }
  else
	return 0;
}

int
eGetOpt :: next_arg (String &s)
{
  if ('-' != nargv[optind][0])
    {
      s = nargv[optind];
      optind++;
      return 1;
    }
  else
    return 0;
}
