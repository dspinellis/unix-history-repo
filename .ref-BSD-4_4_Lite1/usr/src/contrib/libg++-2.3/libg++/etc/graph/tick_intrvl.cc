#include "tick_intrvl.h"

// TICK_INTERVAL returns the step size which can be used to put a
// specified NO_OF_TICKS beteen the specified UPPER_LIMIT and LOWER_LIMIT.

double
tick_interval (double no_of_ticks, double &lower_limit, double &upper_limit)
{
  if (lower_limit == upper_limit)
    {				// make sure the range is nonzero.
      if (lower_limit == 0.)
	{
	  lower_limit = -1.;	// this is the tradtional behavior of graph.
	  upper_limit =  1.;
	}
      else
	{
	  lower_limit *= .9;
	  upper_limit *= 1.1;
	}
    }
  // compute interval for tick marks.
  double exp = 1.;
  int i = (int) floor (log10 (fabs (upper_limit - lower_limit)) * A_HAIR_MORE);
  for (; 0 < i; i--) exp *= 10.;
  for (; 0 > i; i++) exp /= 10.;
  double mant = (upper_limit - lower_limit) / exp;
  
  double interval = 10.;
  double stepsize = 1.;
  while (interval * (no_of_ticks - 1.) > fabs (mant) * A_HAIR_MORE)
    {
      if (interval - stepsize <= 0.) stepsize /= 10.;
      interval -= stepsize;
    }
  interval *= exp;
  if (mant < 0.) interval = - interval;
  return interval;
}
