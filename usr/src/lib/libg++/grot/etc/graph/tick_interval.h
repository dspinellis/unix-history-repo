#include "math.h"

// The resolution of the output device is much less than seven digits.
// A_HAIR_MORE is used to ingore the effects of round off error which should
// occur in the last few of the 16 digits.
#define A_HAIR_MORE 1.0000001

// TICK_INTERVAL returns the step size which can be used to put a
// specified NO_OF_TICKS beteen the specified UPPER_LIMIT and LOWER_LIMIT.

double
tick_interval (double no_of_ticks, double &lower_limit, double &upper_limit);
