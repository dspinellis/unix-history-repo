// -*- C++ -*-
// READ_DATA reads ascii (or binary) data from an istream (or File) of
// coordinates and labels.  It passes back the array of points (x, y,
// and label) containing the data.

#ifndef read_data_h
#define read_data_h 1

#include <stream.h>
#include <String.h>
#include <ctype.h>
#include "pXPlex.h"


#ifdef HUGE
#undef HUGE
#endif
#define HUGE 1e37
#define sp <<" "<<
#define nl <<"\n"

// data_type value indicates the type of data present in the input files.
typedef enum { ASCII, DOUBLE, INT } data_type;

// read ascii binary data from a file
void
read_data (istream& in, char* in_filename, pointXPlex &pplex,
	   int auto_abscissa, double x_start,
	   double delta_x, int &symbol_number,
	   data_type input_data, int switch_symbols);
#endif
