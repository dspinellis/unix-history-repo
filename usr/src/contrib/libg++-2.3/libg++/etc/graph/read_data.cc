#include "read_data.h"

// READ_DATA reads ascii (or binary) data from an istream of
// coordinates and labels.  It passes back the array of points (x, y,
// and label) containing the data.
     
void
read_data (istream& in, char* in_filename, pointXPlex &pplex,
	   int auto_abscissa, double x_start,
	   double delta_x, int &symbol_number,
	   data_type input_data, int switch_symbols)
{
  char next_char;		// next character to be read
  point p;			// the point we are currently reading in.
  p.label = (char *) 0;		// binary files contain no labels.
  double prev_x = -HUGE;	// the previous x value.
  
  if (auto_abscissa)
    x_start -= delta_x;
  
  if (input_data == ASCII)
    {				// input contains ascii data
      while (in.good ())
	{
	  if (auto_abscissa)
	    p.x = x_start = x_start + delta_x;
	  else
	    in >> p.x >> WS;
	  if (switch_symbols && (p.x < prev_x))
	    symbol_number++;
	  prev_x = p.x;
	  
	  in >> p.y;
	  if (!in.good ())		// if we read x but not y complain.
	    {
	      char *input_line;
	      in.clear ();
	      in.gets (&input_line);
	      // if the input contains one coordinate per line
	      // you win here --- emacs can find the source
	      // line from this error message.
	      cerr << in_filename << ":" << 2 + pplex.high()
		<< ": unable to read the" sp 2 + pplex.high()
		  << ((2 + pplex.high() == 1) ? "st" : "th")
		  << " y coordiate.\n";
	      if (input_line)
		if (strlen (input_line))
		    cerr << in_filename << ":" << 2 + pplex.high()
		    << ": unexpected `" << input_line << "'\n";
	      break;
	    }
	  in >> WS;			// skip white space after y coordinate
	  p.label = (char *) 0;			// by default there is no label
	  in.get (next_char);				// look ahead for a label
	  if (in.good ())
	    {
	      in.putback (next_char);
	      if (!isdigit (next_char)	// if a lable is found
		  && (next_char != '.')
		  && (next_char != '+')
		  && (next_char != '-'))
		{
		  in.gets (&p.label);		// store it with x and y
		}
	      in >> WS;			// skip white space after label
	    }
	  p.symbol = symbol_number;
	  pplex.add_high (p);
	}
      return;
    }

  if (input_data == DOUBLE)
    {				// input contains binary double precision
      while (in.good ())
	{
	  if (auto_abscissa)
	    p.x = x_start = x_start + delta_x;
	  else
	    {
	      in.read (&p.x, sizeof(p.x));
	      if (switch_symbols && (p.x < prev_x))
		symbol_number++;
	      prev_x = p.x;
	    }
	  in.read (&p.y, sizeof(p.y));
	  p.symbol = symbol_number;
	  if (in.good ())
	    pplex.add_high (p);
	}
    }
  if (input_data == INT)
    {				// input contains binary integers
      int i;
      while (in.good ())
	{
	  if (auto_abscissa)
	    p.x = x_start = x_start + delta_x;
	  else
	    {
	      in.read (&i, sizeof(i));
	      p.x = i;
	      if (switch_symbols && (p.x < prev_x))
		symbol_number++;
	      prev_x = p.x;
	    }
	  in.read (&p.y, sizeof(p.y));
	  p.symbol = symbol_number;
	  if (in.good ())
	    pplex.add_high (p);
	}
    }
  return;
}
