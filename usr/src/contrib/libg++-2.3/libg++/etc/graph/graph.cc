// graph.cc --- graph reads data and writes out a plot file.

const char *copyright_notice = "\
Copyright (C) 1989 Free Software Foundation \n\
 \n\
This file is part of GNU CC. \n\
 \n\
GNU CC is distributed in the hope that it will be useful, \n\
but WITHOUT ANY WARRANTY.  No author or distributor \n\
accepts responsibility to anyone for the consequences of using it \n\
or for whether it serves any particular purpose or works at all, \n\
unless he says so in writing.  Refer to the GNU CC General Public \n\
License for full details. \n\
 \n\
Everyone is granted permission to copy, modify and redistribute \n\
GNU CC, but only under the conditions described in the \n\
GNU CC General Public License.   A copy of this license is \n\
supposed to have been given to you along with GNU CC so you \n\
can know your rights and responsibilities.  It should be in a \n\
file named COPYING.  Among other things, the copyright notice \n\
and this notice must be preserved on all copies. \n\
";

#include "read_data.h"
#include "eGetOpt.h"
#include "ePlotFile.h"
#include "tick_intrvl.h"
#include <builtin.h>
#include <strstream.h>

const char *usage_message = "\
  [options...]\n\
\n\
    Option:                         Description:\n\
    -C                              print copyright notice\n\
    -D                              binary double precision data\n\
    -E                              use extended plot file format\n\
    -H CHARACTER_HEIGHT             fractional height of characters\n\
    -I                              binary integer data\n\
    -K                              switch symbol for each new line\n\
    -L                              switch line style for each new line\n\
    -M [x|y] MARGIN                 margin between data and edges of box\n\
    -N TICKS                        number of tick marks on each axis\n\
    -P SIZE                         plot file coordinate range\n\
    -S SYMBOL_NUMBER SYMBOL_SIZE    draw symbols at each point\n\
    -T TICK_SIZE                    fractional size of tick marks\n\
    -W CHARACTER_WIDTH              fractional width of characters\n\
    -X X_LABEL                      label printed below the x axis\n\
    -Y Y_LABEL                      label printed right of the y axis\n\
    -a STEP_SIZE LOWER_LIMIT        generate abcissa, read only y values\n\
    -b                              break lines whenever x decreases\n\
    -c POINT_LABEL                  default label printed at each point\n\
    -d                              print debugging information\n\
    -g GRID_STYLE                   draw a grid in the plot\n\
    -h HEIGHT                       fractional height of the plot\n\
    -l TOP_LABEL                    label printed above the plot\n\
    -m LINE_MODE                    solid and dashed lines\n\
    -r RIGHT                        move plot right by fractional ammount\n\
    -s                              save the screen - do not erase\n\
    -t                              transpose x ans y axes\n\
    -u UP                           move plot up by fractional ammount\n\
    -w WIDTH                        fractional width of the plot\n\
    -x [lTB] LOWER_LIMIT UPPER_LIMIT log scale, axis limits\n\
    -y [lLR] LOWER_LIMIT UPPER_LIMIT log scale, axis limits\n\
    -z                              do not read data from standard input\n\
";

// Here are the command line option data and flags:

String default_label;		// default label at each point.
String top_label;		// label above the plot.
String x_label;			// x axis label
String y_label;			// y axis label
data_type input_data = ASCII;	// the type of data to be read in.
double char_height = .03;	// fractional height of printed characters
double char_width = .02;	// fractional width of printed characters
double height = .8;		// fraction height of the plot.
double lower_limit = 0.;	// lower limit in x for generated values
double no_of_ticks = 5.;	// number of tick marks on the axes.
double right = .1;		// the fractional margin on the right side.
double size_of_ticks = .01;	// fractional size of the tick marks.
double spacing = 1.;		// stepsize for equally spaced generated values
double symbol_size = .01;	// index of symbol drawn at each point.
double up = .1;			// the fractional margin above the plot.
double width = .8;		// fraction width of the plot.
double x_lower_limit = HUGE;	// HUGE means get it from the data
double x_margin = 0.0;		// fractional margin between data and box
double x_upper_limit = HUGE;	// HUGE means get it from the data
double y_lower_limit = HUGE;	// HUGE means get it from the data
double y_margin = 0.05;		// fractional margin between data and box
double y_upper_limit = HUGE;	// HUGE means get it from the data
int abcissa_flag = 0;		// nonzero means generate x axiz values
int break_flag = 0;		// break the line whenever x decreases.
int debug_flag = 0;		// verbose debugging output.
int extended_plot_format = 0;	// nonzero means use adjusted labels.
int grid_style = 1;		// style of box and or axes.
int line_style = 0;		// the type of line drawn to connect points.
int no_standard_input = 0;	// nonzero means do not read from standard input
int plot_size = 4096;		// upper limit of plot file coordinates
int save_screen_flag = 0;	// nonzero means do not erase before plotting.
int switch_style = 0;		// switch line style for each new curve
int switch_symbols = 0;		// switch symbols when starting each new curve
int symbol_number = -1;		// index of symbol drawn at each point.
int transpose_axes_flag = 0;	// nonzero means interchange x and y axes.
int x_label_on_top = 0;		// nonzero means label tick marks on right side
int x_log_scale = 0;		// the x axis is log scale
int y_label_on_right = 0;	// nonzero means label tick marks on top
int y_log_scale = 0;		// the y axis is log scale

// the names of line styles recognized in the unix plot file convention.
char *line_style_name[] =
{
  "solid",
  "longdashed",
  "dotted",
  "disconnected",
  "dotdashed",
  "shortdashed"
  };
const int no_of_line_styles = sizeof(line_style_name)/sizeof(line_style_name[0]);

// This is all the data describing how to draw the symbols.
								
typedef enum op {END, CONT, MOVE, CIRCLE}; // a graphic operation

struct coord			// a component coordintate within a symbol
{
  double x, y;			// fractional coordinates
  op operation;			// the type of graphic
};

struct coord symbol[][10] =	// set of symbols
{
  {				// plus sign
    { -.5,  .0, MOVE},
    {  .5,  .0, CONT},
    {  .0, -.5, MOVE},
    {  .0,  .5, CONT},
    {  .0,  .0, END}
  }, {				// cross
    { -.5, -.5, MOVE},
    {  .5,  .5, CONT},
    {  .5, -.5, MOVE},
    { -.5,  .5, CONT},
    {  .0,  .0, END}
  }, {				// diamond
    { -.5,  .0, MOVE},
    {  .0,  .5, CONT},
    {  .5,  .0, CONT},
    {  .0, -.5, CONT},
    { -.5,  .0, CONT},
    {  .0,  .0, END}
  }, {				// square
    { -.5, -.5, MOVE},
    { -.5,  .5, CONT},
    {  .5,  .5, CONT},
    {  .5, -.5, CONT},
    { -.5, -.5, CONT},
    {  .0,  .0, END}
  }, {				// triangle
    { -.5, -.5, MOVE},
    {  .0,  .86603, CONT},
    {  .5, -.5, CONT},
    { -.5, -.5, CONT},
    {  .0,  .0, END}
  }, {				// circle
    {  .5,  .0, CIRCLE},
    {  .0,  .0, END}
  }, {				// circle with a line through it
    {  .5,  .0, CIRCLE},
    {  .0, -.5, MOVE},
    {  .0,  .5, CONT},
    {  .0,  .0, END}
  }				// add more symbols here...
};
const int no_of_symbols = sizeof(symbol) / sizeof(symbol[0]);

// Here are the functions for transforming and clipping.

inline int px (double t)	// transform fractional x to plot x
{
  return (int) (plot_size * (width * t + right)); // should we round rather than
						  // truncate here?
}

inline int py (double t)	// transform fractional x to plot x
{
  return (int) (plot_size * (height * t + up));
}

inline double clip (double t)
{
  return (t >? 0.) <? 1.;
}

// uppper and lower bounds on the data
double xmin = HUGE, ymin = HUGE, xmax = -HUGE, ymax = -HUGE;
double log_xmin, log_ymin, log_xmax, log_ymax;

inline double fx (double t)	// transform data x to fractional x
{
  return x_log_scale ?
    (log (t) - log_xmin) / (log_xmax - log_xmin) :
    (t - xmin) / (xmax - xmin);
}

inline double fy (double t)	// transform data y to fractional y
{
  return y_log_scale ?
    (log (t) - log_ymin) / (log_ymax - log_ymin) :
    (t - ymin) / (ymax - ymin);
}

inline int in_box (double x, double y) // return 1 if point is inside box
{
  return (x >= xmin) && (x <= xmax) && (y >= ymin) && (y <= ymax);
}

int
main (int argc, char **argv)
{
  eGetOpt getopt
    (argc, argv,
     "CDEH::IJKLM::N::P::ST::W::X::Y::a::bc::dg::h::l::m::r::stu::vw::x::y::z");
  int option_char;
  int error_occurred = 0;	// non zero for a bad command line option
  
  while (EOF != (option_char = getopt ()))
    switch (option_char)
      {
      case 'C':
	cerr << copyright_notice; break;
      case 'D':	input_data = DOUBLE; break;
      case 'E':	extended_plot_format++; break;
      case 'H': getopt.next_arg (char_height); break;
      case 'I':	input_data = INT; break;
      case 'K':	switch_symbols++; break;
      case 'L':	switch_style++; break;
      case 'M':
	if ('x' == getopt.first_char())
	  {getopt.optind++; getopt.next_arg (x_margin);} break;
	if ('y' == getopt.first_char())
	  {getopt.optind++; getopt.next_arg (y_margin);} break;
      case 'N':	getopt.next_arg (no_of_ticks); break;
      case 'P':	getopt.next_arg (plot_size); break;
      case 'S':	getopt.next_arg (symbol_number); 
	getopt.next_arg (symbol_size); break;
      case 'T': getopt.next_arg (size_of_ticks); break;
      case 'W': getopt.next_arg (char_width); break;
      case 'X': getopt.next_arg (x_label); break;
      case 'Y': getopt.next_arg (y_label); break;
      case 'a':	abcissa_flag++; getopt.next_arg (spacing);
	getopt.next_arg (lower_limit); break;
      case 'b':	break_flag++; break;
      case 'c':	getopt.next_arg (default_label); break;
      case 'd':	debug_flag++; break;
      case 'g': getopt.next_arg (grid_style); break;
      case 'h': getopt.next_arg (height); break;
      case 'l': getopt.next_arg (top_label); break;
      case 'm': getopt.next_arg (line_style); break;
      case 'r': getopt.next_arg (right); break;
      case 's': save_screen_flag++; break;
      case 't': transpose_axes_flag++; break;
      case 'u': getopt.next_arg (up); break;
      case 'v': cerr << "graph version 0.\n"; break;
      case 'w': getopt.next_arg (width); break;
      case 'x':
	while  (isalpha (getopt.first_char()))
	  {
	    switch (getopt.first_char())
	      {
	      case 'T': x_label_on_top++; getopt.optind++; break;
		      case 'B': x_label_on_top=0; getopt.optind++; break;
		      case 'l': x_log_scale++; getopt.optind++; break;
		      }
	  }
	getopt.next_arg (x_lower_limit); getopt.next_arg (x_upper_limit); break;
      case 'y':
	while  (isalpha (getopt.first_char()))
	  {
	    switch (getopt.first_char())
	      {
	      case 'L': y_label_on_right=0; getopt.optind++; break;
		      case 'R': y_label_on_right++; getopt.optind++; break;
		      case 'l': y_log_scale++; getopt.optind++; break;
		      }
	  }
	getopt.next_arg (y_lower_limit); getopt.next_arg (y_upper_limit); break;
      case 'z': no_standard_input++; break;
      case '?': error_occurred++;
      }
  if (error_occurred) {
    cerr << "usage" sp argv[0] sp usage_message;
    exit (-1);    
  }
				// Complain if the plot does not fits on page
  if (up < 0.) cerr <<
    "Warning: the plot may extend below the bottom of the page.\n";
  if (up + height > 1.) cerr <<
    "Warning: the plot may extend above the top of the page.\n";
  if (right < 0.) cerr <<
    "Warning: the plot may extend beyond the left edge of the page.\n";
  if (right + width > 1.) cerr <<
    "Warning: the plot may extend beyond the right edge of the page.\n";

  				// now we start reading in all the data.
  pointXPlex point;		// all the data is held in an array of points
  
				// read data from standard input.
  if (! no_standard_input)
      read_data (cin, "(stdard input)", point, abcissa_flag,
		 lower_limit, spacing, symbol_number,
		 input_data, switch_symbols);
  
				// read data files specified on command line.
  int i;
  for (i=getopt.optind; i<getopt.nargc; i++)
    {
      char *filename = getopt.nargv[i];
      ifstream input_file(filename);
      if (cin.readable ())
	  read_data (cin, filename, point, abcissa_flag,
		     lower_limit, spacing, symbol_number,
		     input_data, switch_symbols);
    }
  				// The data is read in. Look for limits.
  ePlotFile plot_file (fileno (stdout));
  if (!save_screen_flag)
    plot_file.erase ();

  if (point.length () <= 0)
    {				// Complain if there is no data.
      cerr << argv[0] << ": Warning, no data found in input files.\n";
      xmin = 0;
      ymin = 1;
      xmax = 0;
      ymax = 1;
    }
  else
    {				// if there is data...
      if (debug_flag)
	for (i = point.low (); i < point.fence (); point.next (i))
	  {
	    cerr << point[i].x sp point[i].y;
	    if (point[i].label)
	      cerr sp point[i].label;
	    cerr nl;
	  };
  
      if (transpose_axes_flag)
	{
	  String tmp;
	  tmp = y_label;
	  y_label = x_label;
	  x_label = tmp;
	  double t;
	  for (i = point.low (); i < point.fence (); point.next (i))
	    {
	      t = point[i].y;
	      point[i].y = point[i].x;
	      point[i].x = point[i].y;
	    }
	}
      // find the upper and lower limits
      // of the x any y coordinates.
      for (i = point.low (); i < point.fence (); point.next (i))
	{
	  if (xmin > point[i].x) xmin = point[i].x;
	  if (ymin > point[i].y) ymin = point[i].y;
	  if (xmax < point[i].x) xmax = point[i].x;
	  if (ymax < point[i].y) ymax = point[i].y;
	}
      // add margins beteen edges of the data and box if range is nonzero and
      // the scale is not logarithmic.
      if (!y_log_scale)
	{
	  double tmp = (ymax - ymin);
	  ymax += y_margin * tmp;
	  ymin -= y_margin * tmp;
	}
      if (!x_log_scale)
	{
	  double tmp = (xmax - xmin);
	  xmax += x_margin * tmp;
	  xmin -= x_margin * tmp;
	}
    }
  
  // use limits specified on the command line if present.
  if (x_lower_limit != HUGE) xmin = x_lower_limit;
  if (y_lower_limit != HUGE) ymin = y_lower_limit;
  if (x_upper_limit != HUGE) xmax = x_upper_limit;
  if (y_upper_limit != HUGE) ymax = y_upper_limit;
  
  // make sure that 0 is not in range if we are using a log scale.
  if (   (x_log_scale
	  && (xmin <= 0.)
	  && (xmax >= 0.))
      || (y_log_scale
	  && (ymin <= 0.)
	  && (ymax >= 0.)))
    {
      cerr << "the lower bound on x is" sp xmin nl;
      cerr << "the upper bound on x is" sp xmax nl;
      cerr << "the lower bound on y is" sp ymin nl;
      cerr << "the upper bound on y is" sp ymax nl;
      cerr << "Zero cannot lie between an upper and lower bound" nl
	"if you use a log scale." nl;
      exit (-1);
    }
  if (x_log_scale)
    {
      log_xmin = log (xmin);
      log_xmax = log (xmax);
    }
  if (y_log_scale)
    {
      log_ymin = log (ymin);
      log_ymax = log (ymax);
    }
  			// We have the limits, Now plot.
  plot_file.space (0, 0, plot_size, plot_size);
  // draw a box around the data.
  plot_file.linemod ("solid");
  if (grid_style)
    plot_file.box (px (0.), py (0.), px (1.), py (1.));
  
  char tick_label[32];	// tick lables are less than 16 digits long.
#ifdef _OLD_STREAMS
#define SET_TICK_LABEL(x) strcpy(tick_label, dtoa(x))
#else
  ostrstream tick_stream(tick_label, 32);
#define SET_TICK_LABEL(x) tick_stream.seekp(0), tick_stream << (x) << ends
#endif
  // draw x tick marks.
  if (grid_style)
    {
      // draw labels and ticks on x axis.
      double x_tick = x_log_scale
	? tick_interval (no_of_ticks, log10 (xmin), log10 (xmax))
	  : tick_interval (no_of_ticks, xmin, xmax);
      double x_tick_value;
      if (x_log_scale)
	x_tick_value = pow (10., x_tick
			    * (x_tick > 0.
			       ? ceil (log10 (xmin) * A_HAIR_MORE / x_tick)
			       : floor  (log10 (xmin) * A_HAIR_MORE / x_tick)));
      else
	x_tick_value = x_tick
	* (x_tick > 0. ? ceil (xmin * A_HAIR_MORE / x_tick)
	   : floor (xmin * A_HAIR_MORE / x_tick));
      while (x_tick_value <= xmax * A_HAIR_MORE)
	{			// tick marks on axes.
	  plot_file.line
	    (px (fx (x_tick_value)), py (0.),
	     px (fx (x_tick_value)), py (-1. * size_of_ticks));
	  plot_file.line
	    (px (fx (x_tick_value)), py (1.),
	     px (fx (x_tick_value)), py (1. + size_of_ticks));
	  SET_TICK_LABEL(x_tick_value);
	  plot_file.move
	    (px (fx (x_tick_value) - (extended_plot_format ? 0 :
				      .5 * char_width * strlen (tick_label))),
	     py ((x_label_on_top ? 1. : (extended_plot_format ? 0 : -1.
					 * char_height))
		 + (x_label_on_top ? 1. : -1.) * (0. >? size_of_ticks)));
	  if (extended_plot_format)
	    plot_file.alabel (CENTER_JUSTIFY, x_label_on_top ? BOTTOM_FLUSH
			      : TOP_FLUSH, tick_label);
	  else
	    plot_file.label (tick_label);
	  if (grid_style == 2)
	    {			// grid across box.
	      plot_file.linemod ("shortdashed");
	      plot_file.line
		(px (fx (x_tick_value)), py (0.),
		 px (fx (x_tick_value)), py (1.));
	      plot_file.linemod ("solid");
	    }
	  if ((.5 < fx (x_log_scale ? pow (10., log10 (x_tick_value) + x_tick)
			: x_tick_value + x_tick))
	      && x_label.length ())
	    {			// put the label between tick marks
	      plot_file.move
		(px (fx ((x_log_scale ? pow (10., log10 (x_tick_value)+x_tick/2.)
			  : x_tick_value + x_tick / 2.)
			 - (extended_plot_format ? 0 : 
			    .5 * char_width * x_label.length ()))),
		 py ((x_label_on_top ? 1. : (extended_plot_format ? 0 : -1.
					     * char_height)
		      + (x_label_on_top ? 1. : -1.) * (0. >? size_of_ticks))));
	      if (extended_plot_format)
		plot_file.alabel (CENTER_JUSTIFY, x_label_on_top ? BOTTOM_FLUSH
				  : TOP_FLUSH, x_label);
	      else
		plot_file.label (x_label);
	      x_label = "";
	    }
	  if (x_log_scale)
	    x_tick_value *= pow (10., x_tick);
	  else
	  x_tick_value += x_tick;
	  if (!x_log_scale && fabs (x_tick_value / x_tick) < 1e-7)
	    x_tick_value = 0.;
	}
      			// draw labels and ticks on y axis.
      double y_tick = tick_interval (no_of_ticks, ymin, ymax);
      double y_tick_value = y_tick
	* (y_tick > 0. ? ceil (ymin * A_HAIR_MORE / y_tick)
	   : floor (ymin * A_HAIR_MORE / y_tick));
      while (y_tick_value <= ymax * A_HAIR_MORE)
	{			// draw tick marks on axes
	  plot_file.line
	    (px (0.), py (fy (y_tick_value)),
	     px (-1. * size_of_ticks), py (fy (y_tick_value)));
	  plot_file.line
	    (px (1.), py (fy (y_tick_value)),
	     px (1. + size_of_ticks), py (fy (y_tick_value)));
	  SET_TICK_LABEL(y_tick_value);
	  plot_file.move
	    (px ((y_label_on_right ? 1. : (extended_plot_format ? 0 : -1.
					   * char_width * strlen(tick_label)))
		 + (y_label_on_right ? 1. : -1.) * (0. >? size_of_ticks)),
	     py (fy (y_tick_value) - .5
		 * (extended_plot_format ? 0 : char_height)));
	  if (extended_plot_format)
	    plot_file.alabel (y_label_on_right ? LEFT_JUSTIFY : RIGHT_JUSTIFY,
			      CENTER_FLUSH, tick_label);
	  else
	    {
	      SET_TICK_LABEL(y_tick_value);
	      plot_file.label (tick_label);
	    }
	  if (grid_style == 2)
	    {			// draw grid within box.
	      plot_file.linemod ("shortdashed");
	      plot_file.line
		(px (0.), py (fy (y_tick_value)),
		 px (1.), py (fy (y_tick_value)));
	      plot_file.linemod ("solid");
	    }
	  if ((.5 < fy (y_tick_value + y_tick))
	      && y_label.length ())
	    {			// put the label between tick marks
	      plot_file.move
		(px ((y_label_on_right ? 1. : (extended_plot_format ? 0 : -1.
					       * char_width * strlen (y_label)))
		     + (y_label_on_right ? 1. : -1.) * (0. >? size_of_ticks)),
		 py (fy (y_tick_value + y_tick / 2.)));
	      if (extended_plot_format)
		plot_file.alabel (y_label_on_right ? LEFT_JUSTIFY
				  : RIGHT_JUSTIFY, CENTER_FLUSH, y_label);
	      else
		plot_file.label (y_label);
	      y_label = "";
	    }
	  y_tick_value += y_tick;
	  if (fabs (y_tick_value / y_tick) < 1e-7) y_tick_value = 0.;
	}
      if (top_label.length ())	// put label above plot.
	{
	  plot_file.move
	    (px (.5 - (extended_plot_format ? 0 :
		       .5 * char_width * top_label.length ())),
	     py (1. + size_of_ticks));
	  if (extended_plot_format)
	    plot_file.alabel (CENTER_JUSTIFY, BOTTOM_FLUSH, top_label);
	  else
	    plot_file.label (top_label);
	}
    }
  
  if (line_style >= 0)		// set style of lines connecting data points.
    plot_file.linemod (line_style_name[line_style % no_of_line_styles]);
  
  				// draw all the points
  if (point.length () <= 0)
      return 0;			// exit if there is no data.
  i = point.low ();
  int move = 1;		// 1 means move to first point
  double prev_x = point[i].x;
  int clipped = 0;		// 1 means we were outside the box.
  if (line_style >= 0)		// line_style == -1 means omit lines between points
  while (i < point.fence ())
    {				// break line if flag set and x < last x
      if (point[i].x <  prev_x)
	{
	  if (switch_style)
	    {
		line_style = line_style++;
		plot_file.linemod (line_style_name[line_style % no_of_line_styles]);
	    }
	  if (break_flag)
	    move = 1;
	}
      if (in_box (point[i].x, point[i].y)) // clip out points outside the box
	{
	  if (move)
	    {
	      move = 0;			// only move once for the first point
		plot_file.move (px (fx (point[i].x)), py (fy (point[i].y)));
	    }
	  else
	      if (clipped)
		{
		  clipped = 0;
		  plot_file.move (px (clip (fx (point[i-1].x))),
				  py (clip (fy (point[i-1].y))));
	    plot_file.cont (px (fx (point[i].x)), py (fy (point[i].y)));
	}
	      else
		plot_file.cont (px (fx (point[i].x)), py (fy (point[i].y)));
	  }
	else
	  {
	    if (!clipped)
	      {
		clipped = 1;
		if (move)
		  {
		    move = 0;
		    plot_file.move (px (clip (fx (point[i].x))),
				    py (clip (fy (point[i].y))));
		  }
		else
		  plot_file.cont (px (clip (fx (point[i].x))),
				  py (clip (fy (point[i].y))));
	      }
	  }
	prev_x = point[i].x;
      point.next (i);
    }
  			// now draw all the symbols and data labels
  plot_file.linemod ("solid");
  for (i = point.low (); i < point.fence (); point.next (i))
    {
      if (in_box (point[i].x, point[i].y))
	{
	  if (point[i].label)
	    {
	      plot_file.move
		(px (fx (point[i].x)), py (fy (point[i].y)));
	      if (extended_plot_format)
		plot_file.alabel (CENTER_JUSTIFY, CENTER_FLUSH, point[i].label);
	      else
		plot_file.label (point[i].label);
	    }
	  else if (default_label.length ())
	    {
	      plot_file.move
		(px (fx (point[i].x)), py (fy (point[i].y)));
	      if (extended_plot_format)
		plot_file.alabel (CENTER_JUSTIFY, CENTER_FLUSH, default_label);
	      else
		plot_file.label (default_label);
	    }
	  if (point[i].symbol >= 0)
	    {
	      point[i].symbol %= no_of_symbols;
	      for (int j=0; (END != symbol[point[i].symbol][j].operation); j++)
		switch (symbol[point[i].symbol][j].operation)
		  {
		  case CONT:
		    plot_file.cont
		      (px (fx (point[i].x) 
			   + symbol_size * symbol[point[i].symbol][j].x),
		       py (fy (point[i].y)
			   + symbol_size * symbol[point[i].symbol][j].y));
		    break;
		  case MOVE:
		    plot_file.move
		      (px (fx (point[i].x)
			   + symbol_size * symbol[point[i].symbol][j].x),
		       py (fy (point[i].y)
			   + symbol_size * symbol[point[i].symbol][j].y));
		    break;
		  case CIRCLE:
		    plot_file.circle
		      (px (fx (point[i].x)), py (fy (point[i].y)),
		       (int) (plot_size * width * symbol_size
			      * symbol[point[i].symbol][j].x));
		    break;
		  case END:
		    ;
		  }
	    }
	}
    }
  return 0;
}
