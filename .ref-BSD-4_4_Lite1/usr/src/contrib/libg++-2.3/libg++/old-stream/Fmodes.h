
#ifndef _Fmodes_h
#ifdef __GNUG__
#pragma interface
#endif
#define _Fmodes_h 1

enum io_mode                    // known unix file IO modes
{
  io_readonly   = 0,            
  io_writeonly  = 1,
  io_readwrite  = 2, 
  io_appendonly = 3,
  io_append     = 4,            // append, plus allow reads
};

enum access_mode                // ways to open a file
{
  a_createonly  = 0,            // create, fail if file exists
  a_create      = 1,            // create if doesn't exist, else truncate
  a_useonly     = 2,            // use (no truncate)  fail if doesn't exist
  a_use         = 3,            // use (no truncate), create if doesn't exist
};

enum state_value                // File states
{ 
  _good         = 0,            // all is well
  _eof          = 1,            // at eof
  _fail         = 2,            // logical or physical IO error
  _bad          = 4             // unopened/corrupted
};

#endif
