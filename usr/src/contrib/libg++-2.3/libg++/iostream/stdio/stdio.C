#include <stdioprivate.h>

extern "C" void _cleanup() { streambuf::flush_all(); } // For GNU libc.
