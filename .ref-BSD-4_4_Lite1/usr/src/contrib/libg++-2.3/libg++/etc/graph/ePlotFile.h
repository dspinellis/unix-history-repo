
#ifndef ePlotFile_h
#pragma interface
#define ePlotFile_h 1

#include <PlotFile.h>

// ePlotFile is an extended plot file class which has adjusted labels.

// put the left center or right edge of the text at the current point.
typedef enum alabel_xadj
{ LEFT_JUSTIFY = 'l', CENTER_JUSTIFY = 'c', RIGHT_JUSTIFY = 'r'};

// put the top center or bottom edge of the text at the current point.
typedef enum alabel_yadj
{ BOTTOM_FLUSH = 'b', CENTER_FLUSH = 'c', TOP_FLUSH = 't' };

class ePlotFile : public PlotFile
{
public:
            ePlotFile() : PlotFile() {}
#ifndef _OLD_STREAMS
	    ePlotFile(int fd) : PlotFile(fd) { }
	    ePlotFile(const char *name, int mode=ios::out, int prot=0664)
		: PlotFile(name, mode, prot) { }
#else
            ePlotFile(const char* filename, io_mode m, access_mode a)
              :PlotFile(filename, m, a) {}
            ePlotFile(const char* filename, const char* m)
              :PlotFile(filename, m) {}
            ePlotFile(int filedesc, io_mode m = io_writeonly)
              :PlotFile(filedesc, m) {}
            ePlotFile(FILE* fileptr) : PlotFile(fileptr) {}
#endif

  ePlotFile& alabel (alabel_xadj x_adjust,
		     alabel_yadj y_adjust, char *s);
};

#endif
