#pragma implementation
#include "ePlotFile.h"

ePlotFile& ePlotFile:: alabel (alabel_xadj x_adjust,
			       alabel_yadj y_adjust, char *s)
{
  cmd ('T');
  cmd (x_adjust);
  cmd (y_adjust);
  (PlotFile*)(this)->operator <<(s); // workaround -dl
  (PlotFile*)(this)->operator <<("\n");
  return *this;
};
