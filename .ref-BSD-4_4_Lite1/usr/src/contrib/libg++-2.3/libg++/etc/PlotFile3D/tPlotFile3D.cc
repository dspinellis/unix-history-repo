#include <PlotFile3D.h>
main()
{
  PlotFile3D foo("test.pl");
  foo.space(-1000,-1000,-1000,1000,1000,1000).erase();
      
  // Draw and label
  foo.linemod("longdashed").box(-1000,-1000,-1500,1000,1000,0);
  foo.linemod("solid").sphere(0,0,500,0,0,500,30);
  foo.linemod("dotted").circle(0,0,500,0,0,800,50);
  char title[] = "Test of PlotFile3D package";
  foo.home().label(title);
  
}
