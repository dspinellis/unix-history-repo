/*
 * floor - calculate largest integer not greater than d.
 */
double   modf();
double
floor(d)
double d;
   {
   double fract;

   if (d<0.0) {
      d = -d;
      fract = modf(d, &d);
      if (fract != 0.0)
         d += 1;
      d = -d;
   } else
      modf(d, &d);
   return(d);
   }

/*
 * ceil - calculate smallest integer not less than d.
 */
double
ceil(d)
double d;
   {
   return(-floor(-d));
   }
