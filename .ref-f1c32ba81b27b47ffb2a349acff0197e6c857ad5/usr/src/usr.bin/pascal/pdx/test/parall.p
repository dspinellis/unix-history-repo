(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)parall.p	5.1 (Berkeley) %G%
 *)

program Parall(input,output);

{Declare constants for unit conversions, convergence tests, etc.}

const	SQRT2 = 1.4142136;	{Square root of 2}
	TWOPI = 6.2831853;	{Two times pi}
	MINALPHA = 0.001;	{Minimum y-step size}
	ROUGHLYZERO = 0.001;	{Approximation to zero for convergence}
	YTHRESHOLD = 40.0;	{Heuristic constant for thresholding}
	N = 8;			{Vector and matrix dimension}


{Declare types for circuit elements, vectors, and matrices.}

type	VSOURCE = record
			ampl: real;	 {Volts (peak)}
			freq: real;	 {Radians/second}
			xindex: integer;	{Index for x value}
			yindex: integer;	{Index for y value}
		  end;

	RLPAIR = record
			r: real;	 {Ohms}
			l: real;	 {Henries}
			islope: real;	 {Amps/second}
			invariant: real; {Trapezoidal invariant}
			lasttime: real;  {Most recent time}
			xindex: array [1..2] of integer;  {x value indices}
			yindex: array [1..2] of integer;  {y value indices}
		 end;

	CAPACITOR = record
			c: real;	 {Farads}
			vslope: real;	 {Volts/second}
			invariant: real; {Trapezoidal invariant}
			lasttime: real;  {Most recent time}
			xindex: array [1..2] of integer;  {x value indices}
			yindex: array [1..2] of integer;  {y value indices}
		    end;

	VECTOR = array [1..N] of real;

	MATRIX = array [1..N,1..N] of real;


{Declare global variables for central simulation information.}

var	ground: VSOURCE;	{Ground -- a fake source}
	itcount: integer;	{Main routine iteration count}
	update: integer;	{Update loop counter for main}
	optimcount: integer;	{Number of optimization steps}
	newtoncount: integer;	{Number of Newton steps}
	v1: VSOURCE;		{Voltage source V1}
	rl1: RLPAIR;		{R1/L1 resistor-inductor pair}
	rl2: RLPAIR;		{R2/L2 resistor-inductor pair}
	c1: CAPACITOR;		{Capacitor C1}
	a: MATRIX;		{Global matrix A}
	b: MATRIX;		{Global matrix B}
	jac: MATRIX;		{Global Jacobian matrix}
	x: VECTOR;		{Global vector of dependents}
	xnext: VECTOR;		{Next x-vector for simulation}
	y: VECTOR;		{Global vector of independents}
	ynext: VECTOR;		{Next y-vector for simulation}
	gradient: VECTOR;	{Global gradient vector}
	h: real;		{Time step value}
	time: real;		{Current time}
	lastychange: real;	{YStep's most recent y-change}
	timestep: integer;	{Current timestep number}
	maxsteps: integer;	{Number of time steps to run}
        oldxnorm: real;		{Old one-norm of x-vector}
	newxnorm: real;		{New one-norm of x-vector}
	closenough: boolean;   	{Flag to indicate convergence}




{The following procedure initializes everything for the program based
 on the little test circuit suggested by Sarosh.  The user is asked
 to specify the simulation and circuit parameters, and then the matrix
 and vector values are set up.}

procedure InitializeEverything;
var i,j: integer;
    rtemp: real;
begin

{Ready the input and output files (almost nil for Berkeley).}
writeln(output);
writeln(output,'*** Simulation Output Record ***');
writeln(output);
writeln(output);

{Initialize convergence test/indication variables.}
oldxnorm := 0.0;
newxnorm := 0.0;
lastychange := 0.0;

{Get desired time step size for simulation.}
readln(input,h);
writeln(output,'h (Seconds): ',h:12:7);

{Get desired number of time steps for simulation.}
readln(input,maxsteps);
writeln(output,'maxsteps: ',maxsteps:4);

{Get parameters for source V1 and initialize the source.}
with v1 do
   begin
   readln(input,rtemp);
   writeln(output,'V1 (Volts RMS): ',rtemp:9:3);
   ampl := rtemp * SQRT2;
   readln(input,rtemp);
   writeln(output,'f (Hertz): ',rtemp:9:3);
   freq := rtemp * TWOPI;
   xindex := 1;
   yindex := 1;
   end;

{Get parameters for R1/L1 pair and initialize the pair.}
with rl1 do
   begin
   readln(input,r);
   writeln(output,'R1 (Ohms): ',r:9:3);
   readln(input,l);
   writeln(output,'L1 (Henries): ',l:12:7);
   islope := 0.0;
   invariant := 0.0;
   lasttime := -1.0;        {Negative to force first update}
   xindex[1] := 2;
   yindex[1] := 2;
   xindex[2] := 3;
   yindex[2] := 3;
   end;

{Get parameters for R2/L2 pair and initialize the pair.}
with rl2 do
   begin
   readln(input,r);
   writeln(output,'R2 (Ohms): ',r:9:3);
   readln(input,l);
   writeln(output,'L2 (Henries): ',l:12:7);
   islope := 0.0;
   invariant := 0.0;
   lasttime := -1.0;	   {Negative to force first update}
   xindex[1] := 4;
   yindex[1] := 4;
   xindex[2] := 5;
   yindex[2] := 5;
   end;

{Get parameters for capacitor C1 and initialize the device.}
with c1 do
   begin
   readln(input,c);
   writeln(output,'C1 (Farads): ',c:12:7);
   vslope := 0.0;
   invariant := 0.0;
   lasttime := -1.0;	   {Negative to force first update}
   xindex[1] := 6;
   yindex[1] := 6;
   xindex[2] := 7;
   yindex[2] := 7;
   end;

{Initialize the ground node.}
with ground do
   begin
   ampl := 0.0;
   freq := 0.0;
   xindex := 8;
   yindex := 8;
   end;

{Zero out all the vectors and matrices.}
for i := 1 to N do
   begin
   x[i] := 0.0;
   y[i] := 0.0;
   for j := 1 to N do
      begin
      a[i,j] := 0.0;
      b[i,j] := 0.0;
      jac[i,j] := 0.0;
      end;
   end;

{Initialize the A matrix.}
a[1,2] := -1.0;
a[2,3] := 1.0;
a[2,4] := -1.0;
a[3,5] := 1.0;
a[4,7] := 1.0;
a[5,1] := 1.0;
a[7,6] := 1.0;
a[8,8] := 1.0;

{Initialize the B matrix.}
b[1,1] := 1.0;
b[3,7] := -1.0;
b[4,8] := -1.0;
b[5,2] := 1.0;
b[6,3] := 1.0;
b[6,4] := 1.0;
b[7,5] := 1.0;
b[8,6] := 1.0;

{Initialize the Jacobian matrix.}
rtemp := h / (2.0 * rl1.l  +  rl1.r * h);
jac[2,2] := rtemp;
jac[3,3] := rtemp;
jac[2,3] := -rtemp;
jac[3,2] := -rtemp;
rtemp := h / (2.0 * rl2.l  +  rl2.r * h);
jac[4,4] := rtemp;
jac[5,5] := rtemp;
jac[4,5] := -rtemp;
jac[5,4] := -rtemp;
jac[6,6] := -1.0;
jac[7,7] := 1.0;
jac[7,6] := h / (2.0 * c1.c);
end;




{The following procedure solves the equation Ax=b for an N x N system
 of linear equations, where A is the coefficient matrix, b is the
 right-hand-side vector, and x is the vector of unknowns.  Gaussian
 elimination with maximal column pivots is used.                     }

procedure Solve(a: MATRIX; b: VECTOR; var x: VECTOR);
var y,z: real;
    i,j,k,k1: integer;
begin
for k := 1 to N-1 do
   begin
   y := abs(a[k,k]);
   j := k;
   k1 := k + 1;
   for i := k1 to N do
      if abs(a[i,k]) > y then
         begin
         j := i;
         y := abs(a[i,k]);
         end;
   for i := k to N do
      begin
      y := a[k,i];
      a[k,i] := a[j,i];
      a[j,i] := y;
      end;
   y := b[k];
   b[k] := b[j];
   b[j] := y;
   z := a[k,k];
   for i := k1 to N do
      begin
      y := a[i,k] / z;
      a[i,k] := y;
      for j := k1 to N do a[i,j] := a[i,j]  -  y * a[k,j];
      b[i] := b[i] - y * b[k];
      end;
   end;
x[N] := b[N] / a[N,N];
for i := N-1 downto 1 do
   begin
   y := b[i];
   for j := i+1 to N do y := y  -  a[i,j] * x[j];
   x[i] := y / a[i,i];
   end;
end;


{The following procedure computes and returns a vector called "deltay",
 which is the change in the y-vector prescribed by the Newton-Rhapson
 algorithm.}

procedure NewtonStep(var deltay: VECTOR);
var phi: VECTOR;
    m: MATRIX;
    i,j,k: integer;
begin
for i := 1 to N do
   begin
   phi[i] := 0.0;
   for j := 1 to N do
      begin
      phi[i] := phi[i]  +  a[i,j] * y[j]  +  b[i,j] * x[j];
      m[i,j] := -a[i,j];
      for k := 1 to N do m[i,j] := m[i,j] - b[i,k] * jac[k,j];
      end;

   end;
Solve(m,phi,deltay);
end;




{The following function computes the value of theta, the objective
 function, given the x and y vectors.}

function ThetaValue(x,y: VECTOR): real;
var i,j: integer;
    phielem: real;
    theta: real;
begin
theta := 0.0;
for i:= 1 to N do
   begin
   phielem := 0.0;
   for j := 1 to N do
      phielem := phielem  +  a[i,j] * y[j]  +  b[i,j] * x[j];
   theta := theta  +  phielem * phielem;
   end;
ThetaValue := theta;
end;


{The following function computes the theta value associated with a
 proposed step of size alpha in the direction of the gradient.}

function Theta(alpha: real): real;
var ythere: VECTOR;
    i: integer;
begin
for i := 1 to N do
   ythere[i] := y[i]  -  alpha * gradient[i];
Theta := ThetaValue(x,ythere);
end;


{The following procedure computes the gradient of the objective 
 function (theta) with respect to the vector y.}

procedure ComputeGradient;
var i,j,k: integer;
    m: MATRIX;
    v: VECTOR;
begin
{Compute v = Ay + Bx and M = A' + J'B'.}
for i := 1 to N do
   begin
   v[i] := 0.0;
   for j := 1 to N do
      begin
      v[i] := v[i]  +  a[i,j] * y[j]  +  b[i,j] * x[j];
      m[i,j] := a[j,i];
      for k := 1 to N do
         m[i,j] := m[i,j]  +  jac[k,i] * b[j,k];
      end;
   end;
{Compute gradient = 2Mv.}
for i := 1 to N do
   begin
   gradient[i] := 0.0;
   for j := 1 to N do
      gradient[i] := gradient[i]  +  m[i,j] * v[j];
   gradient[i] := 2.0 * gradient[i];
   end;
end;


{The following procedure computes the bounds on alpha, the step size
 to take in the gradient direction.  The bounding is done according
 to an algorithm suggested in S.W.Director's text on circuits.}

procedure GetAlphaBounds(var lower,upper: real);
var alpha: real;
    oldtheta,newtheta: real;
begin
if ThetaValue(x,y) = 0.0
   then
      begin
      lower := 0;

      upper := 0;
      end
   else
      begin
      lower := MINALPHA;
      oldtheta := Theta(lower);
      upper := MINALPHA * 2.0;
      newtheta := Theta(upper);
      if newtheta <= oldtheta then
         begin
         alpha := upper;
         repeat
            begin
            oldtheta := newtheta;
            alpha := alpha * 2.0;
            newtheta := Theta(alpha);
            end
         until (newtheta > oldtheta);
         lower := alpha / 4.0;
         upper := alpha;
         end;
      end;
end;


{The following function computes the best value of alpha for the step
 in the gradient direction.  This best value is the value that minimizes
 theta along the gradient-directed path.}

function BestAlpha(lower,upper: real): real;
var alphaL,alphaU,alpha1,alpha2: real;
    thetaL,thetaU,theta1,theta2: real;

begin
alphaL := lower;
thetaL := Theta(alphaL);
alphaU := upper;
thetaU := Theta(alphaU);
alpha1 := 0.381966 * alphaU  +  0.618034 * alphaL;
theta1 := Theta(alpha1);
alpha2 := 0.618034 * alphaU  +  0.381966 * alphaL;
theta2 := Theta(alpha2);
repeat
   if theta1 < theta2
      then
         begin
         alphaU := alpha2;
         thetaU := theta2;
         alpha2 := alpha1;
         theta2 := theta1;
         alpha1 := 0.381966 * alphaU  +  0.618034 * alphaL;
         theta1 := Theta(alpha1);
         end
      else
         begin
         alphaL := alpha1;
         thetaL := theta1;
         alpha1 := alpha2;
         theta1 := theta2;
         alpha2 := 0.618034 * alphaU  +  0.381966 * alphaL;
         theta2 := Theta(alpha2);
         end
until abs(thetaU - thetaL) <= ROUGHLYZERO;
BestAlpha := (alphaL + alphaU) / 2.0;
end;


{The following procedure computes and returns a vector called "deltay",
 which is the change in the y-vector prescribed by the steepest-descent
 approach.}

procedure OptimizationStep(var deltay: VECTOR);
var lower,upper: real;
    alpha: real;
    i: integer;
begin
ComputeGradient;
GetAlphaBounds(lower,upper);
if lower <> upper then
   begin
   alpha := BestAlpha(lower,upper);
   for i:= 1 to N do deltay[i] := - alpha * gradient[i];
   end;
end;




{The following function computes the one-norm of a vector argument.
 The length of the argument vector is assumed to be N.}

function OneNorm(vec: VECTOR): real;
var sum: real;
    i: integer;
begin
sum := 0;
for i := 1 to N do  sum := sum + abs(vec[i]);
OneNorm := sum;
end;


{The following procedure takes a y-step, using the optimization
approach when far from the solution and the Newton-Rhapson approach
when fairly close to the solution.}

procedure YStep;
var deltay: VECTOR;
    ychange: real;
    scale: real;
    i: integer;
begin
NewtonStep(deltay);
ychange := OneNorm(deltay);
if ychange > YTHRESHOLD
   then
{
      begin
      OptimizationStep(deltay);
      ychange := OneNorm(deltay);
      if ychange > YTHRESHOLD then
}
         begin
         scale := YTHRESHOLD/ychange;
         for i := 1 to N do deltay[i] := scale * deltay[i];
	 optimcount := optimcount + 1;
	 end	 	{;}
{
      optimcount := optimcount + 1;
      end
}
   else
      begin
      newtoncount := newtoncount + 1;
      end;
for i := 1 to N do ynext[i] := y[i] + deltay[i];
end;




{The following procedure updates the output of a voltage source
 given the current time.}

procedure VsourceStep(vn: VSOURCE);
begin
with vn do
   xnext[xindex] := ampl * sin(freq * time);
end;


{The following procedure updates the outputs of a resistor-inductor
 pair given the time step to take...that is, this routine takes a
 time step for resistor-inductor pairs.  The new outputs are found
 by applying the trapezoidal rule.}

procedure RLPairStep(var rln: RLPAIR);
begin
with rln do
   begin
   if (time > lasttime) then
      begin
      lasttime := time;
      invariant := xnext[xindex[1]]  +  (h / 2.0) * islope;
      end;
   islope := (y[yindex[1]]  -  y[yindex[2]]  -  r * xnext[xindex[1]]) / l;
   xnext[xindex[1]] := invariant  +  (h / 2.0) * islope;
   xnext[xindex[2]] := - xnext[xindex[1]];
   end;
end;


{The following procedure updates the outputs of a capacitor given the 
 time step...it takes the time step using a trapezoidal rule iteration.}

procedure CapacitorStep(var cn: CAPACITOR);
var v: real;
begin
with cn do
   begin
   if (time > lasttime) then
      begin
      lasttime := time;
      v := xnext[xindex[2]]  -  y[yindex[2]];
      invariant := v  +  (h / 2.0) * vslope;
      end;
   vslope := y[yindex[1]] / c;
   v := invariant  +  (h / 2.0) * vslope;
   xnext[xindex[1]] := - y[yindex[1]];
   xnext[xindex[2]] := y[yindex[2]] + v;
   end;
end;


{The following procedure controls the overall x-step for the 
 specific circuit to be simulated.}

procedure XStep;
begin
VsourceStep(v1);
RLPairStep(rl1);
RLPairStep(rl2);
CapacitorStep(c1);
VsourceStep(ground);
end;




{The following procedure prints out the values of all the voltages and
 currents for the circuit at each time step.}

procedure PrintReport;
begin
writeln(output);
writeln(output);
writeln(output,'REPORT at Time = ',time:8:5,' seconds');
writeln(output,'Number of iterations used: ',itcount:2);
writeln(output,'Number of truncations: ',optimcount:2);
writeln(output,'Number of Newton y-steps: ',newtoncount:2);
writeln(output,'The voltages and currents are:');
writeln(output,'  V1 = ',x[1]:12:7,'   I1 = ',y[1]:12:7);
writeln(output,'  V2 = ',y[2]:12:7,'   I2 = ',x[2]:12:7);
writeln(output,'  V3 = ',y[3]:12:7,'   I3 = ',x[3]:12:7);
writeln(output,'  V4 = ',y[4]:12:7,'   I4 = ',x[4]:12:7);
writeln(output,'  V5 = ',y[5]:12:7,'   I5 = ',x[5]:12:7);
writeln(output,'  V6 = ',x[7]:12:7,'   I6 = ',y[6]:12:7);
writeln(output,'  V7 = ',y[7]:12:7,'   I7 = ',x[6]:12:7);
writeln(output,'  V8 = ',x[8]:12:7,'   I8 = ',y[8]:12:7);
end;




{Finally, the main routine controls the whole simulation process.}

begin
InitializeEverything;
PrintReport;
for timestep := 1 to maxsteps do
   begin
   itcount := 0;
   optimcount := 0;
   newtoncount := 0;
   closenough := FALSE;
   time := h * timestep;
   repeat
      begin
      itcount := itcount + 1;
      YStep;
      XStep;
      for update := 1 to N do
	 begin
	 x[update] := xnext[update];
	 y[update] := ynext[update];
	 end;
      oldxnorm := newxnorm;
      newxnorm := OneNorm(x);
      if abs(newxnorm - oldxnorm) <= ROUGHLYZERO 
         then closenough := TRUE;
      end;
      if itcount < 4 then closenough := FALSE;
   until (itcount = 25) or closenough;
   PrintReport;
   end;
end.

