/*
	tanh(arg) computes the hyperbolic tangent of its floating
	point argument.

	sinh and cosh are called except for large arguments, which
	would cause overflow improperly.
*/

double sinh(), cosh();

double
tanh(arg) double arg; {

	double sign;

	sign = 1.;
	if(arg < 0.){
		arg = -arg;
		sign = -1.;
	}

	if(arg > 21.){
		return(sign);
	}

	return(sign*sinh(arg)/cosh(arg));
}
