/* File to handle NSE brain-damage.  */

#include <std.h>
#include <stream.h>

extern "C" {
  void nse_depinfo_cpp_add(const char *);
  void nse_depinfo_cpp_done();
  void nse_depinfo_ld_add(const char *);
  void nse_depinfo_ld_done();
  void nse_depinfo_comp_add(const char *);
  void nse_depinfo_comp_done();
  void nse_makestate_cpp_add(const char *);
  void nse_makestate_cpp_done();
  void nse_makestate_ld_add(const char *);
  void nse_makestate_ld_done();
  void nse_makestate_comp_add(const char *);
  void nse_makestate_comp_done();
}

int main(int argc, char *argv[]) {

  for (int i=0;  i < argc;  i++) {
//    cerr << argv[i] << "\n";
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
	case 'f':
	case 'W':
	  break;
	case 'I':
	  nse_depinfo_cpp_add(argv[i]);  // DO FOR EACH HEADER
      }
    }
    else {
    }
  };

  nse_depinfo_cpp_done();		// DO WHEN ALL IS DONE

  argv[0] = "g++";
  execvp(argv[0], argv);
}
