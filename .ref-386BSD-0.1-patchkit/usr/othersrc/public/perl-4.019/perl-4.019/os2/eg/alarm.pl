sub handler {
  local($sig) = @_;
  print "Caught a SIG$sig -- shutting down\n";
  exit(0);
}

$SIG{'INT'} = 'handler';
$SIG{'QUIT'} = 'handler';
$SIG{'ALRM'} = 'handler';

print "Starting execution ...\n";
alarm(10);

while ( <> ) {
}
print "Normal exit.\n";
