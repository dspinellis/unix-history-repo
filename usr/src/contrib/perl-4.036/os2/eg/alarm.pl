sub handler {
  local($sig) = @_;
  print "Caught a SIG$sig -- shutting down\n";
  exit(0);
}

$SIG{'ALRM'} = 'handler';
$SIG{'INT'} = 'handler';	# Ctrl-C pressed
$SIG{'BREAK'} = 'handler';	# Ctrl-Break pressed
$SIG{'TERM'} = 'handler';	# Killed by another process

print "Starting execution ...\n";
alarm(10);

while ( <> ) {
}
print "Normal exit.\n";
