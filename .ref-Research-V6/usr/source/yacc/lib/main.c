main( argc, argv ) int argc; char *argv[]; {
  yyinit( argc, argv );
  if( yyparse() ) return;
  yyaccpt();
  }
