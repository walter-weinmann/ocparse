main()
{
   yyparse();
   return 0;
}

yyerror(msg)
   char *msg;
{
   extern long yypos;

   printf("line %d: %s\n", yypos, msg);
   exit(1);
}

yywrap()
{
   return 1;
}
