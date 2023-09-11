
%token <char> CHAR
%token <char> DIGIT
%token ALT
%token LPAR RPAR
%token LBRAC RBRAC COMMA
%token LBRACK RBRACK
%token STAR PLUS QMARK
%token HAT DOLLAR BACKSL DOT
%token COLONS LESS MORE EQUAL MINUS EXCL
%token WORDBOUND NONWORDBOUND
%token DIGITCLASS NONDIGITCLASS
%token SPACECLASS NONSPACECLASS
%token WORDCLASS NONWORDCLASS
%token FORMFEED NEWLINE CARRIAGE TAB
%token NULL EOF

%start <Regex.raw_regex> main
%type  <string> decimaldigits
%type  <Regex.raw_regex> pattern
%type  <Regex.raw_regex> disjunction
%type  <Regex.raw_regex> alternative
%type  <Regex.raw_regex> term
%type  <Regex.raw_regex> assertion
%type  <Regex.raw_regex> atom
%type  <Regex.raw_regex> atomescape
%type  <Regex.quantifier> quantifier
%type  <Regex.counted_quantifier> counted_quantifier
%type  <Charclasses.char_group> characterclassescape
%type  <char> characterescape
%type  <char> controlescape
%type  <char> identityescape
%type  <Regex.character> characterclass
%type  <Charclasses.char_class> classcontents
%type  <Charclasses.char_class> nonemptyclassranges
%type  <Charclasses.char_class> nonemptyclassrangesnodash
%type  <char> classatomnodash
%type  <char> classatom

%%

decimaldigits:
  | d1=decimaldigits d2=DIGIT { d1 ^ String.make 1 d2 }
  | d=DIGIT { String.make 1 d }


/* https://tc39.es/ecma262/#sec-patterns */

main: r=pattern EOF { r }

pattern:
  | d=disjunction { d }

disjunction:
  | a=alternative { a }
  | a=alternative ALT d=disjunction { Raw_alt(a,d) }

alternative:
  | a=alternative t=term { Raw_con(a,t) }
  | t=term { t } /* differs from the spec here */
  | EOF { Raw_empty }

term:
  | a=assertion { a }
  | a=atom { a }
  | a=atom q=quantifier { Raw_quant(q,a) }
  | a=atom cq=counted_quantifier { Raw_count(cq,a) }
/* separation between usual quantifier and counted ones since they are separate in the OCaml engine AST. Could be changed if needed */

assertion:
  | HAT { Raw_anchor(BeginInput) }
  | DOLLAR { Raw_anchor(EndInput) }
  | WORDBOUND { Raw_anchor(WordBoundary) }
  | NONWORDBOUND { Raw_anchor(NonWordBoundary) }
  | LPAR QMARK EQUAL d=disjunction RPAR { Raw_lookaround(Lookahead, d) }
  | LPAR QMARK EXCL d=disjunction RPAR { Raw_lookaround(NegLookahead, d) }
  | LPAR QMARK LESS EQUAL d=disjunction RPAR { Raw_lookaround(Lookbehind, d) }
  | LPAR QMARK LESS EXCL d=disjunction RPAR { Raw_lookaround(NegLookbehind, d) }

quantifier:
  | STAR { Star }
  | STAR QMARK { LazyStar }
  | PLUS { Plus }
  | PLUS QMARK { LazyPlus }
  | QMARK { QuestionMark }
  | QMARK QMARK { LazyQuestionMark }

counted_quantifier:
  | LBRAC d=decimaldigits RBRAC { {min=int_of_string d; max=Some (int_of_string d); greedy=true} }
  | LBRAC d=decimaldigits RBRAC QMARK{ {min=int_of_string d; max=Some (int_of_string d); greedy=false} }
  | LBRAC d=decimaldigits COMMA RBRAC { {min=int_of_string d; max=None; greedy=true} }
  | LBRAC d=decimaldigits COMMA RBRAC QMARK { {min=int_of_string d; max=None; greedy=false} }
  | LBRAC dmin=decimaldigits COMMA dmax=decimaldigits RBRAC { {min=int_of_string dmin; max=Some (int_of_string dmax); greedy=true} }
  | LBRAC dmin=decimaldigits COMMA dmax=decimaldigits RBRAC QMARK { {min=int_of_string dmin; max=Some (int_of_string dmax); greedy=false} }

atom:
  | c=CHAR { Raw_character(Char c) }
/* also adding the tokens that can be parsed as simple characters */
  | COMMA { Raw_character(Char(',')) }
  | COLONS { Raw_character(Char(':')) }
  | LESS { Raw_character(Char('<')) }
  | MORE { Raw_character(Char('>')) }
  | EQUAL { Raw_character(Char('=')) }
  | MINUS { Raw_character(Char('-')) }
  | EXCL { Raw_character(Char('!')) }
  | NULL { Raw_character(Char(char_of_int 0)) }
  | d=DIGIT { Raw_character(Char(d)) }
/* TODO: { for instance can be parsed as single char. But not (. I'm not sure where this is in the spec. Also I'm not sure why, if I add a similar rule for LBRAC, it does not work */
  | DOT { Raw_character(Dot) }
  | a=atomescape { a }
  | c=characterclass { Raw_character c }
  | LPAR d=disjunction RPAR { Raw_capture d }
/* TODO: fail if there is a group specifier */
  | LPAR QMARK COLONS d=disjunction RPAR { d }

atomescape:
  | c=characterclassescape { Raw_character(Group c) }
  | c=characterescape { Raw_character(Char c) }

characterclassescape:
  | DIGITCLASS { Digit }
  | NONDIGITCLASS { NonDigit }
  | SPACECLASS { Space }
  | NONSPACECLASS { NonSpace }
  | WORDCLASS { Word }
  | NONWORDCLASS { NonWord }

characterescape:
  | c=controlescape { c }
  | BACKSL i=identityescape { i }

controlescape:
  | FORMFEED { '\x0C' }
  | NEWLINE { '\n' }
  | CARRIAGE { '\r' }
  | TAB { '\t' }

identityescape:
  | ALT { '|' }
  | LPAR { '(' }
  | RPAR { ')' }
  | LBRACK { '[' }
  | RBRACK { ']' }
  | LBRAC { '{' }
  | RBRAC { '}' }
  | COMMA { ',' }
  | STAR { '*' }
  | PLUS { '+' }
  | QMARK { '?' }
  | HAT { '^' }
  | DOLLAR { '$' }
  | BACKSL { '\\' }
  | DOT { '.' }
  | COLONS { ':' }
  | LESS { '<' }
  | MORE { '>' }
  | EQUAL { '=' }
  | MINUS { '-' }
  | EXCL { '!' }
  | c=CHAR { c }


characterclass:
  | LBRACK HAT c=classcontents RBRACK { NegClass c }
  | LBRACK c=classcontents RBRACK { Class c }

classcontents:
  | { [] }
  | n=nonemptyclassranges { n }

nonemptyclassranges:
  | g=characterclassescape  { [CGroup g] }
  | a=classatom { [CChar a] }
  | g=characterclassescape n=nonemptyclassrangesnodash { (CGroup g)::n }
  | a=classatom n=nonemptyclassrangesnodash { (CChar a)::n }
  | a1=classatom MINUS a2=classatom c=classcontents { CRange (a1,a2)::c }

nonemptyclassrangesnodash:
  | g=characterclassescape  { [CGroup g] }
  | a=classatom { [CChar a] }
  | g=characterclassescape n=nonemptyclassrangesnodash { (CGroup g)::n }
  | a=classatomnodash n=nonemptyclassrangesnodash { (CChar a)::n }
  | a1=classatomnodash MINUS a2=classatom c=classcontents { CRange (a1,a2)::c }


/* I'm removing the character groups \s \w... */
/* and making it a special rule */
/* otherwise I'm not sure how to type it for ranges */
/* Note that there might be a problem with e.g [\w-a] */

classatom:
  | MINUS { '-' }
  | c=classatomnodash { c }

classatomnodash:
  | c=controlescape { c }
  | BACKSL i=identityescape { i }
  | ALT { '|' }
  | LPAR { '(' }
  | RPAR { ')' }
  | LBRACK { '[' }
  | LBRAC { '{' }
  | RBRAC { '}' }
  | COMMA { ',' }
  | STAR { '*' }
  | PLUS { '+' }
  | QMARK { '?' }
  | HAT { '^' }
  | DOLLAR { '$' }
  | DOT { '.' }
  | COLONS { ':' }
  | LESS { '<' }
  | MORE { '>' }
  | EQUAL { '=' }
  | EXCL { '!' }
  | c=CHAR { c }




%%
