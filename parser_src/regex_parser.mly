
%token <char> CHAR
%token <int> DIGITS
%token ALT
%token LPAR RPAR
%token LBRACK RBRACK COMMA
%token STAR PLUS QMARK
%token HAT DOLLAR BACKSL DOT
%token COLONS LESS EQUAL EXCL
%token WORDBOUND NONWORDBOUND
%token EOF

%start <Regex.raw_regex> main
%type  <Regex.raw_regex> pattern
%type  <Regex.raw_regex> disjunction
%type  <Regex.raw_regex> alternative
%type  <Regex.raw_regex> term
%type  <Regex.raw_regex> assertion
%type  <Regex.raw_regex> atom
%type  <Regex.quantifier> quantifier
%type  <Regex.counted_quantifier> counted_quantifier

%%



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
  | LBRACK d=DIGITS RBRACK { {min=d; max=Some d; greedy=true} }
  | LBRACK d=DIGITS RBRACK QMARK{ {min=d; max=Some d; greedy=false} }
  | LBRACK d=DIGITS COMMA RBRACK { {min=d; max=None; greedy=true} }
  | LBRACK d=DIGITS COMMA RBRACK QMARK { {min=d; max=None; greedy=false} }
  | LBRACK dmin=DIGITS COMMA dmax=DIGITS RBRACK { {min=dmin; max=Some dmax; greedy=true} }
  | LBRACK dmin=DIGITS COMMA dmax=DIGITS RBRACK QMARK { {min=dmin; max=Some dmax; greedy=false} }


atom:
  | c=CHAR { Raw_character(Char c) } /* todo: patterncharacter */
  | DOT { Raw_character(Dot) }
/* TODO: atom escape */
/* TODO: character class */
  | LPAR d=disjunction RPAR { Raw_capture d }
/* TODO: fail if there is a group specifier */
  | LPAR QMARK COLONS d=disjunction RPAR { d }
/* TODO */


%%
