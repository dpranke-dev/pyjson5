%whitespace    = ' '
               | '\t'
               | '\n'
               | '\r'
               | '\v'
               | '\f'
               | '\xa0'
               | '\u2028'
               | '\u2029'
               | '\ufeff'
               | \p{Zs}

%comment       = '//' [^\r\n]* | '/*' ^.'*/'

%tokens = ident num_literal string

%externs       = strict

grammar        = value end                        -> $1

value          = 'null'                           -> 'None'
               | 'true'                           -> 'True'
               | 'false'                          -> 'False'
               | num_literal                      -> ['number', $1]
               | object                           -> ['object', $1]
               | array                            -> ['array', $1]
               | string                           -> ['string', $1]

object         = '{' member_list '}'              -> $2
               | '{' '}'                          -> []

array          = '[' element_list ']'             -> $2
               | '[' ']'                          -> []

string         = squote sqchar* squote            -> cat($2)
               | dquote dqchar* dquote            -> cat($2)

sqchar         = bslash esc_char                  -> $2
               | bslash eol                       -> ''
               | ~bslash ~squote ~eol any         -> $4
               | ?{ !strict } '\x00'..'\x1f'

dqchar         = bslash esc_char                  -> $2
               | bslash eol                       -> ''
               | ~bslash ~dquote ~eol any         -> $4
               | ?{ !strict } '\x00'..'\x1f'

bslash         = '\\'

squote         = "'"

dquote         = '"'

eol            = '\r' '\n'
               | '\r'
               | '\n'
               | '\u2028'
               | '\u2029'

esc_char       = 'b'                              -> '\b'
               | 'f'                              -> '\f'
               | 'n'                              -> '\n'
               | 'r'                              -> '\r'
               | 't'                              -> '\t'
               | 'v'                              -> '\v'
               | squote                           -> "'"
               | dquote                           -> '"'
               | bslash                           -> '\\'
               | ~('x' | 'u' | digit | eol) any   -> $2
               | '0' ~digit                       -> '\x00'
               | hex_esc
               | unicode_esc

hex_esc        = 'x' hex{2}                       -> xtou(cat($2))

unicode_esc    = 'u' hex{4}                       -> xtou(cat($2))

element_list   = value (',' value)* ','?          -> cons($1, $2)

member_list    = member (',' member)* ','?        -> cons($1, $2)

member         = string ':' value                 -> [$1, $3]
               | ident ':' value                  -> [$1, $3]

ident          = id_start id_continue*            -> cat(cons($1, $2))

id_start       = ascii_id_start
               | other_id_start
               | bslash unicode_esc

ascii_id_start = 'a'..'z' | 'A'..'Z' | '$' | '_'

other_id_start = \p{Ll}
               | \p{Lm}
               | \p{Lo}
               | \p{Lt}
               | \p{Lu}
               | \p{Nl}

id_continue    = ascii_id_start
               | digit
               | other_id_start
               | \p{Mn}
               | \p{Mc}
               | \p{Nd}
               | \p{Pc}
               | bslash unicode_esc
               | '\u200c'
               | '\u200d'

num_literal    = '-' num_literal                  -> strcat('-', $2)
               | '+' num_literal                  -> $2
               | dec_literal ~id_start            -> $1
               | hex_literal                      -> $1
               | 'Infinity'                       -> 'Infinity'
               | 'NaN'                            -> 'NaN'

dec_literal    = <dec_int_lit frac? exp?>
               | <frac exp?>

dec_int_lit    = '0' ~digit | nonzerodigit digit*

digit          = '0'..'9'

nonzerodigit   = '1'..'9'

hex_literal    = ('0x' | '0X') hex+               -> strcat('0x', cat($2))

hex            = 'a'..'f' | 'A'..'F' | digit

frac           = '.' digit+

exp            = ('e' | 'E') ('+' | '-')? digit+
