export module voidlang:token;

import std;
import lexer;

export namespace voidlang {

enum class token_type : std::int8_t {
	ampersand,
	ampersand_ampersand,
	ampersand_equal,
	asterisk,
	asterisk_equal,
	bang_equal,
	brace_left,
	brace_right,
	caret,
	caret_equal,
	colon,
	comma,
	equal,
	equal_equal,
	greater_than,
	greater_than_equal,
	greater_than_greater_than,
	greater_than_greater_than_equal,
	hyphen,
	hyphen_equal,
	hyphen_greater_than,
	hyphen_hyphen,
	less_than,
	less_than_equal,
	less_than_less_than,
	less_than_less_than_equal,
	paren_left,
	paren_right,
	percent,
	percent_equal,
	pipe,
	pipe_equal,
	pipe_pipe,
	plus,
	plus_equal,
	plus_plus,
	question_mark,
	semicolon,
	slash,
	slash_equal,

	kw_let,
	kw_if,
	kw_else,
	kw_return,

	bt_bool,
	bt_i8,
	bt_i16,
	bt_i32,
	bt_i64,
	bt_f32,
	bt_f64,
	bt_void,

	identifier,

	lit_bool,
	lit_integer,
	lit_float,

	eof,

	invalid,
};

using token = lexer::token<token_type>;

auto token_name(token_type token) -> std::string {
	switch (token) {
		using enum token_type;

		case ampersand:                       return "ampersand";
		case ampersand_ampersand:             return "ampersand_ampersand";
		case ampersand_equal:                 return "ampersand_equal";
		case asterisk:                        return "asterisk";
		case asterisk_equal:                  return "asterisk_equal";
		case bang_equal:                      return "bang_equal";
		case brace_left:                      return "brace_left";
		case brace_right:                     return "brace_right";
		case caret:                           return "caret";
		case caret_equal:                     return "caret_equal";
		case colon:                           return "colon";
		case comma:                           return "comma";
		case equal:                           return "equal";
		case equal_equal:                     return "equal_equal";
		case greater_than:                    return "greater_than";
		case greater_than_equal:              return "greater_than_equal";
		case greater_than_greater_than:       return "greater_than_greater_than";
		case greater_than_greater_than_equal: return "greater_than_greater_than_equal";
		case hyphen:                          return "hyphen";
		case hyphen_equal:                    return "hyphen_equal";
		case hyphen_greater_than:             return "hyphen_greater_than";
		case hyphen_hyphen:                   return "hyphen_hyphen";
		case less_than:                       return "less_than";
		case less_than_equal:                 return "less_than_equal";
		case less_than_less_than:             return "less_than_less_than";
		case less_than_less_than_equal:       return "less_than_less_than_equal";
		case paren_left:                      return "paren_left";
		case paren_right:                     return "paren_right";
		case percent:                         return "percent";
		case percent_equal:                   return "percent_equal";
		case pipe:                            return "pipe";
		case pipe_equal:                      return "pipe_equal";
		case pipe_pipe:                       return "pipe_pipe";
		case plus:                            return "plus";
		case plus_equal:                      return "plus_equal";
		case plus_plus:                       return "plus_plus";
		case question_mark:                   return "question_mark";
		case semicolon:                       return "semicolon";
		case slash:                           return "slash";
		case slash_equal:                     return "slash_equal";

		case identifier:                      return "identifier";

		case kw_let:                          return "kw_let";
		case kw_if:                           return "kw_if";
		case kw_else:                         return "kw_else";
		case kw_return:                       return "kw_return";

		case bt_bool:                         return "bt_bool";
		case bt_i8:                           return "bt_i8";
		case bt_i16:                          return "bt_i16";
		case bt_i32:                          return "bt_i32";
		case bt_i64:                          return "bt_i64";
		case bt_f32:                          return "bt_f32";
		case bt_f64:                          return "bt_f64";
		case bt_void:                         return "bt_void";

		case lit_bool:                        return "lit_bool";
		case lit_integer:                     return "lit_integer";
		case lit_float:                       return "lit_float";

		case invalid:                         return "invalid";
		case eof:                             return "eof";
	}
}

}  // namespace voidlang
