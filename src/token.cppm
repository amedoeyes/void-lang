export module voidlang.token;

import std;
import lexer;

export namespace voidlang {

enum class token_type : std::uint8_t {
	ampersand,
	arrow,
	assignment,
	asterisk,
	caret,
	colon,
	comma,
	hyphen,
	logical_and,
	logical_or,
	lparen,
	pipe,
	plus,
	question_mark,
	rparen,
	semicolon,
	slash,

	identifier,

	kw_let,

	bt_i8,
	bt_i16,
	bt_i32,
	bt_i64,
	bt_f32,
	bt_f64,
	bt_void,

	lit_integer,
	lit_float,

	unknown,
	eof,
};

using token = lexer::token<token_type>;

auto token_name(token_type token) -> std::string {
	switch (token) {
		using enum token_type;

		case ampersand:   return "ampersand";
		case arrow:       return "arrow";
		case assignment:  return "assignment";
		case asterisk:    return "asterisk";
		case caret:       return "caret";
		case colon:       return "colon";
		case comma:       return "comma";
		case hyphen:      return "hyphen";
		case logical_and: return "logical_and";
		case logical_or:  return "logical_or";
		case lparen:      return "lparen";
		case pipe:        return "pipe";
		case plus:        return "plus";
		case rparen:      return "rparen";
		case semicolon:   return "semicolon";
		case slash:       return "slash";

		case identifier:  return "identifier";

		case kw_let:      return "kw_let";

		case bt_i8:       return "bt_i8";
		case bt_i16:      return "bt_i16";
		case bt_i32:      return "bt_i32";
		case bt_i64:      return "bt_i64";
		case bt_f32:      return "bt_f32";
		case bt_f64:      return "bt_f64";
		case bt_void:     return "bt_void";

		case lit_integer: return "lit_integer";
		case lit_float:   return "lit_float";

		case unknown:     return "unknown";
		case eof:         return "eof";
	}
}

}  // namespace voidlang
