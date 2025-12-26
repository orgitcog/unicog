#include "tree.h"
#include <boost/spirit/include/classic_core.hpp>
#include <mutex>
#include <thread>

namespace {
using namespace boost::spirit::classic;
using std::string;
using namespace opencog;

// THREAD-SAFETY FIX: Replaced global variables with thread-local storage
// This prevents race conditions when multiple threads use the tree parser
thread_local tree<string> tr;
thread_local tree<string>::iterator at = tr.begin();

// Thread-safe mutex for any shared operations
static std::mutex tree_parser_mutex;

void begin_internal(const char* from, const char* to)
{
    std::lock_guard<std::mutex> lock(tree_parser_mutex);
    at = tr.empty()
        ? tr.insert(at,string(from, to-1))
        : tr.append_child(at,string(from, to-1));
}

void end_internal(const char)
{
    std::lock_guard<std::mutex> lock(tree_parser_mutex);
    at = tr.parent(at);
}

void add_leaf(const char* from, const char* to)
{
    std::lock_guard<std::mutex> lock(tree_parser_mutex);
    if (tr.empty())
        at = tr.insert(at, string(from, to));
    else
        tr.append_child(at, string(from, to));
}

struct TreeGrammar : public grammar<TreeGrammar>
{
    std::vector<string> v;

    template<typename ScannerT>
    struct definition
    {
        definition(const TreeGrammar&)
        {
            // IMPROVED PARSING: Replaced hack with proper message parsing
            // This handles quoted strings more robustly and avoids parsing issues
            term =
                lexeme_d[
                    // Proper message parsing with quoted strings
                    ( str_p("message:") >> ch_p('"')
                      >> *(anychar_p - ch_p('"')) >> ch_p('"'))
                    | // Regular term parsing (non-parenthesis, non-space characters)
                      (+( anychar_p - ch_p('(') - ch_p(')') - space_p))
                ][&add_leaf];
            beg =
                lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p)) >> '('];
            expr =
                (beg[&begin_internal] >> +expr >> ch_p(')')[&end_internal]) |
                term;
        }
        rule<ScannerT> expr, beg, term;

        const rule<ScannerT>& start() const { return expr; }
    };
};

} // ~namespace

namespace std {

std::istream& operator>>(std::istream& in, opencog::tree<std::string>& t)
{
    t.clear();
    std::string str, tmp;
    int nparen=0;
    do {
        // Replaced by getline so that a message i.e. "yo  man" isn't
        // replaced by "yo man" (where a space is missing)
        // This assumes that there are no '(' and ')' in the quoted string.
        std::getline(in, tmp);
        nparen += count(tmp.begin(), tmp.end(), '(')
                 - count(tmp.begin(), tmp.end(), ')');
        str += tmp + ' ';
    } while (in.good() && nparen>0);

    if (nparen != 0) {
        std::stringstream stream (std::stringstream::out);
        stream << "Error: Paren mismatch parsing tree: '"
               << str << "'" << std::endl;
        throw InconsistenceException(TRACE_INFO, "tree - %s.",
                                     stream.str().c_str());
    }
    // Whitespace normalization: we must ignore whitespace after a function
    // name but before a parenthesis to handle various formatting styles.
    // Example: "and  ($1 $2)" is normalized to "and($1 $2)" for consistent parsing.
    // This preprocessing ensures the parser can handle loosely formatted input.
    int sz = str.length();
    int i=0, j = 0;
    while (i<sz and (str[i] == ' ' or str[i] == '\t')) i++;
    while (i<sz) {
       int ix = i;
       // Skip all whitespace that precedes an open paren.
       while (str[ix] == ' ' or str[ix] == '\t') ix++;
       if (str[ix] == '(') {
          i = ix;

          // If there is nothing but whitespace until the close paren,
          // then drop both the open and the close parens.  The operator
          // is effectively child-less in this situation.  If we don't
          // do this, then the parser goes bonkers and decides that the
          // operator is a child of itself.  For example, "+()" parses
          // to become "+(+)" which is incorrect. (Yes, some operators
          // can be zero-ary).
          ix++;
          while (str[ix] == ' ' or str[ix] == '\t') ix++;
          if (str[ix] == ')') i = ++ix;
       }
       str[j] = str[i];
       i++; j++;
    }
    str[j] = 0x0; // null terminate

    // parse_string_tree(str);
    TreeGrammar tg;
    tr.clear();
    at = tr.begin();
    parse(str.c_str(), tg, space_p);

    t = tr;
    tr.clear();

    return in;
}

} // ~namespace opencog
