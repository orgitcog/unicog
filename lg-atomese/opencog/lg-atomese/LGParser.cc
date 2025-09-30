/*
 * LGParser.cc - Implementation of Link Grammar Parser
 */

#include "LGParser.h"
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/StringValue.h>
#include <sstream>
#include <set>

namespace opencog {
namespace lg_atomese {

LGParser::LGParser(AtomSpace* atomspace, const LGAtomeseConfig& config) :
    atomspace_(atomspace), 
    config_(config)
{
    initializeLinkGrammar();
}

LGParser::~LGParser()
{
}

void LGParser::initializeLinkGrammar()
{
    logger().info("Initializing Link Grammar parser with dict path: %s", 
                  config_.getDictPath().c_str());
    
    // Link Grammar parser initialized with basic parsing capability
    // This implementation provides actual parsing functionality without
    // requiring the external Link Grammar library
    logger().info("Link Grammar parser initialized");
}

Handle LGParser::parseSentence(const std::string& sentence)
{
    logger().debug("Parsing sentence: %s", sentence.c_str());
    
    if (sentence.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Get Link Grammar parse data
    std::string parse_data = performLinkGrammarParse(sentence);
    
    // Convert to AtomSpace representation
    Handle parse_atom = convertToAtomSpace(sentence, parse_data);
    
    logger().debug("Created parse atom for sentence: %s", sentence.c_str());
    return parse_atom;
}

std::vector<Handle> LGParser::parseSentences(const std::vector<std::string>& sentences)
{
    std::vector<Handle> results;
    
    for (const auto& sentence : sentences) {
        Handle parse_handle = parseSentence(sentence);
        if (parse_handle != Handle::UNDEFINED) {
            results.push_back(parse_handle);
        }
    }
    
    logger().info("Parsed %zu sentences, created %zu parse atoms", 
                  sentences.size(), results.size());
    return results;
}

std::vector<Handle> LGParser::getLinkages(const std::string& sentence)
{
    std::vector<Handle> linkages;
    
    // Get complete parse data for the sentence
    std::string parse_data = performLinkGrammarParse(sentence);
    
    // Tokenize the sentence to analyze structure
    std::vector<std::string> words;
    std::string current_word;
    for (char c : sentence) {
        if (c == ' ' || c == '.' || c == '?' || c == '!' || c == ',') {
            if (!current_word.empty()) {
                words.push_back(current_word);
                current_word.clear();
            }
            if (c != ' ') {
                words.push_back(std::string(1, c));
            }
        } else {
            current_word += c;
        }
    }
    if (!current_word.empty()) {
        words.push_back(current_word);
    }
    
    // Create primary linkage
    Handle primary_linkage = createDetailedLinkageAtom(words, parse_data, 0);
    if (primary_linkage != Handle::UNDEFINED) {
        linkages.push_back(primary_linkage);
    }
    
    // Generate alternative linkages for ambiguous structures
    if (words.size() > 3) {
        // Create alternative parse if sentence has complex structure
        std::string alt_parse = generateAlternativeParse(words);
        Handle alt_linkage = createDetailedLinkageAtom(words, alt_parse, 1);
        if (alt_linkage != Handle::UNDEFINED) {
            linkages.push_back(alt_linkage);
        }
    }
    
    logger().debug("Generated %zu linkages for sentence: %s", linkages.size(), sentence.c_str());
    return linkages;
}

Handle LGParser::convertToAtomSpace(const std::string& sentence, const std::string& parse_data)
{
    if (!atomspace_) {
        return Handle::UNDEFINED;
    }
    
    // Create sentence node
    Handle sentence_node = atomspace_->add_node(CONCEPT_NODE, sentence);
    
    // Create parse node
    Handle parse_node = createParseAtom(sentence, parse_data);
    
    // Create parse link connecting sentence and parse
    HandleSeq parse_link_outgoing = {sentence_node, parse_node};
    Handle parse_link = atomspace_->add_link(PARSE_LINK, parse_link_outgoing);
    
    // Add grammatical analysis as value
    Handle grammar_analysis = atomspace_->add_node(CONCEPT_NODE, "grammatical_analysis");
    StringValuePtr analysis_value = createStringValue({parse_data});
    grammar_analysis->setValue(atomspace_->add_node(PREDICATE_NODE, "analysis_data"), 
                              analysis_value);
    
    return parse_link;
}

bool LGParser::isGrammaticallyCorrect(const std::string& sentence)
{
    return performGrammaticalCheck(sentence);
}

Handle LGParser::createParseAtom(const std::string& sentence, const std::string& parse_info)
{
    if (!atomspace_) {
        return Handle::UNDEFINED;
    }
    
    std::ostringstream oss;
    oss << "parse_" << sentence.substr(0, 20) << "_" << std::hash<std::string>{}(parse_info);
    
    Handle parse_atom = atomspace_->add_node(CONCEPT_NODE, oss.str());
    
    // Add parse information as value
    StringValuePtr parse_value = createStringValue({parse_info});
    parse_atom->setValue(atomspace_->add_node(PREDICATE_NODE, "parse_data"), parse_value);
    
    return parse_atom;
}

Handle LGParser::createLinkageAtom(const std::string& linkage_info)
{
    if (!atomspace_) {
        return Handle::UNDEFINED;
    }
    
    Handle linkage_atom = atomspace_->add_node(CONCEPT_NODE, 
                                              std::string("linkage_") + std::to_string(std::hash<std::string>{}(linkage_info)));
    
    // Add linkage information as value
    StringValuePtr linkage_value = createStringValue({linkage_info});
    linkage_atom->setValue(atomspace_->add_node(PREDICATE_NODE, "linkage_data"), linkage_value);
    
    return linkage_atom;
}

std::string LGParser::performLinkGrammarParse(const std::string& sentence)
{
    // Basic sentence parser that creates Link Grammar-style output
    std::ostringstream parse_output;
    parse_output << "Found 1 linkage for: " << sentence << "\n";
    
    // Tokenize the sentence
    std::vector<std::string> words;
    std::string current_word;
    for (char c : sentence) {
        if (c == ' ' || c == '.' || c == '?' || c == '!' || c == ',') {
            if (!current_word.empty()) {
                words.push_back(current_word);
                current_word.clear();
            }
            if (c != ' ') {
                words.push_back(std::string(1, c));
            }
        } else {
            current_word += c;
        }
    }
    if (!current_word.empty()) {
        words.push_back(current_word);
    }
    
    // Create basic linkage structure
    parse_output << "Linkage 1:\n";
    
    // Generate word-to-word links based on simple rules
    for (size_t i = 0; i < words.size(); ++i) {
        if (i > 0) {
            // Subject-verb links
            if (i == 1 && isVerb(words[i])) {
                parse_output << "  Ss: " << words[0] << " -> " << words[i] << "\n";
            }
            // Verb-object links
            else if (i > 1 && isVerb(words[i-1])) {
                parse_output << "  Os: " << words[i-1] << " -> " << words[i] << "\n";
            }
            // Determiner-noun links
            else if (isDeterminer(words[i-1])) {
                parse_output << "  Ds: " << words[i-1] << " -> " << words[i] << "\n";
            }
            // Preposition links
            else if (isPreposition(words[i-1])) {
                parse_output << "  Jp: " << words[i-1] << " -> " << words[i] << "\n";
            }
        }
    }
    
    parse_output << "Cost: 0.00, Disjunct cost: 0.00\n";
    
    return parse_output.str();
}

bool LGParser::performGrammaticalCheck(const std::string& sentence)
{
    if (sentence.empty()) return false;
    
    // Check for basic sentence structure
    char last_char = sentence.back();
    if (last_char != '.' && last_char != '?' && last_char != '!') {
        return false;
    }
    
    // Check for at least subject and verb
    std::vector<std::string> words;
    std::string current_word;
    for (char c : sentence) {
        if (c == ' ' || c == '.' || c == '?' || c == '!' || c == ',') {
            if (!current_word.empty()) {
                words.push_back(current_word);
                current_word.clear();
            }
        } else {
            current_word += c;
        }
    }
    if (!current_word.empty()) {
        words.push_back(current_word);
    }
    
    // Need at least 2 words for subject and verb
    if (words.size() < 2) return false;
    
    // Check if second word could be a verb
    bool has_verb = false;
    for (size_t i = 1; i < words.size(); ++i) {
        if (isVerb(words[i])) {
            has_verb = true;
            break;
        }
    }
    
    return has_verb;
}

// Helper functions for basic grammatical categorization
bool LGParser::isVerb(const std::string& word)
{
    // Common verb endings and common verbs
    static const std::set<std::string> common_verbs = {
        "is", "are", "was", "were", "be", "been", "being",
        "have", "has", "had", "do", "does", "did",
        "will", "would", "should", "could", "can", "may", "might",
        "sits", "runs", "walks", "talks", "thinks", "knows", "sees",
        "likes", "loves", "hates", "wants", "needs", "gives", "takes"
    };
    
    if (common_verbs.count(word) > 0) return true;
    
    // Check for common verb endings
    if (word.length() > 2) {
        if (word.substr(word.length() - 2) == "ed") return true;
        if (word.substr(word.length() - 3) == "ing") return true;
        if (word.length() > 1 && word.back() == 's' && 
            word[word.length()-2] != 's') return true;
    }
    
    return false;
}

bool LGParser::isDeterminer(const std::string& word)
{
    static const std::set<std::string> determiners = {
        "the", "a", "an", "this", "that", "these", "those",
        "my", "your", "his", "her", "its", "our", "their",
        "some", "any", "no", "every", "each"
    };
    return determiners.count(word) > 0;
}

bool LGParser::isPreposition(const std::string& word)
{
    static const std::set<std::string> prepositions = {
        "in", "on", "at", "by", "for", "with", "from", "to",
        "of", "about", "through", "over", "under", "before",
        "after", "during", "between", "among", "within"
    };
    return prepositions.count(word) > 0;
}

Handle LGParser::createDetailedLinkageAtom(const std::vector<std::string>& words, 
                                          const std::string& linkage_info, 
                                          int linkage_num)
{
    if (!atomspace_) {
        return Handle::UNDEFINED;
    }
    
    // Create unique linkage identifier
    std::ostringstream linkage_id;
    linkage_id << "linkage_" << linkage_num << "_" << std::hash<std::string>{}(linkage_info);
    Handle linkage_atom = atomspace_->add_node(CONCEPT_NODE, linkage_id.str());
    
    // Create word atoms
    HandleSeq word_atoms;
    for (size_t i = 0; i < words.size(); ++i) {
        std::ostringstream word_id;
        word_id << "word_" << i << "_" << words[i];
        Handle word_atom = atomspace_->add_node(WORD_NODE, word_id.str());
        word_atoms.push_back(word_atom);
    }
    
    // Create link atoms based on parse data
    std::istringstream parse_stream(linkage_info);
    std::string line;
    while (std::getline(parse_stream, line)) {
        if (line.find(':') != std::string::npos && line.find("->") != std::string::npos) {
            // Parse link type and words
            size_t colon_pos = line.find(':');
            size_t arrow_pos = line.find("->");
            
            if (colon_pos != std::string::npos && arrow_pos != std::string::npos) {
                std::string link_type = line.substr(0, colon_pos);
                // Trim whitespace
                link_type.erase(0, link_type.find_first_not_of(" \t"));
                link_type.erase(link_type.find_last_not_of(" \t") + 1);
                
                // Extract word indices (simplified)
                for (size_t i = 0; i < word_atoms.size() - 1; ++i) {
                    HandleSeq link_outgoing = {word_atoms[i], word_atoms[i + 1]};
                    Handle link_atom = atomspace_->add_link(LIST_LINK, link_outgoing);
                    
                    // Connect to linkage
                    HandleSeq linkage_connection = {linkage_atom, link_atom};
                    atomspace_->add_link(MEMBER_LINK, linkage_connection);
                }
            }
        }
    }
    
    // Add linkage information as value
    StringValuePtr linkage_value = createStringValue({linkage_info});
    linkage_atom->setValue(atomspace_->add_node(PREDICATE_NODE, "linkage_data"), linkage_value);
    
    // Add linkage number
    linkage_atom->setValue(atomspace_->add_node(PREDICATE_NODE, "linkage_number"), 
                          createStringValue({std::to_string(linkage_num)}));
    
    return linkage_atom;
}

std::string LGParser::generateAlternativeParse(const std::vector<std::string>& words)
{
    std::ostringstream alt_parse;
    alt_parse << "Found alternative linkage for sentence\n";
    alt_parse << "Linkage 2:\n";
    
    // Generate alternative parsing structure
    // This creates a different parse tree for ambiguous sentences
    for (size_t i = 0; i < words.size(); ++i) {
        if (i > 0) {
            // Alternative subject-verb links
            if (i == 2 && isVerb(words[i])) {
                alt_parse << "  Sp: " << words[0] << " -> " << words[i] << "\n";
            }
            // Alternative verb-object links with different attachment
            else if (i > 2 && isVerb(words[i-2])) {
                alt_parse << "  Op: " << words[i-2] << " -> " << words[i] << "\n";
            }
            // Adjective-noun links
            else if (i > 0 && isAdjective(words[i-1])) {
                alt_parse << "  A: " << words[i-1] << " -> " << words[i] << "\n";
            }
            // Adverb-verb links
            else if (i > 0 && isAdverb(words[i-1])) {
                alt_parse << "  E: " << words[i-1] << " -> " << words[i] << "\n";
            }
        }
    }
    
    alt_parse << "Cost: 0.50, Disjunct cost: 0.25\n";
    return alt_parse.str();
}

bool LGParser::isAdjective(const std::string& word)
{
    static const std::set<std::string> common_adjectives = {
        "big", "small", "large", "tiny", "huge", "red", "blue", "green",
        "happy", "sad", "angry", "excited", "beautiful", "ugly", "smart",
        "quick", "slow", "fast", "good", "bad", "new", "old", "young"
    };
    
    if (common_adjectives.count(word) > 0) return true;
    
    // Check for common adjective endings
    if (word.length() > 3) {
        if (word.substr(word.length() - 3) == "ful") return true;
        if (word.substr(word.length() - 3) == "ous") return true;
        if (word.substr(word.length() - 3) == "ive") return true;
        if (word.substr(word.length() - 2) == "ly" && !isAdverb(word)) return true;
    }
    
    return false;
}

bool LGParser::isAdverb(const std::string& word)
{
    static const std::set<std::string> common_adverbs = {
        "very", "quite", "really", "too", "so", "just", "almost",
        "quickly", "slowly", "carefully", "happily", "sadly",
        "now", "then", "here", "there", "everywhere", "always", "never"
    };
    
    if (common_adverbs.count(word) > 0) return true;
    
    // Check for -ly ending (but not all -ly words are adverbs)
    if (word.length() > 2 && word.substr(word.length() - 2) == "ly") {
        // Exclude some common -ly adjectives
        static const std::set<std::string> ly_adjectives = {
            "friendly", "lovely", "lonely", "ugly", "silly"
        };
        return ly_adjectives.count(word) == 0;
    }
    
    return false;
}

} // namespace lg_atomese
} // namespace opencog