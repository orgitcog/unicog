/*
 * LGParser.cc - Link Grammar Parser implementation
 */

#include "LGParser.h"
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <chrono>
#include <algorithm>
#include <cctype>
#include <sstream>
#include <regex>

// Link Grammar includes - these would be available when LG is installed
#ifdef HAVE_LINK_GRAMMAR
extern "C" {
#include <link-grammar/link-includes.h>
}
#else
#include <cstring> // for strcmp
#endif

namespace opencog {
namespace main {

LGParser::LGParser(AtomSpace* atomspace, const std::string& dict_path) :
    atomspace_(atomspace),
    dictionary_language_(dict_path),
    parser_initialized_(false),
    max_linkages_(100),
    max_parse_time_(30),
    min_null_count_(0),
    use_wsd_(false),
    total_parse_count_(0),
    total_parse_time_(0.0),
    lg_dictionary_(nullptr),
    lg_options_(nullptr)
{
    parser_initialized_ = initializeParser(dict_path);
}

LGParser::~LGParser()
{
    // Clean up parser resources
#ifdef HAVE_LINK_GRAMMAR
    if (lg_options_) {
        parse_options_delete(lg_options_);
        lg_options_ = nullptr;
    }
    if (lg_dictionary_) {
        dictionary_delete(lg_dictionary_);
        lg_dictionary_ = nullptr;
    }
#else
    // Clean up stub resources (fallback when Link Grammar unavailable)
    if (lg_dictionary_) {
        delete static_cast<int*>(lg_dictionary_);
        lg_dictionary_ = nullptr;
    }
    if (lg_options_) {
        delete static_cast<int*>(lg_options_);
        lg_options_ = nullptr;
    }
#endif
}

bool LGParser::initializeParser(const std::string& dict_path)
{
    logger().info("Initializing Link Grammar parser with dictionary: %s", dict_path.c_str());
    
    try {
#ifdef HAVE_LINK_GRAMMAR
        // Initialize Link Grammar with the specified dictionary
        lg_dictionary_ = dictionary_create_lang(dict_path.c_str());
        if (!lg_dictionary_) {
            logger().error("Failed to create Link Grammar dictionary for language: %s", dict_path.c_str());
            return false;
        }
        
        // Create parse options
        lg_options_ = parse_options_create();
        if (!lg_options_) {
            dictionary_delete(lg_dictionary_);
            lg_dictionary_ = nullptr;
            logger().error("Failed to create Link Grammar parse options");
            return false;
        }
        
        // Set parse options
        parse_options_set_max_parse_time(lg_options_, max_parse_time_);
        parse_options_set_linkage_limit(lg_options_, max_linkages_);
        parse_options_set_min_null_count(lg_options_, min_null_count_);
        parse_options_set_max_null_count(lg_options_, 999);
        parse_options_set_short_length(lg_options_, 10);
        parse_options_set_all_short_connectors(lg_options_, 1);
        parse_options_set_verbosity(lg_options_, 0);
        
        // Extract language from dictionary info
        const char* dict_version = dictionary_get_version(lg_dictionary_);
        if (dict_version) {
            std::string version_str(dict_version);
            if (version_str.find("English") != std::string::npos) {
                dictionary_language_ = "en";
            } else if (version_str.find("German") != std::string::npos) {
                dictionary_language_ = "de";
            } else if (version_str.find("Russian") != std::string::npos) {
                dictionary_language_ = "ru";
            } else {
                // Try to extract from path
                if (dict_path.find("en") != std::string::npos) {
                    dictionary_language_ = "en";
                } else if (dict_path.find("de") != std::string::npos) {
                    dictionary_language_ = "de";
                } else if (dict_path.find("ru") != std::string::npos) {
                    dictionary_language_ = "ru";
                } else {
                    dictionary_language_ = "unknown";
                }
            }
        }
#else
        // Fallback stub implementation when Link Grammar library is not available
        // These stubs maintain API compatibility while indicating library absence
        lg_dictionary_ = new int(1); // Stub: minimal allocation for compatibility
        lg_options_ = new int(2);    // Stub: minimal allocation for compatibility
        
        logger().warn("Link Grammar library not available. Using fallback parser with limited functionality.");
        
        // Verify dictionary language from path
        if (dict_path == "en" || dict_path.find("en/") != std::string::npos) {
            dictionary_language_ = "en";
        } else if (dict_path == "de" || dict_path.find("de/") != std::string::npos) {
            dictionary_language_ = "de";
        } else if (dict_path == "ru" || dict_path.find("ru/") != std::string::npos) {
            dictionary_language_ = "ru";
        } else {
            dictionary_language_ = "en"; // Default to English
        }
#endif
        
        logger().info("Link Grammar parser initialized successfully for language: %s", 
                     dictionary_language_.c_str());
        return true;
    } catch (const std::exception& e) {
        logger().error("Failed to initialize Link Grammar parser: %s", e.what());
        return false;
    }
}

ParseResult LGParser::parseSentence(const std::string& sentence)
{
    return parseSentenceWithOptions(sentence, max_linkages_, max_parse_time_);
}

ParseResult LGParser::parseSentenceWithOptions(const std::string& sentence,
                                              int max_linkages,
                                              int max_parse_time)
{
    ParseResult result;
    
    if (!parser_initialized_ || !atomspace_) {
        logger().error("Parser not initialized or AtomSpace not available");
        return result;
    }
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    // Create sentence node
    result.sentence_node = atomspace_->add_node(SENTENCE_NODE, sentence);
    
    // Tokenize the sentence
    std::vector<std::string> words = tokenize(sentence);
    
    // Create unique parse identifier
    size_t sentence_hash = std::hash<std::string>{}(sentence);
    std::string parse_id = "parse_" + std::to_string(sentence_hash);
    
    // Create parse node
    result.parse_node = atomspace_->add_node(PARSE_NODE, parse_id);
    
    // Create parse link
    Handle parse_link = atomspace_->add_link(PARSE_LINK, 
        HandleSeq{result.sentence_node, result.parse_node});
    
    // Create word instances
    result.word_instances = createWordInstances(words, sentence_hash);
    
    // Add word instances to parse
    for (const auto& wi : result.word_instances) {
        atomspace_->add_link(EVALUATION_LINK, HandleSeq{
            atomspace_->add_node(PREDICATE_NODE, "_word_instance"),
            result.parse_node,
            wi
        });
    }
    
    // Simulate Link Grammar parsing to get linkage information
    result.link_instances = simulateLGParsing(words);
    
    // Create linguistic links
    result.linkages = createLinguisticLinks(result.word_instances, result.link_instances);
    
    // Extract and add POS tags
    std::map<std::string, std::string> pos_tags = extractPOSTags(words);
    for (size_t i = 0; i < words.size() && i < result.word_instances.size(); ++i) {
        auto pos_it = pos_tags.find(words[i]);
        if (pos_it != pos_tags.end()) {
            atomspace_->add_link(INHERITANCE_LINK, HandleSeq{
                result.word_instances[i],
                atomspace_->add_node(DEFINED_LINGUISTIC_CONCEPT_NODE, pos_it->second)
            });
        }
    }
    
    // Calculate parse confidence based on various factors
    result.parse_confidence = 0.85; // Base confidence
    if (result.linkages.size() > 0) {
        // Adjust confidence based on linkage quality
        result.parse_confidence += 0.1 * std::min(1.0, result.linkages.size() / 10.0);
    }
    result.num_linkages = result.linkages.size();
    
    // Set parse metadata
    parse_link->setTruthValue(SimpleTruthValue::createTV(
        result.parse_confidence, 0.9)); // strength, confidence
    
    // Add parse metadata as values
    StringValuePtr metadata = createStringValue({
        "language:" + dictionary_language_,
        "word_count:" + std::to_string(words.size()),
        "linkage_count:" + std::to_string(result.num_linkages),
        "parse_status:complete"
    });
    result.parse_node->setValue(atomspace_->add_node(PREDICATE_NODE, "parse_metadata"), metadata);
    
    // Update statistics
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    double parse_time = duration.count() / 1000.0;
    
    total_parse_count_++;
    total_parse_time_ += parse_time;
    
    logger().debug("Parsed sentence '%s' in %.3f seconds, created %zu linkages",
                  sentence.c_str(), parse_time, result.linkages.size());
    
    return result;
}

std::vector<std::string> LGParser::tokenize(const std::string& text)
{
    std::vector<std::string> tokens;
    std::string current_token;
    
    // Simple tokenization - can be enhanced with proper NLP tokenizer
    for (char c : text) {
        if (std::isspace(c)) {
            if (!current_token.empty()) {
                tokens.push_back(current_token);
                current_token.clear();
            }
        } else if (std::ispunct(c) && c != '\'' && c != '-') {
            // Split punctuation as separate tokens (except apostrophes and hyphens)
            if (!current_token.empty()) {
                tokens.push_back(current_token);
                current_token.clear();
            }
            tokens.push_back(std::string(1, c));
        } else {
            current_token += c;
        }
    }
    
    if (!current_token.empty()) {
        tokens.push_back(current_token);
    }
    
    return tokens;
}

std::vector<Handle> LGParser::createWordInstances(const std::vector<std::string>& words,
                                                 size_t sentence_hash)
{
    std::vector<Handle> word_instances;
    
    for (size_t i = 0; i < words.size(); ++i) {
        // Create word node
        Handle word_node = atomspace_->add_node(WORD_NODE, words[i]);
        
        // Create unique word instance
        std::string instance_name = words[i] + "@" + std::to_string(sentence_hash) + 
                                  "-" + std::to_string(i);
        Handle word_instance = atomspace_->add_node(WORD_INSTANCE_NODE, instance_name);
        
        // Link word to its instance
        atomspace_->add_link(REFERENCE_LINK, HandleSeq{word_instance, word_node});
        
        // Add word sequence information
        Handle word_seq = atomspace_->add_link(WORD_SEQUENCE_LINK, HandleSeq{
            word_instance,
            atomspace_->add_node(NUMBER_NODE, std::to_string(i))
        });
        
        word_instances.push_back(word_instance);
    }
    
    return word_instances;
}

std::map<std::string, std::vector<std::pair<int, int>>> LGParser::simulateLGParsing(
    const std::vector<std::string>& words)
{
    std::map<std::string, std::vector<std::pair<int, int>>> linkage_info;
    
#ifdef HAVE_LINK_GRAMMAR
    if (!lg_dictionary_ || !lg_options_) {
        logger().error("Link Grammar parser not properly initialized");
        return linkage_info;
    }
    
    // Build sentence string from words
    std::string sentence;
    for (const auto& word : words) {
        if (!sentence.empty()) sentence += " ";
        sentence += word;
    }
    
    // Create Link Grammar sentence object
    Sentence sent = sentence_create(sentence.c_str(), lg_dictionary_);
    if (!sent) {
        logger().error("Failed to create Link Grammar sentence");
        return linkage_info;
    }
    
    // Parse the sentence
    int num_linkages = sentence_parse(sent, lg_options_);
    
    if (num_linkages > 0) {
        // Use the first linkage (highest quality)
        Linkage linkage = linkage_create(0, sent, lg_options_);
        
        if (linkage) {
            int num_links = linkage_get_num_links(linkage);
            
            // Extract link information
            for (int i = 0; i < num_links; i++) {
                // Get link endpoints
                int left_word = linkage_get_link_lword(linkage, i);
                int right_word = linkage_get_link_rword(linkage, i);
                
                // Get link label and type
                const char* label = linkage_get_link_label(linkage, i);
                const char* llabel = linkage_get_link_llabel(linkage, i);
                const char* rlabel = linkage_get_link_rlabel(linkage, i);
                
                // Use the main label
                std::string link_type = label ? label : "UNKNOWN";
                
                // Store the link (adjusting for word indices)
                linkage_info[link_type].push_back({left_word, right_word});
            }
            
            // Add wall links if present
            int num_words = linkage_get_num_words(linkage);
            if (num_words > 0) {
                // Check for LEFT-WALL
                const char* first_word = linkage_get_word(linkage, 0);
                if (first_word && strcmp(first_word, "LEFT-WALL") == 0) {
                    linkage_info["W"].push_back({-1, 1}); // LEFT-WALL to first real word
                }
                
                // Check for RIGHT-WALL
                const char* last_word = linkage_get_word(linkage, num_words - 1);
                if (last_word && strcmp(last_word, "RIGHT-WALL") == 0) {
                    linkage_info["RW"].push_back({num_words - 2, num_words - 1}); // last real word to RIGHT-WALL
                }
            }
            
            linkage_delete(linkage);
        }
    } else {
        logger().warn("No valid linkages found for sentence: %s", sentence.c_str());
    }
    
    sentence_delete(sent);
    
#else
    // Fallback: Simple heuristic-based parsing for common patterns when LG not available
    for (size_t i = 0; i < words.size(); ++i) {
        std::string word = words[i];
        std::transform(word.begin(), word.end(), word.begin(), ::tolower);
        
        // Subject-verb links
        if (i + 1 < words.size()) {
            if ((word == "the" || word == "a" || word == "an") && i + 2 < words.size()) {
                // Determiner-noun link
                linkage_info["D"].push_back({i, i + 1});
                
                // Look for subject-verb
                if (i + 3 < words.size()) {
                    std::string verb = words[i + 2];
                    std::transform(verb.begin(), verb.end(), verb.begin(), ::tolower);
                    if (verb == "is" || verb == "are" || verb == "was" || verb == "were" ||
                        verb == "has" || verb == "have" || verb == "had") {
                        linkage_info["S"].push_back({i + 2, i + 1}); // verb to subject
                    }
                }
            }
            
            // Direct verb-object links
            if (word == "is" || word == "are" || word == "was" || word == "were" ||
                word == "has" || word == "have" || word == "had" ||
                word.size() > 2 && (word.substr(word.size()-2) == "ed" || 
                                   word.substr(word.size()-1) == "s")) {
                if (i + 1 < words.size() && words[i+1] != "." && words[i+1] != "," && words[i+1] != "!") {
                    linkage_info["O"].push_back({i, i + 1}); // verb to object
                }
            }
            
            // Preposition links
            if (word == "on" || word == "in" || word == "at" || word == "of" || 
                word == "to" || word == "from" || word == "with" || word == "by") {
                if (i > 0 && i + 1 < words.size()) {
                    linkage_info["J"].push_back({i - 1, i}); // prep attaches to previous
                    linkage_info["P"].push_back({i, i + 1}); // prep to object
                }
            }
            
            // Adjective-noun links
            if (i + 1 < words.size() && words[i+1] != "." && words[i+1] != ",") {
                // Simple heuristic: if current word ends in common adjective suffixes
                if (word.size() > 3 && (word.substr(word.size()-2) == "ly" ||
                                       word.substr(word.size()-3) == "ful" ||
                                       word.substr(word.size()-3) == "ous" ||
                                       word.substr(word.size()-3) == "ive")) {
                    linkage_info["A"].push_back({i, i + 1});
                }
            }
        }
    }
    
    // Add some basic connectivity links for parsing coherence
    if (words.size() >= 2) {
        // Wall links (sentence boundaries)
        linkage_info["W"].push_back({-1, 0}); // LEFT-WALL to first word
        linkage_info["RW"].push_back({words.size() - 1, words.size()}); // last word to RIGHT-WALL
    }
#endif
    
    return linkage_info;
}

std::vector<Handle> LGParser::createLinguisticLinks(
    const std::vector<Handle>& word_instances,
    const std::map<std::string, std::vector<std::pair<int, int>>>& linkage_info)
{
    std::vector<Handle> linguistic_links;
    
    for (const auto& [link_type, connections] : linkage_info) {
        for (const auto& [from_idx, to_idx] : connections) {
            // Skip wall links for now (indices -1 or >= word_instances.size())
            if (from_idx < 0 || to_idx < 0 || 
                from_idx >= static_cast<int>(word_instances.size()) || 
                to_idx >= static_cast<int>(word_instances.size())) {
                continue;
            }
            
            // Create LG link instance node
            std::string link_instance_name = link_type + "@" + 
                std::to_string(word_instances[from_idx]->get_hash()) + "-" +
                std::to_string(word_instances[to_idx]->get_hash());
            
            Handle lg_link_instance = atomspace_->add_node(LG_LINK_INSTANCE_NODE, link_instance_name);
            
            // Create evaluation link representing the linguistic relation
            Handle eval_link = atomspace_->add_link(EVALUATION_LINK, HandleSeq{
                atomspace_->add_node(LG_LINK_NODE, link_type),
                atomspace_->add_link(LIST_LINK, HandleSeq{
                    word_instances[from_idx],
                    word_instances[to_idx]
                })
            });
            
            // Add truth value indicating link confidence
            eval_link->setTruthValue(SimpleTruthValue::createTV(0.8, 0.9));
            
            // Associate LG link instance with the evaluation
            atomspace_->add_link(ASSOCIATIVE_LINK, HandleSeq{lg_link_instance, eval_link});
            
            linguistic_links.push_back(eval_link);
        }
    }
    
    return linguistic_links;
}

std::map<std::string, std::string> LGParser::extractPOSTags(const std::vector<std::string>& words)
{
    std::map<std::string, std::string> pos_tags;
    
#ifdef HAVE_LINK_GRAMMAR
    // When Link Grammar is available, we can extract POS from the parse
    // This would ideally be done during the parsing phase to avoid re-parsing
    // For now, we'll use a hybrid approach with enhanced heuristics
    
    // Map Link Grammar connector types to POS tags
    // This is a simplified mapping - real implementation would be more comprehensive
    auto mapLGConnectorToPOS = [](const std::string& connector) -> std::string {
        if (connector.find("S") != std::string::npos) return "VB";  // Subject link = verb
        if (connector.find("O") != std::string::npos) return "VB";  // Object link = verb
        if (connector.find("D") != std::string::npos) return "DT";  // Determiner
        if (connector.find("A") != std::string::npos) return "JJ";  // Adjective
        if (connector.find("J") != std::string::npos) return "IN";  // Preposition junction
        if (connector.find("M") != std::string::npos) return "VB";  // Modal verb
        if (connector.find("I") != std::string::npos) return "VBG"; // -ing verb
        if (connector.find("P") != std::string::npos) return "VBD"; // Past participle
        if (connector.find("G") != std::string::npos) return "VBG"; // Gerund
        return "NN"; // Default to noun
    };
#endif
    
    // Enhanced rule-based POS tagging with better coverage
    for (const std::string& word : words) {
        std::string lower_word = word;
        std::transform(lower_word.begin(), lower_word.end(), lower_word.begin(), ::tolower);
        
        // Determiners
        if (lower_word == "the" || lower_word == "a" || lower_word == "an" ||
            lower_word == "this" || lower_word == "that" || lower_word == "these" ||
            lower_word == "those" || lower_word == "some" || lower_word == "any" ||
            lower_word == "all" || lower_word == "every" || lower_word == "each") {
            pos_tags[word] = "DT";
        }
        // Personal pronouns
        else if (lower_word == "i" || lower_word == "you" || lower_word == "he" ||
                 lower_word == "she" || lower_word == "it" || lower_word == "we" ||
                 lower_word == "they" || lower_word == "me" || lower_word == "him" ||
                 lower_word == "her" || lower_word == "us" || lower_word == "them") {
            pos_tags[word] = "PRP";
        }
        // Possessive pronouns
        else if (lower_word == "my" || lower_word == "your" || lower_word == "his" ||
                 lower_word == "her" || lower_word == "its" || lower_word == "our" ||
                 lower_word == "their" || lower_word == "mine" || lower_word == "yours" ||
                 lower_word == "ours" || lower_word == "theirs") {
            pos_tags[word] = "PRP$";
        }
        // Modal verbs
        else if (lower_word == "can" || lower_word == "could" || lower_word == "may" ||
                 lower_word == "might" || lower_word == "must" || lower_word == "shall" ||
                 lower_word == "should" || lower_word == "will" || lower_word == "would") {
            pos_tags[word] = "MD";
        }
        // Be verbs
        else if (lower_word == "is" || lower_word == "are" || lower_word == "was" ||
                 lower_word == "were" || lower_word == "am" || lower_word == "been" ||
                 lower_word == "be" || lower_word == "being") {
            pos_tags[word] = "VB";
        }
        // Have verbs
        else if (lower_word == "have" || lower_word == "has" || lower_word == "had" ||
                 lower_word == "having") {
            pos_tags[word] = "VB";
        }
        // Common verbs
        else if (lower_word == "do" || lower_word == "does" || lower_word == "did" ||
                 lower_word == "done" || lower_word == "doing" || lower_word == "make" ||
                 lower_word == "makes" || lower_word == "made" || lower_word == "making" ||
                 lower_word == "go" || lower_word == "goes" || lower_word == "went" ||
                 lower_word == "gone" || lower_word == "going") {
            pos_tags[word] = "VB";
        }
        // -ing verbs (gerunds/continuous)
        else if (word.size() > 3 && word.substr(word.size()-3) == "ing") {
            pos_tags[word] = "VBG";
        }
        // Past tense verbs
        else if (word.size() > 2 && word.substr(word.size()-2) == "ed") {
            pos_tags[word] = "VBD";
        }
        // Present tense verbs (3rd person singular)
        else if (word.size() > 1 && word[word.size()-1] == 's' && 
                 (word.size() < 2 || word[word.size()-2] != 's')) {
            // Check if it's not a plural noun
            if (lower_word != "news" && lower_word != "lens" && lower_word != "series") {
                pos_tags[word] = "VBZ";
            } else {
                pos_tags[word] = "NN";
            }
        }
        // Prepositions
        else if (lower_word == "in" || lower_word == "on" || lower_word == "at" ||
                 lower_word == "by" || lower_word == "for" || lower_word == "with" ||
                 lower_word == "to" || lower_word == "from" || lower_word == "of" ||
                 lower_word == "about" || lower_word == "through" || lower_word == "over" ||
                 lower_word == "under" || lower_word == "before" || lower_word == "after" ||
                 lower_word == "between" || lower_word == "among" || lower_word == "during") {
            pos_tags[word] = "IN";
        }
        // Conjunctions
        else if (lower_word == "and" || lower_word == "or" || lower_word == "but" ||
                 lower_word == "nor" || lower_word == "yet" || lower_word == "so") {
            pos_tags[word] = "CC";
        }
        // Adverbs
        else if (word.size() > 2 && word.substr(word.size()-2) == "ly") {
            pos_tags[word] = "RB";
        }
        // Adjectives
        else if (word.size() > 3 && (word.substr(word.size()-3) == "ful" ||
                                    word.substr(word.size()-3) == "ous" ||
                                    word.substr(word.size()-3) == "ive" ||
                                    word.substr(word.size()-3) == "ish" ||
                                    word.substr(word.size()-4) == "able" ||
                                    word.substr(word.size()-4) == "ible" ||
                                    word.substr(word.size()-2) == "al")) {
            pos_tags[word] = "JJ";
        }
        // Numbers
        else if (std::all_of(word.begin(), word.end(), ::isdigit)) {
            pos_tags[word] = "CD";
        }
        // Punctuation
        else if (word.size() == 1 && std::ispunct(word[0])) {
            pos_tags[word] = ".";
        }
        // Default to noun
        else {
            // Check if it's a proper noun (capitalized)
            if (!word.empty() && std::isupper(word[0])) {
                pos_tags[word] = "NNP";
            } else {
                pos_tags[word] = "NN";
            }
        }
    }
    
    return pos_tags;
}

} // namespace main
} // namespace opencog