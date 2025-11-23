/*
 * LGParser.h - Link Grammar Parser for OpenCog
 *
 * Copyright (C) 2025 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_LG_PARSER_H
#define _OPENCOG_LG_PARSER_H

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace main {

/**
 * ParseResult - Contains the results of parsing a sentence
 */
struct ParseResult {
    Handle sentence_node;
    Handle parse_node;
    std::vector<Handle> word_instances;
    std::vector<Handle> linkages;
    std::map<std::string, std::vector<std::pair<int, int>>> link_instances;
    double parse_confidence;
    int num_linkages;
};

/**
 * LGParser - Link Grammar Parser implementation
 * 
 * This class provides natural language parsing functionality using
 * the Link Grammar framework integrated with OpenCog's AtomSpace.
 */
class LGParser {
public:
    /**
     * Constructor
     * @param atomspace Pointer to the AtomSpace where parse results will be stored
     * @param dict_path Path to the Link Grammar dictionary files
     */
    LGParser(AtomSpace* atomspace, const std::string& dict_path = "en");
    
    /**
     * Destructor - cleans up parser resources
     */
    ~LGParser();
    
    /**
     * Parse a sentence and create AtomSpace representation
     * @param sentence The text to parse
     * @return ParseResult containing the parsed representation
     */
    ParseResult parseSentence(const std::string& sentence);
    
    /**
     * Parse a sentence with specific options
     * @param sentence The text to parse
     * @param max_linkages Maximum number of linkages to generate
     * @param max_parse_time Maximum time allowed for parsing (seconds)
     * @return ParseResult containing the parsed representation
     */
    ParseResult parseSentenceWithOptions(const std::string& sentence,
                                        int max_linkages = 100,
                                        int max_parse_time = 30);
    
    /**
     * Get the current dictionary language
     * @return Language code (e.g., "en", "de", "ru")
     */
    std::string getDictionaryLanguage() const { return dictionary_language_; }
    
    /**
     * Set parser options
     */
    void setMaxLinkages(int max_linkages) { max_linkages_ = max_linkages; }
    void setMaxParseTime(int seconds) { max_parse_time_ = seconds; }
    void setMinNullCount(int count) { min_null_count_ = count; }
    void setUseWordSenseDisambiguation(bool use_wsd) { use_wsd_ = use_wsd; }
    
    /**
     * Check if parser is initialized and ready
     * @return true if parser is ready, false otherwise
     */
    bool isReady() const { return parser_initialized_; }
    
    /**
     * Get parsing statistics
     */
    size_t getTotalParseCount() const { return total_parse_count_; }
    double getAverageParseTime() const {
        return total_parse_count_ > 0 ? total_parse_time_ / total_parse_count_ : 0.0;
    }

private:
    AtomSpace* atomspace_;
    std::string dictionary_language_;
    bool parser_initialized_;
    
    // Parser options
    int max_linkages_;
    int max_parse_time_;
    int min_null_count_;
    bool use_wsd_;
    
    // Statistics
    size_t total_parse_count_;
    double total_parse_time_;
    
    // Internal parser state
#ifdef HAVE_LINK_GRAMMAR
    // When Link Grammar is available, these will be proper LG types
    Dictionary lg_dictionary_;
    Parse_Options lg_options_;
#else
    // Stub types when Link Grammar is not available
    // Using void* to maintain API compatibility while indicating unavailability
    void* lg_dictionary_;  // Stub: would be Dictionary if HAVE_LINK_GRAMMAR
    void* lg_options_;     // Stub: would be Parse_Options if HAVE_LINK_GRAMMAR
#endif
    
    /**
     * Initialize the Link Grammar parser
     * @param dict_path Path to dictionary files
     * @return true if initialization successful
     */
    bool initializeParser(const std::string& dict_path);
    
    /**
     * Create word instance nodes in AtomSpace
     * @param words Vector of words from the sentence
     * @param sentence_hash Hash of the sentence for unique identification
     * @return Vector of word instance handles
     */
    std::vector<Handle> createWordInstances(const std::vector<std::string>& words,
                                          size_t sentence_hash);
    
    /**
     * Create linguistic links between word instances
     * @param word_instances Vector of word instance handles
     * @param linkage_info Linkage information from parser
     * @return Vector of created link handles
     */
    std::vector<Handle> createLinguisticLinks(const std::vector<Handle>& word_instances,
                                            const std::map<std::string, std::vector<std::pair<int, int>>>& linkage_info);
    
    /**
     * Extract part-of-speech tags for words
     * @param words Vector of words
     * @return Map of word to POS tag
     */
    std::map<std::string, std::string> extractPOSTags(const std::vector<std::string>& words);
    
    /**
     * Tokenize input text
     * @param text Input text
     * @return Vector of tokens
     */
    std::vector<std::string> tokenize(const std::string& text);
    
    /**
     * Simulate Link Grammar parsing when library is unavailable
     * Provides basic dependency structure as fallback
     * @param words Vector of tokenized words
     * @return Map of link types to word index pairs
     */
    std::map<std::string, std::vector<std::pair<int, int>>> simulateLGParsing(
        const std::vector<std::string>& words);
};

} // namespace main
} // namespace opencog

#endif // _OPENCOG_LG_PARSER_H