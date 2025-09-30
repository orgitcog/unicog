/*
 * LGParser.h - Link Grammar Parser for AtomSpace conversion
 *
 * Week 17: lg-atomese Integration - Link Grammar parsing implementation
 */

#ifndef _OPENCOG_LG_PARSER_H
#define _OPENCOG_LG_PARSER_H

#include <string>
#include <vector>
#include <memory>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Handle.h>
#include "LGAtomeseConfig.h"

namespace opencog {
namespace lg_atomese {

/**
 * Link Grammar Parser
 * Parses sentences using Link Grammar and converts to AtomSpace representation
 */
class LGParser
{
public:
    LGParser(AtomSpace* atomspace, const LGAtomeseConfig& config);
    virtual ~LGParser();
    
    // Parse sentence and return AtomSpace representation
    Handle parseSentence(const std::string& sentence);
    
    // Parse multiple sentences
    std::vector<Handle> parseSentences(const std::vector<std::string>& sentences);
    
    // Get grammatical linkages for a sentence
    std::vector<Handle> getLinkages(const std::string& sentence);
    
    // Convert Link Grammar parse to AtomSpace atoms
    Handle convertToAtomSpace(const std::string& sentence, const std::string& parse_data);
    
    // Validate grammatical correctness
    bool isGrammaticallyCorrect(const std::string& sentence);
    
private:
    AtomSpace* atomspace_;
    LGAtomeseConfig config_;
    
    // Helper methods for Link Grammar integration
    void initializeLinkGrammar();
    Handle createParseAtom(const std::string& sentence, const std::string& parse_info);
    Handle createLinkageAtom(const std::string& linkage_info);
    
    // Implementation for Link Grammar functionality
    std::string performLinkGrammarParse(const std::string& sentence);
    bool performGrammaticalCheck(const std::string& sentence);
    
    // Helper functions for grammatical analysis
    static bool isVerb(const std::string& word);
    static bool isDeterminer(const std::string& word);
    static bool isPreposition(const std::string& word);
    static bool isAdjective(const std::string& word);
    static bool isAdverb(const std::string& word);
    
    // Additional helper methods
    Handle createDetailedLinkageAtom(const std::vector<std::string>& words, 
                                   const std::string& linkage_info, 
                                   int linkage_num);
    std::string generateAlternativeParse(const std::vector<std::string>& words);
};

} // namespace lg_atomese
} // namespace opencog

#endif // _OPENCOG_LG_PARSER_H