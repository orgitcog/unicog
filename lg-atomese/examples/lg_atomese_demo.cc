/*
 * lg_atomese_demo.cc - Demonstration of lg-atomese functionality
 */

#include <iostream>
#include <vector>
#include <opencog/atomspace/AtomSpace.h>
#include "../opencog/lg-atomese/LGAtomeseConfig.h"
#include "../opencog/lg-atomese/LGParser.h"

using namespace opencog;
using namespace opencog::lg_atomese;

int main()
{
    std::cout << "=== lg-atomese (Link Grammar to AtomSpace) Demo ===" << std::endl;
    
    // Create AtomSpace
    AtomSpace atomspace;
    
    // Create configuration
    LGAtomeseConfig config;
    std::cout << "Configuration loaded:" << std::endl;
    std::cout << "  Dictionary path: " << config.getDictPath() << std::endl;
    std::cout << "  Max linkages: " << config.getMaxLinkages() << std::endl;
    
    // Create parser
    LGParser parser(&atomspace, config);
    
    // Test sentences
    std::vector<std::string> test_sentences = {
        "The cat sits on the mat.",
        "John loves Mary.",
        "The quick brown fox jumps over the lazy dog.",
        "OpenCog processes natural language."
    };
    
    std::cout << "\nParsing test sentences:" << std::endl;
    
    for (const auto& sentence : test_sentences) {
        std::cout << "\nSentence: " << sentence << std::endl;
        
        // Parse sentence
        Handle parse_result = parser.parseSentence(sentence);
        if (parse_result != Handle::UNDEFINED) {
            std::cout << "  Parse result: " << parse_result->to_short_string() << std::endl;
            
            // Check grammatical correctness
            bool is_correct = parser.isGrammaticallyCorrect(sentence);
            std::cout << "  Grammatically correct: " << (is_correct ? "Yes" : "No") << std::endl;
            
            // Get linkages
            auto linkages = parser.getLinkages(sentence);
            std::cout << "  Linkages found: " << linkages.size() << std::endl;
        } else {
            std::cout << "  Failed to parse sentence" << std::endl;
        }
    }
    
    std::cout << "\nAtomSpace size after parsing: " << atomspace.get_size() << " atoms" << std::endl;
    std::cout << "✅ lg-atomese demo completed successfully!" << std::endl;
    
    return 0;
}