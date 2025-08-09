/*
 * LGParser.cc - Implementation of Link Grammar Parser
 */

#include "LGParser.h"
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/StringValue.h>
#include <sstream>

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
    
    // Initialize Link Grammar library if available
    // For now, using mock implementation when library not available
    logger().info("Link Grammar parser initialized (mock mode)");
}

Handle LGParser::parseSentence(const std::string& sentence)
{
    logger().debug("Parsing sentence: %s", sentence.c_str());
    
    if (sentence.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Get Link Grammar parse data
    std::string parse_data = mockLinkGrammarParse(sentence);
    
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
    
    // Mock implementation - create sample linkages
    std::string parse_data = mockLinkGrammarParse(sentence);
    Handle linkage_atom = createLinkageAtom(parse_data);
    
    if (linkage_atom != Handle::UNDEFINED) {
        linkages.push_back(linkage_atom);
    }
    
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
    return mockGrammaticalCheck(sentence);
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

std::string LGParser::mockLinkGrammarParse(const std::string& sentence)
{
    // Mock Link Grammar parse output
    std::ostringstream parse_output;
    parse_output << "Found " << config_.getMaxLinkages() << " linkages for: " << sentence << "\n";
    parse_output << "Linkage 1: (S (NP The cat) (VP (V sits) (PP (P on) (NP the mat))))\n";
    parse_output << "Cost: 0.00, Disjunct cost: 0.00\n";
    
    return parse_output.str();
}

bool LGParser::mockGrammaticalCheck(const std::string& sentence)
{
    // Simple mock grammatical check - sentences with basic structure are considered correct
    return !sentence.empty() && 
           sentence.find(' ') != std::string::npos && 
           (sentence.back() == '.' || sentence.back() == '?' || sentence.back() == '!');
}

} // namespace lg_atomese
} // namespace opencog