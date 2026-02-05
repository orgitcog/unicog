- Sort the following placeholder / TODO / FIXME / mock implementations into categories according to difficulty of real implementation...

./ggml-tensor-kernel/src/AtomSpaceTensorMapper_minimal.cc:#include "atomspace_stub.h"
./ggml-tensor-kernel/src/AttentionAllocator_minimal.cc:#include "atomspace_stub.h"
./ggml-tensor-kernel/src/TensorKernel_minimal.cc:#include "atomspace_stub.h"
./atomspace/examples/pattern-matcher/filter-value.scm:; Begin by creating a mockup of a complicated data stream. What makes
./atomspace/examples/pattern-matcher/deduction-engine.scm:;; TODO: x is undefined
./atomspace/examples/pattern-matcher/filter-strings.scm:; A mockup of a stream of data to be filtered.
./atomspace/examples/atomspace/queue.scm:; XXX FIXME, this example is not yet complete and does not yet work...
./atomspace/opencog/atomspace/TypeIndex.h: * @todo The iterator is NOT thread-safe against the insertion or
./atomspace/opencog/atomspace/Transient.cc:/// XXX FIXME. Performance has not been recently measured; there
./atomspace/opencog/atomspace/AtomTable.cc:            std::bind(&AtomSpace::typeAdded, this, std::placeholders::_1));
./atomspace/opencog/atomspace/AtomTable.cc:    // atom in the parent. What??? XXX NOT TRUE FIXME
./atomspace/opencog/atomspace/AtomSpace.cc:        // TODO: this should probably be moved to a method on class Atom.
./atomspace/opencog/atomspace/AtomSpace.cc:	// Fixme maybe later someday, if/when this is needed.
./atomspace/opencog/atomspace/AtomSpace.cc:// XXX FIXME -- The recursive design of the depth() routine below makes
./atomspace/opencog/atomspace/AtomSpace.h:    // XXX FIXME Users should call StorageNode::add_nocheck() instead.
./atomspace/opencog/cython/opencog/ExecuteStub.h: * opencog/cython/opencog/ExecuteStub.h
./atomspace/opencog/cython/opencog/ExecuteStub.h:#ifndef _OPENCOG_EXECUTESTUB_H
./atomspace/opencog/cython/opencog/ExecuteStub.h:#define _OPENCOG_EXECUTESTUB_H
./atomspace/opencog/cython/opencog/ExecuteStub.h:#endif // _OPENCOG_EXECUTESTUB_H
./atomspace/opencog/cython/opencog/ExecuteStub.cc:#include "ExecuteStub.h"
./atomspace/opencog/cython/PythonEval.cc: * @todo When can we remove the singleton instance? Answer: not sure.
./atomspace/opencog/cython/PythonEval.cc:    // XXX FIXME this does a lot of wasteful string copying.
./atomspace/opencog/cython/PythonEval.cc:    _result += "PythonEval: interrupt not implemented!\n";
./atomspace/opencog/cython/PythonEval.cc:    logger().warn("[PythonEval] interrupt not implemented!\n");
./atomspace/opencog/cython/PyIncludeWrapper.h:// 0.15.1 and maybe other versions)  FIXME someday...
./atomspace/opencog/haskell/AtomSpace_CWrapper.h:     * XXX FIXME no one should be using Handle's to work with atoms,
./atomspace/opencog/haskell/PatternMatcher_CWrapper.h: * XXX FIXME: atoms must never be accessed by UUID except by the
./atomspace/opencog/ocaml/CamlWrap.cc:	// XXX FIXME
./atomspace/opencog/ocaml/CamlWrap.cc:	// XXX FIXME
./atomspace/opencog/guile/SchemeSmobAS.cc: * until a better permission system is invented. XXX FIXME.
./atomspace/opencog/guile/SchemeSmob.cc: * XXX TODO:
./atomspace/opencog/guile/modules/ExecSCM.cc:// XXX FIXME: can we fix cython to not do this, already?
./atomspace/opencog/guile/SchemeSmobAtom.cc:// XXX FIXME. Work around the despicable, horrible guile UTF8 handling.
./atomspace/opencog/guile/SchemeSmobAtom.cc:	printf("XXX FIXME Bad string %s\nconverted to %s\n", (char *) data, wbuf);
./atomspace/opencog/guile/SchemeSmobValue.cc: * XXX FIXME Clearly, a factory for values is called for.
./atomspace/opencog/guile/SchemeSmobValue.cc:		// XXX FIXME... at this time, nodes have a single name.
./atomspace/opencog/guile/SchemeSmob.h:	// Logger XXX TODO these do not belong here, they
./atomspace/opencog/guile/SchemeEval.cc:	// XXX FIXME This lock is not needed, because in guile-2.2,
./atomspace/opencog/guile/SchemeEval.cc:	// TODO: it would be nice to pass exceptions on through, but
./atomspace/opencog/guile/SchemeEval.cc:			// XXX FIXME -- idealy we should avoid catch-and-rethrow.
./atomspace/opencog/guile/SchemeEval.cc:	// XXX FIXME only a subset is needed.
./atomspace/opencog/sheaf/attic/linear-parser.scm:  XXX FIXME WARNING DANGER: As written, this runs in exponential time
./atomspace/opencog/sheaf/attic/vo-graph.scm:  XXX FIXME WARNING DANGER: As written, this runs in exponential time
./atomspace/opencog/sheaf/attic/vo-graph.scm:  XXX FIXME WARNING DANGER: As written, this runs in exponential time
./atomspace/opencog/atoms/pattern/PatternUtils.cc:		// XXX FIXME Are the below needed?
./atomspace/opencog/atoms/pattern/PatternUtils.cc:	HandleSeq todo(clauses);
./atomspace/opencog/atoms/pattern/PatternUtils.cc:	while (0 < todo.size())
./atomspace/opencog/atoms/pattern/PatternUtils.cc:		for (const Handle& cl: todo)
./atomspace/opencog/atoms/pattern/PatternUtils.cc:			todo = no_con_yet;
./atomspace/opencog/atoms/pattern/PatternUtils.cc:		todo = no_con_yet;
./atomspace/opencog/atoms/pattern/PatternTerm.h:	// TODO: it would probably be more efficient to swap which of these
./atomspace/opencog/atoms/pattern/BindLink.cc:	// Shoot. XXX FIXME. Most of the unit tests require that the atom
./atomspace/opencog/atoms/pattern/GetLink.cc:	// Shoot. XXX FIXME. Most of the unit tests require that the atom
./atomspace/opencog/atoms/pattern/PatternLink.cc:		// XXX FIXME, more correct would be to loop over
./atomspace/opencog/atoms/pattern/PatternLink.cc:		// XXX FIXME, this handles `absents`, `always` and `grouping`
./atomspace/opencog/atoms/pattern/PatternLink.cc:	// BUG - XXX FIXME. This extracts PresentLink's from the
./atomspace/opencog/atoms/pattern/PatternLink.cc:/// XXX FIXME: the code here assumes that the situation is indeed
./atomspace/opencog/atoms/pattern/PatternLink.cc:			// XXX FIXME -- this is wrong. What we really want is to
./atomspace/opencog/atoms/pattern/PatternLink.cc:// XXX FIXME: debug_log() above is more readable than the below.
./atomspace/opencog/atoms/truthvalue/FormulaTruthValue.cc:// XXX FIXME This update is not thread-safe.
./atomspace/opencog/atoms/truthvalue/CountTruthValue.h:    // XXX FIXME Are all of these really needed?
./atomspace/opencog/atoms/truthvalue/SimpleTruthValue.cc:                                   "SimpleTruthValue::merge: case not implemented");
./atomspace/opencog/atoms/truthvalue/SimpleTruthValue.h:    // XXX FIXME Are all of these really needed?
./atomspace/opencog/atoms/core/TypeChoice.cc:		// For now, just avoid throwing an exception. XXX FIXME.
./atomspace/opencog/atoms/core/Checkers.cc:// XXX FIXME Much of the onfusion below is due to a bug: if the
./atomspace/opencog/atoms/core/Checkers.cc:		// XXX FIXME, this is to be removed, because UnionLink,
./atomspace/opencog/atoms/core/Checkers.cc:		// XXX FIXME ... Perhaps IntersectionLink, UnionLink will
./atomspace/opencog/atoms/core/Checkers.cc:		// TODO - look up the schema, and make sure its numeric, also.
./atomspace/opencog/atoms/core/PutLink.cc:		// XXX TODO we should perform a type-check on the function.
./atomspace/opencog/atoms/core/RandomChoice.cc:// XXX FIXME - fix this so it can also choose a single value
./atomspace/opencog/atoms/core/RandomChoice.cc:			// XXX TODO if execute() above returns FloatValue, use that!
./atomspace/opencog/atoms/core/RandomChoice.cc:			// XXX FIXME, also allow a FloatValue!!
./atomspace/opencog/atoms/core/Variables.cc:	// XXX FIXME URE calls us with broken handle!!
./atomspace/opencog/atoms/core/Variables.cc: * XXX TODO this does not currently handle type equations, as outlined
./atomspace/opencog/atoms/core/Variables.cc:	// XXX TODO type-checking could be lazy; if the function is not
./atomspace/opencog/atoms/core/TypeIntersectionLink.cc:				"Intersection of deep types not implemented!");
./atomspace/opencog/atoms/core/TypeIntersectionLink.cc:		"Intersection of signatures or type constants not implemented!");
./atomspace/opencog/atoms/core/TypedVariableLink.cc:	// which should be using a SignatureLink for this case. XXX FIXME.
./atomspace/opencog/atoms/core/TypedVariableLink.cc:	    VARIABLE_NODE != dtype and // XXX FIXME this is wrong; URE-bug
./atomspace/opencog/atoms/core/FindUtils.h: * XXX FIXME: what if it appears quoted in one place, and unquoted
./atomspace/opencog/atoms/core/TypeUtils.cc:				"Not implemented! TODO XXX FIXME");
./atomspace/opencog/atoms/core/TypeUtils.cc:			"Not implemented! TODO XXX FIXME");
./atomspace/opencog/atoms/core/TypeUtils.cc:	// Interesting. XXX FIXME. This is not yet implemented!
./atomspace/opencog/atoms/core/TypeUtils.cc:	throw RuntimeException(TRACE_INFO, "Not implemented!");
./atomspace/opencog/atoms/core/TypeUtils.cc:	// returned. XXX FIXME -- surely this should be a throw, instead!!!
./atomspace/opencog/atoms/core/TypeNode.h:		// XXX TODO ... Some types are defined. In this case,
./atomspace/opencog/atoms/core/RewriteLink.cc:		OC_ASSERT(false, "Not implemented");
./atomspace/opencog/atoms/core/RewriteLink.cc:		// TODO: the following has no unit test!!! Yet it introduces a
./atomspace/opencog/atoms/core/RewriteLink.cc:			// TODO: generalize with when Unquote and Quote are apart
./atomspace/opencog/atoms/core/PrenexLink.cc:	// TODO: this step could be simplified by using final_variables
./atomspace/opencog/atoms/core/RewriteLink.h:	                                 // TODO: we probably want to
./atomspace/opencog/atoms/value/FormulaStream.cc:// XXX FIXME The update here is not thread-safe...
./atomspace/opencog/atoms/execution/Instantiator.h:		 * TODO: maybe this can eliminate the need for
./atomspace/opencog/atoms/execution/Instantiator.h:	 * TODO: should be refined to make the difference between AndLink
./atomspace/opencog/atoms/execution/Instantiator.h:	 * TODO: this should probably be moved to some pattern matcher
./atomspace/opencog/atoms/execution/Instantiator.cc:/// cleanly separated from each other. (XXX FIXME, these need to be
./atomspace/opencog/atoms/execution/Instantiator.cc:		// TODO: what about globs?
./atomspace/opencog/atoms/execution/Instantiator.cc:			// TODO: copy values
./atomspace/opencog/atoms/execution/Instantiator.cc:	// XXX FIXME Can we defer the addition to the atomspace to an even
./atomspace/opencog/atoms/execution/Instantiator.cc:	// XXX FIXME, we need to get rid of this call entirely, and just
./atomspace/opencog/atoms/execution/EvaluationLink.cc:		"Either incorrect or not implemented yet (crisp). Cannot evaluate %s",
./atomspace/opencog/atoms/execution/EvaluationLink.cc:		"Either incorrect or not implemented yet. Cannot evaluate %s",
./atomspace/opencog/atoms/join/JoinLink.cc:/// TODO: it might be faster to use hash tables instead of rb-trees
./atomspace/opencog/atoms/join/JoinLink.cc:/// i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.
./atomspace/opencog/atoms/join/JoinLink.cc:	// XXX FIXME this is really dumb, using a queue and then
./atomspace/opencog/atoms/reduct/AccumulateLink.cc:	// XXX TODO -- we could also handle vectors of strings, by
./atomspace/opencog/atoms/reduct/DecimateLink.cc:	// XXX FIXME ... both the NumberNode and the FloatValue variations
./atomspace/opencog/atoms/reduct/BoolOpLink.cc:	// XXX TODO we can relax this, and accept simple truth values, too.
./atomspace/opencog/atoms/base/Valuation.cc:	// XXX TODO -- C++ smart pointers are not atomic; we really
./atomspace/opencog/atoms/base/Valuation.h:	ValuePtr _value;  // XXX TODO should this be  vector?
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:/// XXX TODO: We could have a non-blocking version of this atom. We
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:                        concurrent_queue<Handle>* todo,
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:		if (not todo->try_get(h)) return;
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:	concurrent_queue<Handle> todo_list;
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:		todo_list.push(h);
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:			as, silent, &todo_list, qvp, &ex));
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:	auto todo_list = std::make_shared<concurrent_queue<Handle>>();
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:		todo_list->push(h);
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:		std::thread([as, silent, todo_list, qvp, finished_count, nthreads = _nthreads]() {
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:				if (not todo_list->try_get(h)) break;
./atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:					// TODO: Consider adding error reporting mechanism to QueueValue
./atomspace/opencog/atoms/flow/SplitLink.cc:		throw RuntimeException(TRACE_INFO, "Not implemented!");
./atomspace/opencog/atoms/flow/CollectionOfLink.cc:	// FIXME: _outoging[0] could be executable, in which case
./atomspace/opencog/atoms/flow/StringOfLink.cc:				// XXX FIXME this is unclear, under-specified and
./atomspace/opencog/atoms/flow/FilterLink.cc:						"Globbing for Values not implemented! FIXME!");
./atomspace/opencog/atoms/flow/FilterLink.cc:		// XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,
./atomspace/opencog/atoms/flow/ValueOfLink.cc:	// XXX TODO FIXME ... if either of these are executable, then
./atomspace/opencog/atoms/flow/FormulaPredicateLink.cc:/// XXX FIXME - in the future, some user is going to want to include
./atomspace/opencog/scm/opencog.scm:; FIXME: Both of the above-described problems might no longer exist.
./atomspace/opencog/scm/opencog/base/atom-docs.scm:; XXX FIXME replace below by real docs.
./atomspace/opencog/scm/opencog/base/utilities.scm:  XXX TODO -- this would almost surely be simpler to implement using
./atomspace/opencog/scm/opencog/base/utilities.scm:	; XXX FIXME -- this is a stunningly slow and sloppy random-string
./atomspace/opencog/query/Recognizer.cc:	// TODO: Change to something better if possible...
./atomspace/opencog/query/InitiateSearchMixin.cc:		// XXX FIXME; we should be using ptm->isVariable() instead !?
./atomspace/opencog/query/InitiateSearchMixin.cc:	// XXX TODO We could start inside an evaluatable, but it would
./atomspace/opencog/query/InitiateSearchMixin.cc:		// So we are good to go. XXX FIXME -- we could try again, to find
./atomspace/opencog/query/InitiateSearchMixin.cc:		// TODO -- weed out duplicates!
./atomspace/opencog/query/InitiateSearchMixin.cc:// no unit test triggers this, but its not clear why. XXX FIXME??
./atomspace/opencog/query/PatternMatchEngine.h:	// with it. XXX Needs to move to the Mixin class... XXX FIXME.
./atomspace/opencog/query/RewriteMixin.cc:	// See issue #950 and pull req #962. XXX FIXME later.
./atomspace/opencog/query/RewriteMixin.cc:/// XXX FIXME now I see how it can be done. The groupings should
./atomspace/opencog/query/SatisfyMixin.cc: * XXX FIXME: A major performance optimization is possible, to handle
./atomspace/opencog/query/SatisfyMixin.cc:			// XXX FIXME terrible hack.
./atomspace/opencog/query/TermMatchMixin.cc:			"Not implemented! Need to implement a stack, here.");
./atomspace/opencog/query/TermMatchMixin.cc:	// XXX TODO as discussed on the mailing list, we should perhaps first
./atomspace/opencog/query/TermMatchMixin.cc:		// XXX FIXME: worse: this cannot possibly be right when
./atomspace/opencog/query/NextSearchMixin.cc:// XXX TODO ... Rather than counting the number of variables, we
./atomspace/opencog/query/NextSearchMixin.cc:	// this simpler code is good enough. XXX FIXME someday?
./atomspace/opencog/query/PatternMatchEngine.cc:/// XXX FIXME: this is currently a weak stop-gap measure to handle
./atomspace/opencog/query/PatternMatchEngine.cc:	// XXX TODO The logic here should be updated to resemble that
./atomspace/opencog/query/PatternMatchEngine.cc:	// its definition. XXX TODO. Hmm. Should we do this at runtime,
./atomspace/opencog/query/PatternMatchEngine.cc:		throw RuntimeException(TRACE_INFO, "Not implemented!!");
./atomspace/opencog/query/PatternMatchEngine.cc:	// XXX FIXME - this is not very elegant. We should probably
./atomspace/opencog/query/PatternMatchEngine.cc:// XXX FIXME: Issue #3016 - Unification with unordered AndLinks
./atomspace/opencog/query/PatternMatchEngine.cc:/// XXX FIXME: Right now, this code handles graphs that have only one
./atomspace/opencog/query/PatternMatchEngine.cc:	// XXX TODO FIXME. The ptm needs to be decomposed into connected
./atomspace/opencog/query/PatternMatchEngine.cc:		OC_ASSERT(not term->hasGlobbyVar(), "Buggy or not implemented!");
./atomspace/opencog/query/PatternMatchEngine.cc:/// XXX FIXME -- do the above.
./atomspace/opencog/query/PatternMatchEngine.cc:		// XXX TODO make sure that all variables in the clause have
./atomspace/opencog/query/PatternMatchEngine.cc: * XXX TODO -- if the algo is working correctly, then all
./atomspace/opencog/query/PatternMatchEngine.cc: * TODO: The implementation here is minimal - very simple, very basic.
./atomspace/tests/atoms/execution/defined-schema.scm:; XXX FIXME, this does not quite work as one might naively expect,
./atomspace/tests/query/seq-absence.scm:;; and right now, I'm not gonna fix it... XXX FIXME.
./atomspace/tests/query/seq-absence.scm:;; XXX FIXME ... this and the above need to get done right.
./lg-atomese/opencog/lg-atomese/LGParser.h:    // Mock implementation for Link Grammar functionality when library not available
./lg-atomese/opencog/lg-atomese/LGParser.h:    std::string mockLinkGrammarParse(const std::string& sentence);
./lg-atomese/opencog/lg-atomese/LGParser.h:    bool mockGrammaticalCheck(const std::string& sentence);
./lg-atomese/opencog/lg-atomese/LGParser.cc:    // For now, using mock implementation when library not available
./lg-atomese/opencog/lg-atomese/LGParser.cc:    logger().info("Link Grammar parser initialized (mock mode)");
./lg-atomese/opencog/lg-atomese/LGParser.cc:    std::string parse_data = mockLinkGrammarParse(sentence);
./lg-atomese/opencog/lg-atomese/LGParser.cc:    // Mock implementation - create sample linkages
./lg-atomese/opencog/lg-atomese/LGParser.cc:    std::string parse_data = mockLinkGrammarParse(sentence);
./lg-atomese/opencog/lg-atomese/LGParser.cc:    return mockGrammaticalCheck(sentence);
./lg-atomese/opencog/lg-atomese/LGParser.cc:std::string LGParser::mockLinkGrammarParse(const std::string& sentence)
./lg-atomese/opencog/lg-atomese/LGParser.cc:    // Mock Link Grammar parse output
./lg-atomese/opencog/lg-atomese/LGParser.cc:bool LGParser::mockGrammaticalCheck(const std::string& sentence)
./lg-atomese/opencog/lg-atomese/LGParser.cc:    // Simple mock grammatical check - sentences with basic structure are considered correct
./cogserver/opencog/cogserver/server/CogServer.cc:        // XXX FIXME. terrible terrible hack. What we should be
./cogserver/opencog/cogserver/server/CogServer.cc:// assumes a singleton instance, so we leave this for now. XXX FIXME.
./cogserver/opencog/cogserver/server/ServerConsole.cc:/// XXX escaped quotes are not handled correctly. FIXME.
./cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc:	using namespace std::placeholders;  // for _1, _2, _3...
./cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc:	// which seems inefficient. But still ... XXX FIXME ?
./cogserver/opencog/cogserver/attic/proxy/ThruCommands.cc:		// TODO: check if the StorageNode is read-only.
./cogserver/opencog/cogserver/attic/proxy/ReadThruProxy.cc:	using namespace std::placeholders;  // for _1, _2, _3...
./cogserver/opencog/network/ServerSocket.cc:// TODO: should use std::jthread, once c++20 is widely available.
./cogserver/opencog/network/WebSocket.cc:			Send("HTTP/1.1 501 Not Implemented\r\n"
./opencog/opencog/main/IntegrationCoordinator.cc:        // Mock integration of lg-atomese component
./opencog/opencog/main/IntegrationCoordinator.cc:        // Mock integration of learn component
./opencog/opencog/main/IntegrationCoordinator.cc:        // Mock integration of attention system
./opencog/opencog/main/IntegrationCoordinator.cc:        // Mock integration of reasoning engine
./opencog/opencog/main/IntegrationCoordinator.cc:    // Mock reasoning cycle - create some inference atoms
./opencog/opencog/main/IntegrationCoordinator.cc:    metrics["system_uptime"] = 1000.0; // Mock uptime in seconds
./opencog/opencog/main/IntegrationCoordinator.cc:    // Mock configuration loading
./opencog/opencog/main/IntegrationCoordinator.cc:    // Mock text parsing - create parse atom
./opencog/opencog/main/IntegrationCoordinator.cc:    StringValuePtr parse_data = createStringValue({"mock_parse_data", "grammatical_structure"});
./opencog/opencog/main/IntegrationCoordinator.cc:    // Mock feature extraction
./opencog/opencog/main/IntegrationCoordinator.cc:    // Mock learning algorithm feeding
./opencog/opencog/main/IntegrationCoordinator.cc:    // Mock knowledge retrieval
./spacetime/src/TemporalSequence.cc: * Additional temporal sequence processing (stub implementation)
./spacetime/src/TemporalSequence.cc:// Placeholder for future advanced sequence algorithms
./spacetime/src/GeometricAlgorithms.cc: * Basic geometric algorithms and computations (stub implementation)
./spacetime/src/GeometricAlgorithms.cc:// Placeholder geometric algorithms
./cognitive-visualization/src/CognitiveVisualizer.cc:        // TODO: Implement full node_positions, attention_intensities, and cognitive_salience parsing
./tests/verification-framework.scm:; Ensures no placeholder, stub, or mock implementations exist
./tests/verification-framework.scm:;; Verify an implementation is real (not a stub/mock)
./tests/verification-framework.scm:  "Verify that an implementation produces expected behavior (not placeholder)"
./tests/verification-framework.scm:         (is-placeholder? (or (equal? actual-output "TODO")
./tests/verification-framework.scm:                            (equal? actual-output "STUB")
./tests/verification-framework.scm:                            (equal? actual-output "MOCK")
./tests/verification-framework.scm:    (if is-placeholder?
./tests/verification-framework.scm:          (format #t "❌ VERIFICATION FAILED: ~a appears to be a placeholder/stub~%" name)
./tests/verification-framework.scm:        ;; Test 1: Verify not a placeholder
./tests/verification-framework.scm:  (format #t "Ensuring NO placeholder, stub, or mock implementations exist.~%")
./tests/verification-framework.scm:          (format #t "✅ No placeholders, stubs, or mock implementations detected.~%")
./tests/verification-framework.scm:          (format #t "⚠️  Some implementations may contain placeholders or incomplete code.~%")
./unify/opencog/unify/Unify.h:	// TODO: the notion of equality between 2 CHandles might one where
./unify/opencog/unify/Unify.h:	// TODO: the type of a typed block is currently a handle of the
./unify/opencog/unify/Unify.h:	// TODO: maybe we could simplify a great deal of code by replacing
./unify/opencog/unify/Unify.h:	 * TODO: replace by RewriteLink methods!
./unify/opencog/unify/Unify.h:	 * TODO: replace by RewriteLink methods!
./unify/opencog/unify/Unify.h:	 * TODO: could probably optimize by memoizing unify, due to
./unify/opencog/unify/Unify.h:public:                         // TODO: being friend with UnifyUTest
./unify/opencog/unify/Unify.h:	 * (TODO: explain why) thus possibly multiple partitions will be
./unify/opencog/unify/Unify.h: * TODO: give example.
./unify/opencog/unify/Unify.cc:	// TODO: replace by std::set::erase_if once C++20 is enabled
./unify/opencog/unify/Unify.cc:	// TODO: make sure that ts.second contains the declaration of all
./unify/opencog/unify/Unify.cc:		OC_ASSERT(false, "Not implemented");
./unify/opencog/unify/Unify.cc:// TODO: for now it is assumed clauses are connected by an AndLink
./unify/opencog/unify/Unify.cc:// TODO: maybe replace Handle vardecl by Variables variables.
./unify/opencog/unify/Unify.cc:	return {}; // TODO: do we really need that?
./unify/opencog/unify/atoms/UnifierLink.cc:	// XXX FIXME, Maybe. This seems to handle all of the cases I've
./cogutil/opencog/util/files.cc:    // XXX FIXME Searching the current path is a security breach just
./cogutil/opencog/util/Config.cc:        // XXX FIXME Allowing boost to search relative paths is
./cogutil/opencog/util/tree.cc:// XXX FIXME This is clearly not thread-safe.
./cogutil/opencog/util/tree.cc:                         // XXX THIS IS A HACK -- FIXME
./cogutil/opencog/util/Counter.h:	/** @todo this will be replaced by C++11 constructor
./cogutil/opencog/util/sigslot.h://              std::placeholders::_1, std::placeholders::_2);
./cogutil/opencog/util/boost_ext/accumulators/statistics/geometric_mean_mirror.h:#include <boost/mpl/placeholders.hpp>
./cogutil/opencog/util/boost_ext/accumulators/statistics/geometric_mean.h:#include <boost/mpl/placeholders.hpp>
./cogutil/opencog/util/digraph.h:    /// @todo replace default random generator by OpenCog's RandGen
./cogutil/opencog/util/numeric.h:// TODO: replace the following by C++17 std::clamp
./cogutil/opencog/util/numeric.h:    // XXX FIXME writing out the explicit loop will almost
./cogutil/opencog/util/algorithm.h:		*out++ = begin = std::partition(begin, end, boost::bind(p, boost::placeholders::_1) == i);
./cogutil/opencog/util/algorithm.h: * TODO: Use T::contains instead once we move to C++20.
./cogutil/opencog/util/tree.h:/** \todo
./cogutil/opencog/util/tree.h:    tree_assert(1==0); // FIXME: not correct yet
./cogutil/opencog/util/tree.h:    // FIXME: this should be optimised.
./cogutil/opencog/util/tree.h:    return; // FIXME: we do not use first_parent_ yet, and it actually needs some serious reworking if
./cogutil/opencog/util/tree.h:    return; // FIXME: see 'set_first_parent()'
./cogutil/opencog/util/tree.h:    //       if(par==0) { // FIXME: need to keep track of this!
./cogutil/opencog/util/tree.h:            if(par==0) { // FIXME: need to keep track of this!
./cogutil/opencog/util/tree.h:// FIXME: add the other members of fixed_depth_iterator.
./cogutil/opencog/util/Logger.cc:#if defined(HAVE_GNU_BACKTRACE) /// @todo backtrace and backtrace_symbols
./cogutil/opencog/util/lru_cache.h:    /// @todo buggy thread safe operator
./cogutil/opencog/util/lru_cache.h:    /// @todo REPLACE THAT FAULTY CODE
./cogutil/opencog/util/lru_cache.h:/// @todo this stuff sucks an should be removed. It is kept because
./cogutil/opencog/util/Cover_Tree.h:    //TODO: this is pretty inefficient, there may be a better way
./cogutil/opencog/util/lazy_selector.cc:                    boost::bind(&lazy_selector::is_free, this, boost::placeholders::_1));
./cogutil/opencog/util/lazy_normal_selector.h:// XXX FIXME this makes no sense; the normal distribution is not a
./atomspace-rocks/opencog/persist/monospace/MonoStorage.h:		void destroy(void) { kill_data(); /* TODO also delete the db */ }
./atomspace-rocks/opencog/persist/monospace/MonoIO.cc:	// XXX TODO - maybe load links depth-order...
./atomspace-rocks/opencog/persist/monospace/MonoIO.cc:	// XXX FIXME. We would like to call
./atomspace-rocks/opencog/persist/rocks/RocksPersistSCM.cc:// XXX FIXME -- are open and close actually needed for anything?
./atomspace-rocks/opencog/persist/rocks/RocksIO.cc:// XXX FIXME (and in MonoSpace, too). According to the BackingStore
./atomspace-rocks/opencog/persist/rocks/RocksIO.cc:/// Note: currently broken for multi-space usage, XXX FIXME.
./atomspace-rocks/opencog/persist/rocks/RocksIO.cc:	// XXX FIXME. We would like to call
./atomspace-rocks/opencog/persist/rocks/RocksFrame.cc:/// no k@ on them.  A DB scrub routine (not implemented) could
./atomspace-rocks/opencog/persist/rocks/RocksDAG.cc:// XXX TODO: we should probably cache the results, instead of
./atomspace-rocks/opencog/persist/rocks/RocksStorage.h:		void destroy(void) { kill_data(); /* TODO also delete the db */ }
./atomspace-storage/opencog/persist/csv/table_read.cc:// TODO: This routine should be extended so that comments that start
./atomspace-storage/opencog/persist/csv/table_read.h:// TODO: Should this be a StringValue?
./atomspace-storage/opencog/persist/api/cython/PersistCython.cc:// XXX FIXME: except for the error messages, most of this code is
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/api/BackingStore.h:			throw IOException(TRACE_INFO, "Not implemented!");
./atomspace-storage/opencog/persist/prolog/DecodeProlog.cc:// Stub for handling queries.
./atomspace-storage/opencog/persist/proxy/DynamicDataProxy.cc:// XXX TODO ... create this in some temp atomspace...
./atomspace-storage/opencog/persist/proxy/ProxyNode.h:	// updated at some later date. XXX FIXME.
./atomspace-storage/opencog/persist/proxy/ProxyNode.h:	virtual void create(void) {} // stop-gap. FIXME
./atomspace-storage/opencog/persist/proxy/ProxyNode.h:	virtual void destroy(void);  //stop-gap. FIXME
./atomspace-storage/opencog/persist/proxy/ProxyNode.h:	virtual void erase(void);    // stop-gap. FIXME
./atomspace-storage/opencog/persist/proxy/ProxyNode.h:	// XXX FIXME Unimplemented BackingStore virtuals.
./atomspace-storage/opencog/persist/proxy/CachingProxy.cc:// XXX TODO Add support for expiration times, limited AtomSpace
./atomspace-storage/opencog/persist/proxy/WriteBufferProxy.cc:	// XXX FIXME. Buffering these naively, like this, voilates the
./atomspace-storage/opencog/persist/proxy/ProxyNode.cc:	rpt += "Monitoring not implemented for ";
./atomspace-storage/opencog/persist/proxy/ProxyNode.cc:// XXX FIXME. Using this ProxyParametersLink thing is a kind of
./atomspace-storage/opencog/persist/proxy/ProxyNode.cc:	// XXX FIXME;
./atomspace-storage/opencog/persist/sexcom/Dispatcher.h:	// XXX FIXME: This is a terrible design for performance.
./atomspace-storage/opencog/persist/sexcom/Commands.cc:// FIXME read above comment.
./atomspace-storage/opencog/persist/sexcom/Commands.cc:	// TODO: In principle, we should be selective, and only pass
./atomspace-storage/opencog/persist/sexcom/Commands.cc:	// But still ... maybe fixme?
./atomspace-storage/opencog/persist/sexcom/Dispatcher.cc:	using namespace std::placeholders;  // for _1, _2, _3...
./atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc: * XXX FIXME This needs to be fuzzed; it is very likely to crash
./atomspace-storage/opencog/persist/tlb/UuidSCM.cc:	// what the heck. I'm gonna punt. XXX FIXME.
./atomspace-storage/opencog/persist/flow/StoreValueOfLink.cc:	// XXX TODO FIXME ... if either of these are executable, then
./atomspace-storage/opencog/persist/flow/FetchValueOfLink.cc:	// XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]
./components/core/atomspace-rocks/opencog/persist/monospace/MonoStorage.h:		void destroy(void) { kill_data(); /* TODO also delete the db */ }
./components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc:	// XXX TODO - maybe load links depth-order...
./components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc:	// XXX FIXME. We would like to call
./components/core/atomspace-rocks/opencog/persist/rocks/RocksPersistSCM.cc:// XXX FIXME -- are open and close actually needed for anything?
./components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:// XXX FIXME (and in MonoSpace, too). According to the BackingStore
./components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:/// Note: currently broken for multi-space usage, XXX FIXME.
./components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:	// XXX FIXME. We would like to call
./components/core/atomspace-rocks/opencog/persist/rocks/RocksFrame.cc:/// no k@ on them.  A DB scrub routine (not implemented) could
./components/core/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc:// XXX TODO: we should probably cache the results, instead of
./components/core/atomspace-rocks/opencog/persist/rocks/RocksStorage.h:		void destroy(void) { kill_data(); /* TODO also delete the db */ }
./components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc:using namespace std::placeholders;
./components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.h:		// TODO: add protoatom to JSON functionality
./components/learning/moses/examples/example-progs/pole_balancing.h:    /// @todo when C++11 fully supported replace that by constexpr
./components/learning/moses/examples/example-progs/trap-uni.cc:                    std::bind(&trap::vee, this, std::placeholders::_1)),
./components/learning/moses/examples/example-progs/trap-uni.cc:                    std::bind(&trap::vee, this, std::placeholders::_1)),
./components/learning/moses/examples/example-progs/continmax.cc:// XXX todo -- finish documentation to make it look more like the
./components/learning/moses/examples/example-ant/ant_scoring.cc:    // @todo: It is not a good behavioral_score. Instead it should be
./components/learning/moses/moses/comboreduct/type_checker/type_tree.cc:        logger().error() << "default value for " << tn << " not implemented";
./components/learning/moses/moses/comboreduct/type_checker/type_tree.cc:            // XXX TODO the code below was modified to allow arg lists of
./components/learning/moses/moses/comboreduct/type_checker/type_tree.h:// TODO : lambda
./components/learning/moses/moses/comboreduct/table/table_io.cc:// TODO: This routine should be extended so that comments that start
./components/learning/moses/moses/comboreduct/table/table_io.cc: * TODO: we really need a sparse table format, as well.  
./components/learning/moses/moses/comboreduct/table/table_io.cc:                // TODO could be simplified, optimized, etc
./components/learning/moses/moses/comboreduct/table/table_io.cc:        // TODO: this could definitely be optimized
./components/learning/moses/moses/comboreduct/table/table_io.cc:        OC_ASSERT(timestamp_feature.empty(), "Timestamp feature not implemented");
./components/learning/moses/moses/comboreduct/table/table_io.cc:// TODO: implement timestamp support
./components/learning/moses/moses/comboreduct/table/table.h:// XXX FIXME TODO: change the implementation, per the above note.
./components/learning/moses/moses/comboreduct/table/table.h:        OC_ASSERT(false, "Not implemented");
./components/learning/moses/moses/comboreduct/table/table.h:        OC_ASSERT(false, "Not implemented");
./components/learning/moses/moses/comboreduct/table/table.h:                // XXX TODO WARNING ERROR: builtin hardcoded shit!!!
./components/learning/moses/moses/comboreduct/table/table.h:                // XXX TODO WARNING ERROR: builtin hardcoded shit!!!
./components/learning/moses/moses/comboreduct/table/table.h:     * TODO: we really should use iterators here, not column numbers.
./components/learning/moses/moses/comboreduct/table/table.h:     * TODO: should be generalized for multi_type_seq rather than
./components/learning/moses/moses/comboreduct/table/table.h: * XXX TODO -- this also should probably support the weight column,
./components/learning/moses/moses/comboreduct/table/table.h:    // XXX TODO to implement enum support, cut-n-paste from CTable
./components/learning/moses/moses/comboreduct/table/table.h: * correct, we really should use Fisher information. @todo this).
./components/learning/moses/moses/comboreduct/table/table.h:                    OC_ASSERT(false, "case not implemented");
./components/learning/moses/moses/comboreduct/table/table.h:        // XXX TODO, it would be easier if KLD took a sorted list
./components/learning/moses/moses/comboreduct/table/table.h:        // XXX TODO remove this print, for better performance.
./components/learning/moses/moses/comboreduct/table/table.cc:                OC_ASSERT(false, "Not implemented yet");
./components/learning/moses/moses/comboreduct/table/table.cc:    // @todo it outputs vertex_seq, it's not very general
./components/learning/moses/moses/comboreduct/table/table.cc:// XXX TODO replace this by the util p_norm function.
./components/learning/moses/moses/comboreduct/table/table.cc:// XXX TODO replace this by the util p_norm function.
./components/learning/moses/moses/comboreduct/table/table.cc:// XXX TODO replace this by the util p_norm function.
./components/learning/moses/moses/comboreduct/table/table_io.h:// TODO: reimplement loadITable with the same model of loadTable and
./components/learning/moses/moses/comboreduct/reduct/meta_rules.cc:               // @todo: checking that it inherits would be better
./components/learning/moses/moses/comboreduct/reduct/contin_rules.h:// @todo maybe one needs to consider subset of Y1 to Ym to be
./components/learning/moses/moses/comboreduct/reduct/flat_normal_form.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/comboreduct/reduct/flat_normal_form.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/comboreduct/reduct/general_rules.cc:            OC_ASSERT(false, "Not implemented yet");
./components/learning/moses/moses/comboreduct/reduct/mixed_rules.cc:// maybe TODO : 0<sum x_i -> true if exist i 0<x_i->true and forall other i 0<=x_i
./components/learning/moses/moses/comboreduct/reduct/mixed_rules.cc:                //check if 0<-(y+pi) -> false //TODO
./components/learning/moses/moses/comboreduct/reduct/contin_rules.cc:                        return; //@todo: maybe the other
./components/learning/moses/moses/comboreduct/reduct/contin_rules.cc:// TODO:  sin(*(-1 x)) -> -sin(x)
./components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:// XXX TODO: I don't understand why this is not damaging contin_if  !??
./components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:        // stub out, for performance.
./components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:            // stubbed out for performance
./components/learning/moses/moses/comboreduct/interpreter/eval.cc:    // @todo FIXME there should be a general way to distinguish between
./components/learning/moses/moses/comboreduct/interpreter/eval.cc:            OC_ASSERT(false, "apply() is not implemented");
./components/learning/moses/moses/comboreduct/interpreter/eval.cc:        // XXX TODO: contin_if should go away.
./components/learning/moses/moses/comboreduct/interpreter/eval.cc:                /// @todo tree copy
./components/learning/moses/moses/comboreduct/interpreter/eval.cc:        /// @todo FIXME avoid copying !?
./components/learning/moses/moses/comboreduct/interpreter/eval.cc:        /// @todo FIXME avoid copying !?
./components/learning/moses/moses/comboreduct/interpreter/interpreter.cc:        // XXX TODO: contin_if should go away.
./components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo the implementation could be done in 2 lines with
./components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo we're not stuck any more with boost 1.38!!!
./components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo the implementation could be done in 2 lines with
./components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo we're not stuck any more with boost 1.38!!!
./components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo the implementation could be done in 1 lines with
./components/learning/moses/moses/comboreduct/combo/simple_nn.h:           // remove impact-less connections. @todo: the code can be
./components/learning/moses/moses/comboreduct/combo/simple_nn.h:        int selected = rand() % possible.size(); // @todo:replace this by RandGen
./components/learning/moses/moses/comboreduct/combo/message.h:    /// @todo: this operator is used for scoring, and so the string
./components/learning/moses/moses/comboreduct/combo/vertex.h:    rand,         // random contin_t in [0,1) FIXME TODO : update reduct rules
./components/learning/moses/moses/comboreduct/combo/vertex.h:        // TODO
./components/learning/moses/moses/comboreduct/combo/vertex.h:    //TODO
./components/learning/moses/moses/comboreduct/combo/descriptions.cc:// ToDo: would be nice to have a more Caml/Haskell style syntax here,
./components/learning/moses/moses/comboreduct/combo/descriptions.cc:    // type. XXX FIXME...
./components/learning/moses/moses/comboreduct/main/action-reductor.cc:    // TODO -- replace this by cond
./components/learning/moses/moses/comboreduct/main/gen-table.cc:    /// @todo translate in case the variables are names rather than
./components/learning/moses/moses/comboreduct/main/eval-table.cc:// XXX FIXME
./components/learning/moses/moses/comboreduct/main/eval-table.cc:                  "Timestamp feature not implemented. "
./components/learning/moses/moses/comboreduct/main/eval-table.cc:         "TODO could be detected automatically.\n")
./components/learning/moses/moses/feature-selection/algo/simple.h:    // performance... TODO try this, if this is actually a bottleneck.
./components/learning/moses/moses/feature-selection/algo/incremental.h:                /// @todo this lock can be more granular
./components/learning/moses/moses/feature-selection/algo/incremental.h:                    /// @todo the lock could be more granular
./components/learning/moses/moses/feature-selection/algo/incremental.h:    /// @todo replace by lru_cache once thread safe fixed
./components/learning/moses/moses/feature-selection/algo/incremental.h:    /// @todo replace by lru_cache once thread safe fixed
./components/learning/moses/moses/feature-selection/main/feature-selection.h:    // @todo it might make sense to use directly hc_parameters from
./components/learning/moses/moses/feature-selection/main/fs-main.cc:// b, d, g, k, n (TODO complete)
./components/learning/moses/moses/feature-selection/main/eval-features.h:/// @todo this could be in some common file
./components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:    // @todo Sounds good too, as long as it's constant, so you would
./components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO -- should not return the penalties as part of the bscore,
./components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./components/learning/moses/moses/moses/scoring/time_dispersion.h:     * TODO: multiplier other than 1 are not supported at the moment
./components/learning/moses/moses/moses/scoring/scoring_base.h:    // XXX TODO should be a std::valarray not a vector.
./components/learning/moses/moses/moses/scoring/bscores.cc:    /// @todo could be optimized by avoiding computing the OTable and
./components/learning/moses/moses/moses/scoring/bscores.cc:    // TODO
./components/learning/moses/moses/moses/scoring/bscores.cc:/// XXX this should probably be removed! TODO FIXME
./components/learning/moses/moses/moses/scoring/scoring_base.cc:    OC_ASSERT(false, "Ensemble scoring not implemented for bscorer %s",
./components/learning/moses/moses/moses/scoring/scoring_base.cc:    // OC_ASSERT(false, "Worst possible score not implemented for bscorer %s",
./components/learning/moses/moses/moses/scoring/scoring_base.cc:    // XXX FIXME complexity_t should be a double not an int ...
./components/learning/moses/moses/moses/scoring/scoring_base.cc:    OC_ASSERT(false, "bscore error not implemented for bscorer %s",
./components/learning/moses/moses/moses/scoring/scoring_base.cc:    OC_ASSERT(false, "tree error not implemented for bscorer %s",
./components/learning/moses/moses/moses/scoring/time_dispersion.cc:    // TODO multipler other than 1 is not supported yet
./components/learning/moses/moses/moses/scoring/time_dispersion.cc:        ss << "Case " << static_cast<size_t>(_granularity) << " not implemented";
./components/learning/moses/moses/moses/scoring/precision_bscore.cc:    // @todo it's not really the best bscore but rather the best score
./components/learning/moses/moses/moses/scoring/precision_bscore.cc:    // @todo doesn't treat the case with worst_norm
./components/learning/moses/moses/moses/scoring/bscores.h:    // @todo when switching to gcc 4.6 use constructor delagation to
./components/learning/moses/moses/moses/scoring/bscores.h:    mutable KLDS<contin_t> _klds; /// @todo dangerous: not thread safe!!!
./components/learning/moses/moses/moses/scoring/ss_bscore.cc:    OC_ASSERT(false, "Not implemented yet");
./components/learning/moses/moses/moses/scoring/ss_bscore.cc:    OC_ASSERT(false, "Not implemented yet");
./components/learning/moses/moses/moses/metapopulation/merging.cc:        // XXX FIXME: we should use a pointer set for scored_combo_tree_set
./components/learning/moses/moses/moses/metapopulation/merging.cc:    // XXX TODO fix the cap so its more sensitive to the size of
./components/learning/moses/moses/moses/metapopulation/merging.cc:// XXX FIXME looks to me like it++ can often be collaed twice within this loop!
./components/learning/moses/moses/moses/metapopulation/ensemble.h: * XXX FIXME: right now, the ensemble is attached to the metapop, its
./components/learning/moses/moses/moses/metapopulation/metapopulation.h: * XXX FIXME: right now, the ensemble is attached to the metapop, its
./components/learning/moses/moses/moses/metapopulation/metapopulation.h:    /// @todo use C++11 redirection
./components/learning/moses/moses/moses/metapopulation/metapopulation.h:    /// @todo it would probably be more efficient to use
./components/learning/moses/moses/moses/metapopulation/metapopulation.h:    /// minor though in terms of performance gain. FIXME.
./components/learning/moses/moses/moses/metapopulation/metapopulation.h:    // TODO: we may want to output the visited status as well
./components/learning/moses/moses/moses/metapopulation/metapopulation.cc:    // XXX FIXME should probably not recompute every time ...
./components/learning/moses/moses/moses/representation/representation.cc:// XXX TODO: One might think that varying the stepsize, i.e. shrinking
./components/learning/moses/moses/moses/representation/representation.cc:    // XXX TODO need to add support for "term algebra" knobs
./components/learning/moses/moses/moses/representation/representation.cc:    // @todo: term algebras
./components/learning/moses/moses/moses/representation/instance_scorer.h:// XXX FIXME, calling score_tree above does not throw the exception; this should be done
./components/learning/moses/moses/moses/representation/build_knobs.cc: * XXX TODO: see comments on disc_probe() below.  This method is a real
./components/learning/moses/moses/moses/representation/build_knobs.cc:/// TODO: measure and compare the resulting performance.
./components/learning/moses/moses/moses/representation/build_knobs.cc:        /// @todo could use kb.complexity_bound() to be faster, but
./components/learning/moses/moses/moses/representation/build_knobs.cc:    // TODO: should bias the selection of these, so that
./components/learning/moses/moses/moses/representation/build_knobs.cc:    // We should probably OC_ASSERT here ... TODO
./components/learning/moses/moses/moses/representation/build_knobs.cc:    // XXX TODO clarify actual breakeven on range of problems...
./components/learning/moses/moses/moses/representation/build_knobs.cc:    // XXX TODO: Is this really optimal?  The below adds an entire copy
./components/learning/moses/moses/moses/representation/build_knobs.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/moses/representation/build_knobs.cc:        logger().warn("TODO: handle case where it = id::times in build_knobs::rec_canonize");
./components/learning/moses/moses/moses/representation/build_knobs.cc:// TODO: implement support for enumerated types in the input.
./components/learning/moses/moses/moses/representation/build_knobs.cc:    //TODO: should bias the selection of these (and possibly choose larger subtrees)
./components/learning/moses/moses/moses/representation/build_knobs.cc:// XXX TODO this below is clearly unfinished, broken, etc.
./components/learning/moses/moses/moses/representation/build_knobs.cc:    //FIXME: now just attaches to the first output
./components/learning/moses/moses/moses/representation/field_set.h:        // @todo: could be a source of bug if such order is not total
./components/learning/moses/moses/moses/representation/field_set.h:        // @todo: compute at the start in _fields - could be faster..
./components/learning/moses/moses/moses/representation/field_set.h:        // @todo: compute at the start in _fields - could be faster..
./components/learning/moses/moses/moses/representation/field_set.h:        // @todo: compute at the start in _fields - could be faster..
./components/learning/moses/moses/moses/representation/field_set.h:    /// @todo rename that begin_bit for more consistency
./components/learning/moses/moses/moses/representation/representation.h:                             // @todo: it is not sure whether we need
./components/learning/moses/moses/moses/representation/knobs.h:    // @todo: it does not go back to the initiale state
./components/learning/moses/moses/moses/representation/knobs.h:    // the shift is definitely in the wrong direction!! FIXME.
./components/learning/moses/moses/moses/deme/feature_selector.cc:/// XXX TODO Explain what this function does. Why does it create a second
./components/learning/moses/moses/moses/deme/feature_selector.cc:                    OC_ASSERT(false, "Not implemented");
./components/learning/moses/moses/moses/deme/deme_expander.cc: * XXX TODO I honestly just don't see the utility of this multi-deme
./components/learning/moses/moses/moses/deme/deme_expander.cc:                // TODO: DO NOT CHANGE THE MAX SCORE IF USER SET IT: BUT THAT
./components/learning/moses/moses/moses/deme/deme_expander.cc:                // TODO: re-enable that once best_possible_bscore is fixed
./components/learning/moses/moses/moses/deme/deme_expander.cc:        // XXX FIXME this is a bug .. the user may have specified that
./components/learning/moses/moses/moses/optimization/hill-climbing.h:    // XXX TODO make sure this value is appropriately updated.
./components/learning/moses/moses/moses/optimization/univariate.cc:                  "Trunction selection not implemented."
./components/learning/moses/moses/moses/optimization/hill-climbing.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/moses/optimization/particle-swarm.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/moses/optimization/particle-swarm.cc:        // TODO: work in a better way to identify convergence.
./components/learning/moses/moses/moses/optimization/particle-swarm.cc:// TODO: Explanation
./components/learning/moses/moses/moses/optimization/particle-swarm.h:// TODO: pso description
./components/learning/moses/moses/moses/optimization/particle-swarm.h:    // TODO: Wind dispersion, but test without first
./components/learning/moses/moses/moses/optimization/star-anneal.cc:// XXX TODO the annealing temperature control code should be ported over
./components/learning/moses/moses/moses/optimization/star-anneal.cc:    using namespace boost::placeholders;
./components/learning/moses/moses/moses/optimization/star-anneal.cc:    // @todo this should be adapted for SA
./components/learning/moses/moses/moses/optimization/star-anneal.h:     * distance.  @todo: it may be better to have the distance
./components/learning/moses/moses/moses/main/problem-params.h:// XXX FIXME TODO The structure below should be split into multiple
./components/learning/moses/moses/moses/main/problem-params.cc:    // XXX TODO: make this print correctly, instead of using brackets.
./components/learning/moses/moses/moses/main/problem-params.cc:        // The remaining options (TODO organize this)
./components/learning/moses/moses/moses/main/problem-params.cc:        ss << "Granularity " << time_bscore_granularity_str << " not implemented";
./components/learning/moses/moses/moses/main/eval-candidate-likelihood.cc:        OC_ASSERT(false, "likelihood for problem %s is not implemented",
./components/learning/moses/moses/moses/main/demo-problems.cc:        // @todo: Occam's razor and nsamples is not taken into account
./components/learning/moses/moses/moses/main/demo-problems.cc:        // @todo: introduce some noise optionally
./components/learning/moses/moses/moses/main/table-problems.cc:    // XXX FIXME -- the multiple tables should be merged into one.
./components/learning/moses/moses/moses/main/table-problems.cc:    // XXX FIXME .. check that they all have the same signature.
./components/learning/moses/moses/moses/eda/replacement.h:// TODO: I think it might be a little more efficent to use the
./components/learning/moses/moses/moses/eda/optimize.h:                            std::bind(std::cref(score), std::placeholders::_1));
./components/learning/moses/moses/moses/eda/optimize.h:                                std::bind(std::cref(score), std::placeholders::_1));
./components/learning/moses/moses/moses/eda/local_structure.h:// XXX TODO document what this does...
./components/learning/moses/moses/moses/eda/local_structure.h:// XXX TODO this is unclear, explain what is being accumulated where.
./components/learning/moses/moses/moses/eda/local_structure.h:    using namespace boost::placeholders;
./components/learning/moses/moses/moses/eda/local_structure.h:    using namespace boost::placeholders;
./components/learning/moses/moses/moses/eda/local_structure.cc:                               std::bind(&field_set::get_raw, &_fields, std::placeholders::_1, idx),
./components/learning/moses/moses/moses/eda/local_structure.cc:                               std::bind(&field_set::get_raw, &_fields, std::placeholders::_2, idx))) ==
./components/learning/moses/moses/moses/eda/local_structure.cc:                             std::bind(valueof<const instance>, std::placeholders::_1), src_idx),
./components/learning/moses/moses/moses/eda/local_structure.cc:                 std::bind(&local_structure_model::rec_split_term, this, std::placeholders::_1, std::placeholders::_2,
./components/learning/moses/moses/moses/eda/local_structure.cc:                      src_idx + 1, idx, std::placeholders::_3, std::placeholders::_4));
./components/learning/moses/moses/moses/eda/local_structure.cc:                             std::bind(valueof<const instance>, std::placeholders::_1), src_idx),
./components/learning/moses/moses/moses/moses/neighborhood_sampling.cc:        // @todo: handle term algebras
./components/learning/moses/moses/moses/moses/mpi_moses.cc:// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.
./components/learning/moses/moses/moses/moses/mpi_moses.cc:        if (!dex.create_demes(exemplar, 0 /* TODO replace with the
./components/learning/moses/moses/moses/moses/mpi_moses.cc:        // XXX TODO should probably fetch max_time from somewhere...
./components/learning/moses/moses/moses/moses/mpi_moses.cc:    // OC_ASSERT(false, "TODO: understand what is the role source=0 exactly");
./components/learning/moses/moses/moses/moses/mpi_moses.cc:        // XXX TODO instead of overwritting the demeID it should be
./components/learning/moses/moses/moses/moses/partial.cc:        // XXX TODO: we need to get the actual number of gens run, back
./components/learning/moses/moses/moses/moses/distributed_moses.cc:    // Arghhh FIXME. fuser might not be installed, or it may not be in
./components/learning/moses/moses/moses/moses/moses_main.h:        // XXX TODO this should be fixed, someday...
./components/learning/moses/moses/moses/moses/local_moses.cc:                // TODO use the option of the output
./components/learning/moses/moses/moses/moses/types.h:// TODO this should be a std::valarray not std::vector but I am too
./components/learning/moses/moses/moses/moses/mpi_moses.h:// this just right now. XXX TODO: do this, someday.
./components/learning/moses/moses/moses/moses/complexity.cc:// we should count logical and, logical_or, below ..!?!? TODO, clarify.
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h: * @todo: in order to better approximate the real-number metric, we
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h: * @todo: term algebra fields are ignored for now
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h:                // @todo: now the distance is 1, choose the distance
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h: * XXX TODO: the current algo could be speeded up a fair bit, cutting
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h: * @todo: term algebra is ignored for the moment.
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h:        // @todo: handle term algebras XXX
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h:            // XXX TODO, unroll the last tail call, just like the single-bit
./components/learning/moses/moses/moses/moses/neighborhood_sampling.h: * XXX/TODO: the performance of this thing can be strongly improved
./atomspace-restful/opencog/events/AtomSpacePublisherModule.cc:using namespace std::placeholders;
./atomspace-restful/opencog/events/AtomSpacePublisherModule.h:		// TODO: add protoatom to JSON functionality
./ure/examples/ure/contraposition/contraposition.scm:;; TODO: this example is boggus, please someone fixes it.
./ure/examples/ure/contraposition/contraposition.scm:;; TODO: we probably don't need that
./ure/examples/ure/replacement/kb.scm:    ;; TODO: use Set instead of List
./ure/examples/ure/replacement/rb.scm:    ;; TODO: use Set instead of List
./ure/opencog/ure/BetaDistribution.cc:	// TODO should be replaced by tv->get_mode() once implemented
./ure/opencog/ure/backwardchainer/BIT.h:	// BITNode handle (TODO: maybe this is not necessary)
./ure/opencog/ure/backwardchainer/BIT.h:	// TODO: Maybe this should be moved to BackwardChainer
./ure/opencog/ure/backwardchainer/BIT.h:	 * @todo support fitness function.
./ure/opencog/ure/backwardchainer/BIT.h:	 * https://github.com/opencog/atomspace/issues/903. TODO:
./ure/opencog/ure/backwardchainer/BIT.h:	 * TODO: give examples.
./ure/opencog/ure/backwardchainer/BIT.h:	 * TODO: give examples.
./ure/opencog/ure/backwardchainer/BIT.h:	 * TODO: give examples.
./ure/opencog/ure/backwardchainer/BackwardChainer.cc:                                 const Handle& focus_set, // TODO:
./ure/opencog/ure/backwardchainer/BackwardChainer.cc:	// TODO: Maybe we could take advantage of the new read-only
./ure/opencog/ure/backwardchainer/ControlPolicy.h:// TODO: maybe wrap that in a class, and use it in foward chainer
./ure/opencog/ure/backwardchainer/ControlPolicy.h:	 * TODO: add comments about inference control policy, see
./ure/opencog/ure/backwardchainer/ControlPolicy.h:	 * TODO: replace this by the mean method of the TruthValue once
./ure/opencog/ure/backwardchainer/Fitness.h:	// TODO: we may want to move the arguments in its own class if it
./ure/opencog/ure/backwardchainer/Fitness.h:	// TODO: replace by class dedicated to hold the parameters
./ure/opencog/ure/backwardchainer/Fitness.cc:		ure_logger().error() << "Not implemented";
./ure/opencog/ure/backwardchainer/Fitness.cc:		ure_logger().error() << "Not implemented";
./ure/opencog/ure/backwardchainer/ControlPolicy.cc:	// TODO: should be alpha-converted to have no variable in common.
./ure/opencog/ure/backwardchainer/ControlPolicy.cc:		ss << "Not implemented yet. "
./ure/opencog/ure/backwardchainer/TraceRecorder.h:	// TODO: the TV on the evaluation link should be more carefully
./ure/opencog/ure/backwardchainer/BackwardChainer.h: * TODO: update that comment
./ure/opencog/ure/backwardchainer/BackwardChainer.h:	 * @param focus_set          Focus set (not implemented)
./ure/opencog/ure/backwardchainer/BackwardChainer.h:	                // TODO: maybe move the control and focus set to
./ure/opencog/ure/backwardchainer/BackwardChainer.h:	                // TODO: maybe wrap all fitnesses in a Fitness class
./ure/opencog/ure/backwardchainer/BackwardChainer.h:	                // TODO: maybe move the control and focus set to
./ure/opencog/ure/backwardchainer/BackwardChainer.h:	                // TODO: maybe wrap all fitnesses in a Fitness class
./ure/opencog/ure/backwardchainer/BackwardChainer.h:	// TODO: perhaps move that under BIT
./ure/opencog/ure/backwardchainer/BIT.cc:	set_leaf2bitnode();         // TODO: might differ till needed to optimize
./ure/opencog/ure/backwardchainer/BIT.cc:					          " premises not implemented!");
./ure/opencog/ure/backwardchainer/BIT.cc:					          " premises not implemented!");
./ure/opencog/ure/backwardchainer/BIT.cc:	// TODO: is this merging necessary?
./ure/opencog/ure/UREConfig.h: * @todo: It doesn't support the hierarchical configuration structure
./ure/opencog/ure/Rule.h:	 * TODO: support backward rule form.
./ure/opencog/ure/Rule.h:	 * TODO: probably obsolete, should be removed
./ure/opencog/ure/Rule.h:	 * TODO: we probably want to support a vector of sources for rules
./ure/opencog/ure/Rule.h:	 * TODO: we probably want to return only typed substitutions.
./ure/opencog/ure/Rule.h:	 * TODO: it's not clear the forward chainer needs the
./ure/opencog/ure/Rule.h:	 * TODO: we probably want to return only typed substitutions.
./ure/opencog/ure/Rule.h:	// TODO: subdivide in smaller and shared mutexes
./ure/opencog/ure/ThompsonSampling.h:	 * TODO: for now it builds the entire selection distribution, then
./ure/opencog/ure/Rule.cc:	// TODO: could certainly be optimized by not systematically
./ure/opencog/ure/Rule.cc:	OC_ASSERT(false, "RuleSet::operator< not implemented");
./ure/opencog/ure/forwardchainer/SourceSet.h:// TODO: this class has thing in common with AndBIT, maybe their
./ure/opencog/ure/forwardchainer/SourceSet.h:	// TODO: subdivide in smaller and shared mutexes
./ure/opencog/ure/forwardchainer/SourceSet.h:// TODO: this class has things in common with BIT, maybe their common
./ure/opencog/ure/forwardchainer/SourceSet.h:	// TODO: subdivide in smaller and shared mutexes
./ure/opencog/ure/forwardchainer/SourceSet.cc:	// TODO:
./ure/opencog/ure/forwardchainer/FCStat.h:	// TODO: subdivide in smaller and shared mutexes
./ure/opencog/ure/forwardchainer/ForwardChainer.h:	 * TODO: move to ControlPolicy
./ure/opencog/ure/forwardchainer/ForwardChainer.h:	// TODO: subdivide in smaller and shared mutexes
./ure/opencog/ure/forwardchainer/ForwardChainer.h:	// TODO: use shared mutexes
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:	// TODO: For now the FC follows the old standard. We may move to
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:	// Relex2Logic uses this. TODO make a separate class to handle
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:// TODO: if creating/destroying threads is too expensive, use a thread
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:		// TODO: This can be simplified but is let here until do_step is
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:	// TODO: refine mutex
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:			// TODO: This has the effect of deallocating the rules, which
./ure/opencog/ure/forwardchainer/ForwardChainer.cc:	std::lock_guard<std::mutex> lock(_rules_mutex); // TODO: refine
./ure/opencog/ure/forwardchainer/SourceRuleSet.h:	// TODO: implement tournament selection as well, as a cheaper
./ure/opencog/scm/opencog/ure/ure-utils.scm:;; -- cog-new-flattened-link -- Create flattened link TODO: remove cog- prefix
./ure/opencog/scm/opencog/ure/ure-utils.scm:      consider for forward chaining (Not Implemented).
./ure/opencog/scm/opencog/ure/ure-utils.scm:;; TODO: generalize ure-rm-rule to accept rule-symbol and rule-name as
./ure/opencog/scm/opencog/ure/ure-utils.scm:; TODO: Move logic to ForwardChainer.
./ure/opencog/scm/opencog/ure/ure-utils.scm:    ; TODO: Add an optional argument for filtering results b/n steps using.
./ure/tests/ure/backwardchainer/scm/green-balls-targets.scm:;; TODO: the type of G should be further specified, such the number of
./ure/tests/ure/rules/conditional-direct-evaluation.scm:;; TODO: we should make the evidence as premises. One way to do that
./ure/tests/ure/rules/conditional-direct-evaluation.scm:;; TODO: turn that into a generator
./ure/tests/ure/rules/conditional-direct-evaluation.scm:  (let* ;; TODO replace by a distributional TV based calculation.
./ure/tests/ure/rules/implication-scope-to-implication-rule.scm:           ;; TODO: support VariableSet
./ure/tests/ure/rules/pln-implication-and-lambda-factorization-rule.scm:;; TODO: Replace this by higher order fact
./ure/tests/ure/rules/evidence-based-conditional-direct-evaluation.scm:;; TODO: resume once GlobNode is supported
./ure/tests/ure/rules/intensional-inheritance-direct-introduction.scm:;; TODO: in order to add the Attraction links in the premises maybe an
./ure/tests/ure/rules/implication-instantiation-rule.scm:;; TODO: To make this function better a form of partial pattern
./ure/tests/ure/rules/implication-instantiation-rule.scm:                                        ; remaining variables. TODO:
./ure/tests/ure/rules/implication-and-lambda-factorization-rule.scm:;; TODO: Replace this by higher order fact
./distributed-cognition/src/MultiAgentStressTest.cc:// Stub implementations for other complex methods
./moses/examples/example-progs/pole_balancing.h:    /// @todo when C++11 fully supported replace that by constexpr
./moses/examples/example-progs/trap-uni.cc:                    std::bind(&trap::vee, this, std::placeholders::_1)),
./moses/examples/example-progs/trap-uni.cc:                    std::bind(&trap::vee, this, std::placeholders::_1)),
./moses/examples/example-progs/continmax.cc:// XXX todo -- finish documentation to make it look more like the
./moses/examples/example-ant/ant_scoring.cc:    // @todo: It is not a good behavioral_score. Instead it should be
./moses/moses/comboreduct/type_checker/type_tree.cc:        logger().error() << "default value for " << tn << " not implemented";
./moses/moses/comboreduct/type_checker/type_tree.cc:            // XXX TODO the code below was modified to allow arg lists of
./moses/moses/comboreduct/type_checker/type_tree.h:// TODO : lambda
./moses/moses/comboreduct/table/table_io.cc:// TODO: This routine should be extended so that comments that start
./moses/moses/comboreduct/table/table_io.cc: * TODO: we really need a sparse table format, as well.  
./moses/moses/comboreduct/table/table_io.cc:                // TODO could be simplified, optimized, etc
./moses/moses/comboreduct/table/table_io.cc:        // TODO: this could definitely be optimized
./moses/moses/comboreduct/table/table_io.cc:        OC_ASSERT(timestamp_feature.empty(), "Timestamp feature not implemented");
./moses/moses/comboreduct/table/table_io.cc:// TODO: implement timestamp support
./moses/moses/comboreduct/table/table.h:// XXX FIXME TODO: change the implementation, per the above note.
./moses/moses/comboreduct/table/table.h:        OC_ASSERT(false, "Not implemented");
./moses/moses/comboreduct/table/table.h:        OC_ASSERT(false, "Not implemented");
./moses/moses/comboreduct/table/table.h:                // XXX TODO WARNING ERROR: builtin hardcoded shit!!!
./moses/moses/comboreduct/table/table.h:                // XXX TODO WARNING ERROR: builtin hardcoded shit!!!
./moses/moses/comboreduct/table/table.h:     * TODO: we really should use iterators here, not column numbers.
./moses/moses/comboreduct/table/table.h:     * TODO: should be generalized for multi_type_seq rather than
./moses/moses/comboreduct/table/table.h: * XXX TODO -- this also should probably support the weight column,
./moses/moses/comboreduct/table/table.h:    // XXX TODO to implement enum support, cut-n-paste from CTable
./moses/moses/comboreduct/table/table.h: * correct, we really should use Fisher information. @todo this).
./moses/moses/comboreduct/table/table.h:                    OC_ASSERT(false, "case not implemented");
./moses/moses/comboreduct/table/table.h:        // XXX TODO, it would be easier if KLD took a sorted list
./moses/moses/comboreduct/table/table.h:        // XXX TODO remove this print, for better performance.
./moses/moses/comboreduct/table/table.cc:                OC_ASSERT(false, "Not implemented yet");
./moses/moses/comboreduct/table/table.cc:    // @todo it outputs vertex_seq, it's not very general
./moses/moses/comboreduct/table/table.cc:// XXX TODO replace this by the util p_norm function.
./moses/moses/comboreduct/table/table.cc:// XXX TODO replace this by the util p_norm function.
./moses/moses/comboreduct/table/table.cc:// XXX TODO replace this by the util p_norm function.
./moses/moses/comboreduct/table/table_io.h:// TODO: reimplement loadITable with the same model of loadTable and
./moses/moses/comboreduct/reduct/meta_rules.cc:               // @todo: checking that it inherits would be better
./moses/moses/comboreduct/reduct/contin_rules.h:// @todo maybe one needs to consider subset of Y1 to Ym to be
./moses/moses/comboreduct/reduct/flat_normal_form.cc:    using namespace boost::placeholders;
./moses/moses/comboreduct/reduct/flat_normal_form.cc:    using namespace boost::placeholders;
./moses/moses/comboreduct/reduct/general_rules.cc:            OC_ASSERT(false, "Not implemented yet");
./moses/moses/comboreduct/reduct/mixed_rules.cc:// maybe TODO : 0<sum x_i -> true if exist i 0<x_i->true and forall other i 0<=x_i
./moses/moses/comboreduct/reduct/mixed_rules.cc:                //check if 0<-(y+pi) -> false //TODO
./moses/moses/comboreduct/reduct/contin_rules.cc:                        return; //@todo: maybe the other
./moses/moses/comboreduct/reduct/contin_rules.cc:// TODO:  sin(*(-1 x)) -> -sin(x)
./moses/moses/comboreduct/reduct/logical_rules.cc:// XXX TODO: I don't understand why this is not damaging contin_if  !??
./moses/moses/comboreduct/reduct/logical_rules.cc:    using namespace boost::placeholders;
./moses/moses/comboreduct/reduct/logical_rules.cc:    using namespace boost::placeholders;
./moses/moses/comboreduct/reduct/logical_rules.cc:        // stub out, for performance.
./moses/moses/comboreduct/reduct/logical_rules.cc:            // stubbed out for performance
./moses/moses/comboreduct/interpreter/eval.cc:    // @todo FIXME there should be a general way to distinguish between
./moses/moses/comboreduct/interpreter/eval.cc:            OC_ASSERT(false, "apply() is not implemented");
./moses/moses/comboreduct/interpreter/eval.cc:        // XXX TODO: contin_if should go away.
./moses/moses/comboreduct/interpreter/eval.cc:                /// @todo tree copy
./moses/moses/comboreduct/interpreter/eval.cc:        /// @todo FIXME avoid copying !?
./moses/moses/comboreduct/interpreter/eval.cc:        /// @todo FIXME avoid copying !?
./moses/moses/comboreduct/interpreter/interpreter.cc:        // XXX TODO: contin_if should go away.
./moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo the implementation could be done in 2 lines with
./moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo we're not stuck any more with boost 1.38!!!
./moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo the implementation could be done in 2 lines with
./moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo we're not stuck any more with boost 1.38!!!
./moses/moses/comboreduct/combo/iostream_combo.cc:    /// @todo the implementation could be done in 1 lines with
./moses/moses/comboreduct/combo/simple_nn.h:           // remove impact-less connections. @todo: the code can be
./moses/moses/comboreduct/combo/simple_nn.h:        int selected = rand() % possible.size(); // @todo:replace this by RandGen
./moses/moses/comboreduct/combo/message.h:    /// @todo: this operator is used for scoring, and so the string
./moses/moses/comboreduct/combo/vertex.h:    rand,         // random contin_t in [0,1) FIXME TODO : update reduct rules
./moses/moses/comboreduct/combo/vertex.h:        // TODO
./moses/moses/comboreduct/combo/vertex.h:    //TODO
./moses/moses/comboreduct/combo/descriptions.cc:// ToDo: would be nice to have a more Caml/Haskell style syntax here,
./moses/moses/comboreduct/combo/descriptions.cc:    // type. XXX FIXME...
./moses/moses/comboreduct/main/action-reductor.cc:    // TODO -- replace this by cond
./moses/moses/comboreduct/main/gen-table.cc:    /// @todo translate in case the variables are names rather than
./moses/moses/comboreduct/main/eval-table.cc:// XXX FIXME
./moses/moses/comboreduct/main/eval-table.cc:                  "Timestamp feature not implemented. "
./moses/moses/comboreduct/main/eval-table.cc:         "TODO could be detected automatically.\n")
./moses/moses/feature-selection/algo/simple.h:    // performance... TODO try this, if this is actually a bottleneck.
./moses/moses/feature-selection/algo/incremental.h:                /// @todo this lock can be more granular
./moses/moses/feature-selection/algo/incremental.h:                    /// @todo the lock could be more granular
./moses/moses/feature-selection/algo/incremental.h:    /// @todo replace by lru_cache once thread safe fixed
./moses/moses/feature-selection/algo/incremental.h:    /// @todo replace by lru_cache once thread safe fixed
./moses/moses/feature-selection/main/feature-selection.h:    // @todo it might make sense to use directly hc_parameters from
./moses/moses/feature-selection/main/fs-main.cc:// b, d, g, k, n (TODO complete)
./moses/moses/feature-selection/main/eval-features.h:/// @todo this could be in some common file
./moses/moses/moses/scoring/discriminating_bscore.cc:    // @todo Sounds good too, as long as it's constant, so you would
./moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO -- should not return the penalties as part of the bscore,
./moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./moses/moses/moses/scoring/discriminating_bscore.cc:    // XXX TODO FIXME is this really correct?
./moses/moses/moses/scoring/time_dispersion.h:     * TODO: multiplier other than 1 are not supported at the moment
./moses/moses/moses/scoring/scoring_base.h:    // XXX TODO should be a std::valarray not a vector.
./moses/moses/moses/scoring/bscores.cc:    /// @todo could be optimized by avoiding computing the OTable and
./moses/moses/moses/scoring/bscores.cc:    // TODO
./moses/moses/moses/scoring/bscores.cc:/// XXX this should probably be removed! TODO FIXME
./moses/moses/moses/scoring/scoring_base.cc:    OC_ASSERT(false, "Ensemble scoring not implemented for bscorer %s",
./moses/moses/moses/scoring/scoring_base.cc:    // OC_ASSERT(false, "Worst possible score not implemented for bscorer %s",
./moses/moses/moses/scoring/scoring_base.cc:    // XXX FIXME complexity_t should be a double not an int ...
./moses/moses/moses/scoring/scoring_base.cc:    OC_ASSERT(false, "bscore error not implemented for bscorer %s",
./moses/moses/moses/scoring/scoring_base.cc:    OC_ASSERT(false, "tree error not implemented for bscorer %s",
./moses/moses/moses/scoring/time_dispersion.cc:    // TODO multipler other than 1 is not supported yet
./moses/moses/moses/scoring/time_dispersion.cc:        ss << "Case " << static_cast<size_t>(_granularity) << " not implemented";
./moses/moses/moses/scoring/precision_bscore.cc:    // @todo it's not really the best bscore but rather the best score
./moses/moses/moses/scoring/precision_bscore.cc:    // @todo doesn't treat the case with worst_norm
./moses/moses/moses/scoring/bscores.h:    // @todo when switching to gcc 4.6 use constructor delagation to
./moses/moses/moses/scoring/bscores.h:    mutable KLDS<contin_t> _klds; /// @todo dangerous: not thread safe!!!
./moses/moses/moses/scoring/ss_bscore.cc:    OC_ASSERT(false, "Not implemented yet");
./moses/moses/moses/scoring/ss_bscore.cc:    OC_ASSERT(false, "Not implemented yet");
./moses/moses/moses/metapopulation/merging.cc:        // XXX FIXME: we should use a pointer set for scored_combo_tree_set
./moses/moses/moses/metapopulation/merging.cc:    // XXX TODO fix the cap so its more sensitive to the size of
./moses/moses/moses/metapopulation/merging.cc:// XXX FIXME looks to me like it++ can often be collaed twice within this loop!
./moses/moses/moses/metapopulation/ensemble.h: * XXX FIXME: right now, the ensemble is attached to the metapop, its
./moses/moses/moses/metapopulation/metapopulation.h: * XXX FIXME: right now, the ensemble is attached to the metapop, its
./moses/moses/moses/metapopulation/metapopulation.h:    /// @todo use C++11 redirection
./moses/moses/moses/metapopulation/metapopulation.h:    /// @todo it would probably be more efficient to use
./moses/moses/moses/metapopulation/metapopulation.h:    /// minor though in terms of performance gain. FIXME.
./moses/moses/moses/metapopulation/metapopulation.h:    // TODO: we may want to output the visited status as well
./moses/moses/moses/metapopulation/metapopulation.cc:    // XXX FIXME should probably not recompute every time ...
./moses/moses/moses/representation/representation.cc:// XXX TODO: One might think that varying the stepsize, i.e. shrinking
./moses/moses/moses/representation/representation.cc:    // XXX TODO need to add support for "term algebra" knobs
./moses/moses/moses/representation/representation.cc:    // @todo: term algebras
./moses/moses/moses/representation/instance_scorer.h:// XXX FIXME, calling score_tree above does not throw the exception; this should be done
./moses/moses/moses/representation/build_knobs.cc: * XXX TODO: see comments on disc_probe() below.  This method is a real
./moses/moses/moses/representation/build_knobs.cc:/// TODO: measure and compare the resulting performance.
./moses/moses/moses/representation/build_knobs.cc:        /// @todo could use kb.complexity_bound() to be faster, but
./moses/moses/moses/representation/build_knobs.cc:    // TODO: should bias the selection of these, so that
./moses/moses/moses/representation/build_knobs.cc:    // We should probably OC_ASSERT here ... TODO
./moses/moses/moses/representation/build_knobs.cc:    // XXX TODO clarify actual breakeven on range of problems...
./moses/moses/moses/representation/build_knobs.cc:    // XXX TODO: Is this really optimal?  The below adds an entire copy
./moses/moses/moses/representation/build_knobs.cc:    using namespace boost::placeholders;
./moses/moses/moses/representation/build_knobs.cc:        logger().warn("TODO: handle case where it = id::times in build_knobs::rec_canonize");
./moses/moses/moses/representation/build_knobs.cc:// TODO: implement support for enumerated types in the input.
./moses/moses/moses/representation/build_knobs.cc:    //TODO: should bias the selection of these (and possibly choose larger subtrees)
./moses/moses/moses/representation/build_knobs.cc:// XXX TODO this below is clearly unfinished, broken, etc.
./moses/moses/moses/representation/build_knobs.cc:    //FIXME: now just attaches to the first output
./moses/moses/moses/representation/field_set.h:        // @todo: could be a source of bug if such order is not total
./moses/moses/moses/representation/field_set.h:        // @todo: compute at the start in _fields - could be faster..
./moses/moses/moses/representation/field_set.h:        // @todo: compute at the start in _fields - could be faster..
./moses/moses/moses/representation/field_set.h:        // @todo: compute at the start in _fields - could be faster..
./moses/moses/moses/representation/field_set.h:    /// @todo rename that begin_bit for more consistency
./moses/moses/moses/representation/representation.h:                             // @todo: it is not sure whether we need
./moses/moses/moses/representation/knobs.h:    // @todo: it does not go back to the initiale state
./moses/moses/moses/representation/knobs.h:    // the shift is definitely in the wrong direction!! FIXME.
./moses/moses/moses/deme/feature_selector.cc:/// XXX TODO Explain what this function does. Why does it create a second
./moses/moses/moses/deme/feature_selector.cc:                    OC_ASSERT(false, "Not implemented");
./moses/moses/moses/deme/deme_expander.cc: * XXX TODO I honestly just don't see the utility of this multi-deme
./moses/moses/moses/deme/deme_expander.cc:                // TODO: DO NOT CHANGE THE MAX SCORE IF USER SET IT: BUT THAT
./moses/moses/moses/deme/deme_expander.cc:                // TODO: re-enable that once best_possible_bscore is fixed
./moses/moses/moses/deme/deme_expander.cc:        // XXX FIXME this is a bug .. the user may have specified that
./moses/moses/moses/optimization/hill-climbing.h:    // XXX TODO make sure this value is appropriately updated.
./moses/moses/moses/optimization/univariate.cc:                  "Trunction selection not implemented."
./moses/moses/moses/optimization/hill-climbing.cc:    using namespace boost::placeholders;
./moses/moses/moses/optimization/particle-swarm.cc:    using namespace boost::placeholders;
./moses/moses/moses/optimization/particle-swarm.cc:        // TODO: work in a better way to identify convergence.
./moses/moses/moses/optimization/particle-swarm.cc:// TODO: Explanation
./moses/moses/moses/optimization/particle-swarm.h:// TODO: pso description
./moses/moses/moses/optimization/particle-swarm.h:    // TODO: Wind dispersion, but test without first
./moses/moses/moses/optimization/star-anneal.cc:// XXX TODO the annealing temperature control code should be ported over
./moses/moses/moses/optimization/star-anneal.cc:    using namespace boost::placeholders;
./moses/moses/moses/optimization/star-anneal.cc:    // @todo this should be adapted for SA
./moses/moses/moses/optimization/star-anneal.h:     * distance.  @todo: it may be better to have the distance
./moses/moses/moses/main/problem-params.h:// XXX FIXME TODO The structure below should be split into multiple
./moses/moses/moses/main/problem-params.cc:    // XXX TODO: make this print correctly, instead of using brackets.
./moses/moses/moses/main/problem-params.cc:        // The remaining options (TODO organize this)
./moses/moses/moses/main/problem-params.cc:        ss << "Granularity " << time_bscore_granularity_str << " not implemented";
./moses/moses/moses/main/eval-candidate-likelihood.cc:        OC_ASSERT(false, "likelihood for problem %s is not implemented",
./moses/moses/moses/main/demo-problems.cc:        // @todo: Occam's razor and nsamples is not taken into account
./moses/moses/moses/main/demo-problems.cc:        // @todo: introduce some noise optionally
./moses/moses/moses/main/table-problems.cc:    // XXX FIXME -- the multiple tables should be merged into one.
./moses/moses/moses/main/table-problems.cc:    // XXX FIXME .. check that they all have the same signature.
./moses/moses/moses/eda/replacement.h:// TODO: I think it might be a little more efficent to use the
./moses/moses/moses/eda/optimize.h:                            std::bind(std::cref(score), std::placeholders::_1));
./moses/moses/moses/eda/optimize.h:                                std::bind(std::cref(score), std::placeholders::_1));
./moses/moses/moses/eda/local_structure.h:// XXX TODO document what this does...
./moses/moses/moses/eda/local_structure.h:// XXX TODO this is unclear, explain what is being accumulated where.
./moses/moses/moses/eda/local_structure.h:    using namespace boost::placeholders;
./moses/moses/moses/eda/local_structure.h:    using namespace boost::placeholders;
./moses/moses/moses/eda/local_structure.cc:                               std::bind(&field_set::get_raw, &_fields, std::placeholders::_1, idx),
./moses/moses/moses/eda/local_structure.cc:                               std::bind(&field_set::get_raw, &_fields, std::placeholders::_2, idx))) ==
./moses/moses/moses/eda/local_structure.cc:                             std::bind(valueof<const instance>, std::placeholders::_1), src_idx),
./moses/moses/moses/eda/local_structure.cc:                 std::bind(&local_structure_model::rec_split_term, this, std::placeholders::_1, std::placeholders::_2,
./moses/moses/moses/eda/local_structure.cc:                      src_idx + 1, idx, std::placeholders::_3, std::placeholders::_4));
./moses/moses/moses/eda/local_structure.cc:                             std::bind(valueof<const instance>, std::placeholders::_1), src_idx),
./moses/moses/moses/moses/neighborhood_sampling.cc:        // @todo: handle term algebras
./moses/moses/moses/moses/mpi_moses.cc:// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.
./moses/moses/moses/moses/mpi_moses.cc:        if (!dex.create_demes(exemplar, 0 /* TODO replace with the
./moses/moses/moses/moses/mpi_moses.cc:        // XXX TODO should probably fetch max_time from somewhere...
./moses/moses/moses/moses/mpi_moses.cc:    // OC_ASSERT(false, "TODO: understand what is the role source=0 exactly");
./moses/moses/moses/moses/mpi_moses.cc:        // XXX TODO instead of overwritting the demeID it should be
./moses/moses/moses/moses/partial.cc:        // XXX TODO: we need to get the actual number of gens run, back
./moses/moses/moses/moses/distributed_moses.cc:    // Arghhh FIXME. fuser might not be installed, or it may not be in
./moses/moses/moses/moses/moses_main.h:        // XXX TODO this should be fixed, someday...
./moses/moses/moses/moses/local_moses.cc:                // TODO use the option of the output
./moses/moses/moses/moses/types.h:// TODO this should be a std::valarray not std::vector but I am too
./moses/moses/moses/moses/mpi_moses.h:// this just right now. XXX TODO: do this, someday.
./moses/moses/moses/moses/complexity.cc:// we should count logical and, logical_or, below ..!?!? TODO, clarify.
./moses/moses/moses/moses/neighborhood_sampling.h: * @todo: in order to better approximate the real-number metric, we
./moses/moses/moses/moses/neighborhood_sampling.h: * @todo: term algebra fields are ignored for now
./moses/moses/moses/moses/neighborhood_sampling.h:                // @todo: now the distance is 1, choose the distance
./moses/moses/moses/moses/neighborhood_sampling.h: * XXX TODO: the current algo could be speeded up a fair bit, cutting
./moses/moses/moses/moses/neighborhood_sampling.h: * @todo: term algebra is ignored for the moment.
./moses/moses/moses/moses/neighborhood_sampling.h:        // @todo: handle term algebras XXX
./moses/moses/moses/moses/neighborhood_sampling.h:            // XXX TODO, unroll the last tail call, just like the single-bit
./moses/moses/moses/moses/neighborhood_sampling.h: * XXX/TODO: the performance of this thing can be strongly improved
