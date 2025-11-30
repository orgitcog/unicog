#!/bin/bash
# Hypergraph Pattern Encoding Script
# Encodes workflow logic as dynamic hypergraph pattern nodes

set -e

echo "🕸️  Hypergraph Pattern Encoding for Workflow Logic"
echo "================================================="

# Initialize pattern encoding environment
HYPERGRAPH_REPORT="hypergraph-patterns.json"
ATOMESE_OUTPUT="workflow-patterns.atomese"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Function to extract workflow step patterns
extract_workflow_patterns() {
    echo "🔄 Extracting Workflow Step Patterns..."
    
    local workflow_steps=()
    local step_dependencies=()
    local step_count=0
    
    # Analyze GitHub workflow file for patterns
    if [[ -f ".github/workflows/bootstrap.yml" ]]; then
        echo "  📋 Analyzing bootstrap workflow patterns..."
        
        # Extract job names and dependencies
        local jobs=()
        while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                jobs+=("$line")
            fi
        done < <(grep -E "^[[:space:]]*[a-zA-Z_-]+:" ".github/workflows/bootstrap.yml" 2>/dev/null | grep -v "^[[:space:]]*name:" | sed 's/:.*//g' | sed 's/^[[:space:]]*//' || true)
        
        for job in "${jobs[@]}"; do
            if [[ -n "$job" && "$job" != "on" && "$job" != "permissions" ]]; then
                workflow_steps+=("job:$job")
                ((step_count++))
                echo "    🔗 Workflow job detected: $job"
                
                # Check for dependencies
                local needs_line=$(grep -A 10 "^[[:space:]]*$job:" ".github/workflows/bootstrap.yml" | grep "needs:" | head -1)
                if [[ -n "$needs_line" ]]; then
                    local dependency=$(echo "$needs_line" | sed 's/.*needs:[[:space:]]*//' | sed 's/[[:space:]]*$//')
                    step_dependencies+=("$job->$dependency")
                    echo "      ↳ Depends on: $dependency"
                fi
            fi
        done
        
        # Extract step patterns within jobs
        local steps=()
        while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                steps+=("$line")
            fi
        done < <(grep -E "^[[:space:]]*-[[:space:]]*name:" ".github/workflows/bootstrap.yml" 2>/dev/null | sed 's/.*name:[[:space:]]*//' | sed 's/[[:space:]]*$//' || true)
        
        for step in "${steps[@]}"; do
            if [[ -n "$step" ]]; then
                workflow_steps+=("step:$(echo "$step" | tr ' ' '_')")
                ((step_count++))
                echo "    ⚙️  Workflow step detected: $step"
            fi
        done
    fi
    
    export WORKFLOW_STEPS="${workflow_steps[*]}"
    export STEP_DEPENDENCIES="${step_dependencies[*]}"
    export STEP_COUNT="$step_count"
    
    echo "  ✅ Extracted $step_count workflow patterns"
}

# Function to encode cognitive process patterns
encode_cognitive_patterns() {
    echo ""
    echo "🧠 Encoding Cognitive Process Patterns..."
    
    local cognitive_nodes=()
    local cognitive_edges=()
    local process_count=0
    
    # Analyze test scripts for cognitive patterns
    while IFS= read -r -d '' script; do
        local script_name=$(basename "$script")
        echo "  🔍 Analyzing cognitive patterns in $script_name..."
        
        # Extract function patterns
        local functions=($(grep -E "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*\(\)" "$script" | sed 's/[[:space:]]*().*//g' | sed 's/^[[:space:]]*//' || echo ""))
        
        for func in "${functions[@]}"; do
            if [[ -n "$func" ]]; then
                cognitive_nodes+=("function:${script_name}:${func}")
                ((process_count++))
                echo "    🔗 Cognitive function: $func in $script_name"
            fi
        done
        
        # Extract verification patterns
        local verifications=$(grep -E "(verify|test|check|validate)" "$script" 2>/dev/null | wc -l)
        verifications=${verifications//[^0-9]/}  # Remove non-numeric characters
        verifications=${verifications:-0}  # Default to 0 if empty
        if [[ $verifications -gt 0 ]]; then
            cognitive_nodes+=("verification:${script_name}:${verifications}_checks")
            cognitive_edges+=("${script_name}->verification_process")
            echo "    ✅ Verification patterns: $verifications checks in $script_name"
        fi
        
        # Extract attention patterns
        local attention_patterns=$(grep -c -i "attention\|focus\|cognitive\|ecan" "$script" 2>/dev/null || echo "0")
        attention_patterns=${attention_patterns//[^0-9]/}  # Remove non-numeric characters
        attention_patterns=${attention_patterns:-0}  # Default to 0 if empty
        if [[ $attention_patterns -gt 0 ]]; then
            cognitive_nodes+=("attention:${script_name}:${attention_patterns}_patterns")
            cognitive_edges+=("${script_name}->attention_allocation")
            echo "    🧠 Attention patterns: $attention_patterns in $script_name"
        fi
        
    done < <(find . -name "*.sh" -executable -print0 2>/dev/null)
    
    export COGNITIVE_NODES="${cognitive_nodes[*]}"
    export COGNITIVE_EDGES="${cognitive_edges[*]}"
    export PROCESS_COUNT="$process_count"
    
    echo "  ✅ Encoded $process_count cognitive process patterns"
}

# Function to map code structure to hypergraph
map_code_to_hypergraph() {
    echo ""
    echo "🗂️  Mapping Code Structure to Hypergraph..."
    
    local code_nodes=()
    local code_hyperedges=()
    local structure_count=0
    
    # Map C++ files to hypernodes
    while IFS= read -r -d '' file; do
        local filename=$(basename "$file")
        local classes=$(grep -c "^[[:space:]]*class[[:space:]]" "$file" 2>/dev/null || echo "0")
        classes=${classes//[^0-9]/}  # Remove non-numeric characters
        classes=${classes:-0}  # Default to 0 if empty
        local functions=$(grep -c "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*(" "$file" 2>/dev/null || echo "0")
        functions=${functions//[^0-9]/}  # Remove non-numeric characters
        functions=${functions:-0}  # Default to 0 if empty
        local includes=$(grep -c "^[[:space:]]*#include" "$file" 2>/dev/null || echo "0")
        includes=${includes//[^0-9]/}  # Remove non-numeric characters
        includes=${includes:-0}  # Default to 0 if empty
        
        if [[ $((classes + functions)) -gt 0 ]]; then
            code_nodes+=("cpp:$filename:classes_${classes}:functions_${functions}")
            ((structure_count++))
            
            # Create hyperedges for file relationships
            if [[ $includes -gt 0 ]]; then
                code_hyperedges+=("$filename->cpp_dependencies:$includes")
            fi
            
            echo "    📊 C++ file: $filename (classes: $classes, functions: $functions)"
        fi
        
    done < <(find . -name "*.cc" -o -name "*.cpp" -o -name "*.h" -o -name "*.hpp" -print0 2>/dev/null)
    
    # Map Scheme files to hypernodes
    while IFS= read -r -d '' file; do
        local filename=$(basename "$file")
        local definitions=$(grep -c "^[[:space:]]*([[:space:]]*define" "$file" 2>/dev/null || echo "0")
        definitions=${definitions//[^0-9]/}  # Remove non-numeric characters
        definitions=${definitions:-0}  # Default to 0 if empty
        local lambdas=$(grep -c "lambda" "$file" 2>/dev/null || echo "0")
        lambdas=${lambdas//[^0-9]/}  # Remove non-numeric characters
        lambdas=${lambdas:-0}  # Default to 0 if empty
        local modules=$(grep -c "use-modules" "$file" 2>/dev/null || echo "0")
        modules=${modules//[^0-9]/}  # Remove non-numeric characters
        modules=${modules:-0}  # Default to 0 if empty
        
        if [[ $((definitions + lambdas)) -gt 0 ]]; then
            code_nodes+=("scheme:$filename:definitions_${definitions}:lambdas_${lambdas}")
            ((structure_count++))
            
            # Create hyperedges for module relationships
            if [[ $modules -gt 0 ]]; then
                code_hyperedges+=("$filename->scheme_modules:$modules")
            fi
            
            echo "    🎭 Scheme file: $filename (definitions: $definitions, lambdas: $lambdas)"
        fi
        
    done < <(find . -name "*.scm" -print0 2>/dev/null)
    
    export CODE_NODES="${code_nodes[*]}"
    export CODE_HYPEREDGES="${code_hyperedges[*]}"
    export STRUCTURE_COUNT="$structure_count"
    
    echo "  ✅ Mapped $structure_count code structures to hypergraph"
}

# Function to generate Atomese representation
generate_atomese_patterns() {
    echo ""
    echo "⚛️  Generating Atomese Hypergraph Representation..."
    
    cat > "$ATOMESE_OUTPUT" << 'EOF'
; OpenCog Unified Workflow Hypergraph Patterns
; Generated automatically from bootstrap workflow analysis
; Timestamp: $TIMESTAMP

; === WORKFLOW STEP PATTERNS ===
EOF
    
    # Add workflow steps as concept nodes
    echo "; Workflow Step Concepts" >> "$ATOMESE_OUTPUT"
    IFS=' ' read -ra steps <<< "$WORKFLOW_STEPS"
    for step in "${steps[@]}"; do
        if [[ -n "$step" ]]; then
            step_type=${step%%:*}
            step_name=${step##*:}
            echo "(ConceptNode \"$step_name\")" >> "$ATOMESE_OUTPUT"
            echo "(InheritanceLink" >> "$ATOMESE_OUTPUT"
            echo "  (ConceptNode \"$step_name\")" >> "$ATOMESE_OUTPUT"
            echo "  (ConceptNode \"${step_type}_pattern\"))" >> "$ATOMESE_OUTPUT"
            echo "" >> "$ATOMESE_OUTPUT"
        fi
    done
    
    # Add step dependencies as evaluation links
    echo "; Workflow Dependencies" >> "$ATOMESE_OUTPUT"
    IFS=' ' read -ra deps <<< "$STEP_DEPENDENCIES"
    for dep in "${deps[@]}"; do
        if [[ -n "$dep" ]]; then
            source=${dep%%->*}
            target=${dep##*->}
            echo "(EvaluationLink" >> "$ATOMESE_OUTPUT"
            echo "  (PredicateNode \"depends_on\")" >> "$ATOMESE_OUTPUT"
            echo "  (ListLink" >> "$ATOMESE_OUTPUT"
            echo "    (ConceptNode \"$source\")" >> "$ATOMESE_OUTPUT"
            echo "    (ConceptNode \"$target\")))" >> "$ATOMESE_OUTPUT"
            echo "" >> "$ATOMESE_OUTPUT"
        fi
    done
    
    # Add cognitive process patterns
    cat >> "$ATOMESE_OUTPUT" << 'EOF'

; === COGNITIVE PROCESS PATTERNS ===
; Cognitive Function Hierarchies
EOF
    
    IFS=' ' read -ra cog_nodes <<< "$COGNITIVE_NODES"
    for node in "${cog_nodes[@]}"; do
        if [[ -n "$node" ]]; then
            IFS=':' read -ra parts <<< "$node"
            if [[ ${#parts[@]} -ge 3 ]]; then
                type=${parts[0]}
                script=${parts[1]}
                name=${parts[2]}
                
                echo "(ConceptNode \"$name\")" >> "$ATOMESE_OUTPUT"
                echo "(InheritanceLink" >> "$ATOMESE_OUTPUT"
                echo "  (ConceptNode \"$name\")" >> "$ATOMESE_OUTPUT"
                echo "  (ConceptNode \"${type}_process\"))" >> "$ATOMESE_OUTPUT"
                echo "(EvaluationLink" >> "$ATOMESE_OUTPUT"
                echo "  (PredicateNode \"implemented_in\")" >> "$ATOMESE_OUTPUT"
                echo "  (ListLink" >> "$ATOMESE_OUTPUT"
                echo "    (ConceptNode \"$name\")" >> "$ATOMESE_OUTPUT"
                echo "    (ConceptNode \"$script\")))" >> "$ATOMESE_OUTPUT"
                echo "" >> "$ATOMESE_OUTPUT"
            fi
        fi
    done
    
    # Add cognitive edges
    IFS=' ' read -ra cog_edges <<< "$COGNITIVE_EDGES"
    for edge in "${cog_edges[@]}"; do
        if [[ -n "$edge" ]]; then
            source=${edge%%->*}
            target=${edge##*->}
            echo "(EvaluationLink" >> "$ATOMESE_OUTPUT"
            echo "  (PredicateNode \"cognitive_flow\")" >> "$ATOMESE_OUTPUT"
            echo "  (ListLink" >> "$ATOMESE_OUTPUT"
            echo "    (ConceptNode \"$source\")" >> "$ATOMESE_OUTPUT"
            echo "    (ConceptNode \"$target\")))" >> "$ATOMESE_OUTPUT"
            echo "" >> "$ATOMESE_OUTPUT"
        fi
    done
    
    # Add code structure patterns
    cat >> "$ATOMESE_OUTPUT" << 'EOF'

; === CODE STRUCTURE HYPERGRAPH ===
; Multi-language Code Integration Patterns
EOF
    
    IFS=' ' read -ra code_nodes <<< "$CODE_NODES"
    for node in "${code_nodes[@]}"; do
        if [[ -n "$node" ]]; then
            IFS=':' read -ra parts <<< "$node"
            if [[ ${#parts[@]} -ge 4 ]]; then
                lang=${parts[0]}
                filename=${parts[1]}
                classes=${parts[2]}
                functions=${parts[3]}
                
                echo "(ConceptNode \"$filename\")" >> "$ATOMESE_OUTPUT"
                echo "(InheritanceLink" >> "$ATOMESE_OUTPUT"
                echo "  (ConceptNode \"$filename\")" >> "$ATOMESE_OUTPUT"
                echo "  (ConceptNode \"${lang}_file\"))" >> "$ATOMESE_OUTPUT"
                
                # Add complexity metrics as number nodes
                class_count=${classes##*_}
                function_count=${functions##*_}
                echo "(EvaluationLink" >> "$ATOMESE_OUTPUT"
                echo "  (PredicateNode \"complexity_metrics\")" >> "$ATOMESE_OUTPUT"
                echo "  (ListLink" >> "$ATOMESE_OUTPUT"
                echo "    (ConceptNode \"$filename\")" >> "$ATOMESE_OUTPUT"
                echo "    (NumberNode $class_count)" >> "$ATOMESE_OUTPUT"
                echo "    (NumberNode $function_count)))" >> "$ATOMESE_OUTPUT"
                echo "" >> "$ATOMESE_OUTPUT"
            fi
        fi
    done
    
    # Add hyperedges for cross-language integration
    cat >> "$ATOMESE_OUTPUT" << 'EOF'

; === NEURAL-SYMBOLIC INTEGRATION HYPEREDGES ===
; Cross-language Integration Patterns
(ConceptNode "neural_symbolic_integration")
(InheritanceLink
  (ConceptNode "neural_symbolic_integration")
  (ConceptNode "cognitive_architecture"))

; Attention Allocation Hypergraph
(ConceptNode "attention_allocation")
(EvaluationLink
  (PredicateNode "cognitive_resource_mapping")
  (ListLink
    (ConceptNode "attention_allocation")
    (ConceptNode "workflow_optimization")))

; Meta-cognitive Feedback Loop
(ConceptNode "recursive_enhancement")
(EvaluationLink
  (PredicateNode "feedback_loop")
  (ListLink
    (ConceptNode "workflow_patterns")
    (ConceptNode "recursive_enhancement")))
EOF
    
    echo "✅ Atomese hypergraph patterns saved to: $ATOMESE_OUTPUT"
}

# Function to create JSON-LD representation
generate_jsonld_patterns() {
    echo ""
    echo "🌐 Generating JSON-LD Hypergraph Representation..."
    
    local total_nodes=$((STEP_COUNT + PROCESS_COUNT + STRUCTURE_COUNT))
    local total_edges=$(echo "$STEP_DEPENDENCIES $COGNITIVE_EDGES $CODE_HYPEREDGES" | wc -w)
    
    cat > "$HYPERGRAPH_REPORT" << EOF
{
  "@context": {
    "og": "http://opencog.org/schema/",
    "workflow": "http://opencog.org/workflow/",
    "cognitive": "http://opencog.org/cognitive/",
    "hypergraph": "http://opencog.org/hypergraph/"
  },
  "timestamp": "$TIMESTAMP",
  "hypergraph_patterns": {
    "@type": "hypergraph:CognitiveWorkflowGraph",
    "workflow_step_patterns": {
      "total_steps": $STEP_COUNT,
      "step_nodes": [
$(IFS=' '; for step in $WORKFLOW_STEPS; do
    step_type=${step%%:*}
    step_name=${step##*:}
    echo "        {\"@type\": \"workflow:${step_type^}Node\", \"name\": \"$step_name\", \"id\": \"workflow:$step_name\"},"
done | sed '$ s/,$//')
      ],
      "step_dependencies": [
$(IFS=' '; for dep in $STEP_DEPENDENCIES; do
    source=${dep%%->*}
    target=${dep##*->}
    echo "        {\"@type\": \"workflow:DependencyEdge\", \"source\": \"workflow:$source\", \"target\": \"workflow:$target\"},"
done | sed '$ s/,$//')
      ]
    },
    "cognitive_process_patterns": {
      "total_processes": $PROCESS_COUNT,
      "cognitive_nodes": [
$(IFS=' '; for node in $COGNITIVE_NODES; do
    IFS=':' read -ra parts <<< "$node"
    if [[ ${#parts[@]} -ge 3 ]]; then
        type=${parts[0]}
        script=${parts[1]}
        name=${parts[2]}
        echo "        {\"@type\": \"cognitive:${type^}Process\", \"name\": \"$name\", \"script\": \"$script\", \"id\": \"cognitive:$name\"},"
    fi
done | sed '$ s/,$//')
      ],
      "cognitive_edges": [
$(IFS=' '; for edge in $COGNITIVE_EDGES; do
    source=${edge%%->*}
    target=${edge##*->}
    echo "        {\"@type\": \"cognitive:FlowEdge\", \"source\": \"cognitive:$source\", \"target\": \"cognitive:$target\"},"
done | sed '$ s/,$//')
      ]
    },
    "code_structure_hypergraph": {
      "total_structures": $STRUCTURE_COUNT,
      "code_nodes": [
$(IFS=' '; for node in $CODE_NODES; do
    IFS=':' read -ra parts <<< "$node"
    if [[ ${#parts[@]} -ge 4 ]]; then
        lang=${parts[0]}
        filename=${parts[1]}
        classes=${parts[2]}
        functions=${parts[3]}
        class_count=${classes##*_}
        function_count=${functions##*_}
        echo "        {\"@type\": \"og:${lang^}File\", \"filename\": \"$filename\", \"classes\": $class_count, \"functions\": $function_count, \"id\": \"og:$filename\"},"
    fi
done | sed '$ s/,$//')
      ],
      "code_hyperedges": [
$(IFS=' '; for edge in $CODE_HYPEREDGES; do
    source=${edge%%->*}
    target_info=${edge##*->}
    target=${target_info%%:*}
    count=${target_info##*:}
    echo "        {\"@type\": \"og:DependencyHyperedge\", \"source\": \"og:$source\", \"target\": \"$target\", \"strength\": $count},"
done | sed '$ s/,$//')
      ]
    },
    "hypergraph_metrics": {
      "total_nodes": $total_nodes,
      "total_edges": $total_edges,
      "graph_density": $(echo "scale=3; $total_edges / ($total_nodes * ($total_nodes - 1) / 2)" | bc -l 2>/dev/null || echo "0.001"),
      "cognitive_coherence": $((total_nodes * 100 / (total_edges + 1))),
      "integration_strength": "$(if [[ $total_edges -gt $total_nodes ]]; then echo "high"; elif [[ $total_edges -gt $((total_nodes / 2)) ]]; then echo "medium"; else echo "low"; fi)",
      "hypergraph_complexity": "$(if [[ $total_nodes -gt 50 ]]; then echo "high"; elif [[ $total_nodes -gt 20 ]]; then echo "medium"; else echo "low"; fi)"
    },
    "meta_patterns": {
      "neural_symbolic_bridges": $(echo "$CODE_NODES" | grep -c "cpp.*scheme\|scheme.*cpp" || echo "0"),
      "attention_allocation_nodes": $(echo "$COGNITIVE_NODES" | grep -c "attention" || echo "0"),
      "recursive_feedback_loops": $(echo "$COGNITIVE_EDGES" | grep -c "feedback\|recursive" || echo "1"),
      "tensor_field_coherence": $((total_nodes + total_edges)),
      "cognitive_grammar_fragments": $((STEP_COUNT + PROCESS_COUNT))
    }
  }
}
EOF
    
    echo "✅ JSON-LD hypergraph patterns saved to: $HYPERGRAPH_REPORT"
}

# Main execution
main() {
    echo "Starting hypergraph pattern encoding at $TIMESTAMP"
    echo ""
    
    # Run all pattern encoding functions
    extract_workflow_patterns
    encode_cognitive_patterns  
    map_code_to_hypergraph
    generate_atomese_patterns
    generate_jsonld_patterns
    
    echo ""
    echo "🎉 Hypergraph Pattern Encoding Complete!"
    echo "📊 Results summary:"
    echo "   - Workflow Patterns: $STEP_COUNT steps encoded"
    echo "   - Cognitive Processes: $PROCESS_COUNT processes mapped"
    echo "   - Code Structures: $STRUCTURE_COUNT files analyzed"
    echo "   - Atomese Output: $ATOMESE_OUTPUT"
    echo "   - JSON-LD Output: $HYPERGRAPH_REPORT"
    echo ""
    echo "🕸️  Hypergraph cognitive grammar fragments ready for AtomSpace ingestion!"
}

# Run main function  
main "$@"