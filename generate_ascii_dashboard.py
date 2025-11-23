#!/usr/bin/env python3
"""
Generate ASCII art visualization of entelechy introspection
"""

import json


def create_bar_chart(value, max_width=40):
    """Create ASCII bar chart"""
    filled = int(value * max_width)
    empty = max_width - filled
    return '█' * filled + '░' * empty


def generate_ascii_dashboard():
    """Generate comprehensive ASCII dashboard"""
    
    # Load data
    with open('entelechy_introspection.json') as f:
        data = json.load(f)
    
    with open('fragmentations.json') as f:
        frag_data = json.load(f)
    
    report = []
    report.append("=" * 80)
    report.append("🧠 OPENCOG UNIFIED ENTELECHY INTROSPECTION - VISUAL DASHBOARD")
    report.append("=" * 80)
    report.append("")
    
    # Entelechy Metrics
    report.append("┌─ ENTELECHY ASSESSMENT ────────────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    
    metrics = [
        ("Actualization", data['entelechy_assessment']['actualization_score']),
        ("Completeness", data['entelechy_assessment']['completeness_score']),
        ("Coherence", data['entelechy_assessment']['coherence_score']),
        ("Vitality", data['entelechy_assessment']['vitality_score']),
        ("Alignment", data['entelechy_assessment']['alignment_score']),
    ]
    
    for name, value in metrics:
        bar = create_bar_chart(value, 40)
        status = "🟢" if value >= 0.7 else "🟡" if value >= 0.5 else "🔴"
        report.append(f"│  {name:13} {status} {bar} {value:5.1%}  │")
    
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Component Layer Health
    report.append("┌─ COMPONENT LAYER HEALTH ──────────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    
    layers = [
        ("Foundation", data['dimensional_insights']['ontological']['foundation_layer']['health']),
        ("Core", data['dimensional_insights']['ontological']['core_layer']['health']),
        ("Logic", data['dimensional_insights']['ontological']['specialized_layers']['logic']['health']),
        ("Cognitive", data['dimensional_insights']['ontological']['specialized_layers']['cognitive']['health']),
        ("Advanced", data['dimensional_insights']['ontological']['specialized_layers']['advanced']['health']),
        ("Learning", data['dimensional_insights']['ontological']['specialized_layers']['learning']['health']),
        ("Language", data['dimensional_insights']['ontological']['specialized_layers']['language']['health']),
        ("Integration", data['dimensional_insights']['ontological']['specialized_layers']['integration']['health']),
    ]
    
    for name, value in layers:
        bar = create_bar_chart(value, 40)
        status = "✅" if value >= 0.8 else "⚠️ " if value >= 0.6 else "❌"
        report.append(f"│  {name:13} {status} {bar} {value:5.1%}  │")
    
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Fragmentation Summary
    report.append("┌─ FRAGMENTATION ANALYSIS ──────────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    
    total = frag_data['total_fragments']
    critical = len(frag_data['by_severity']['critical'])
    high = len(frag_data['by_severity']['high'])
    medium = len(frag_data['by_severity']['medium'])
    low = len(frag_data['by_severity']['low'])
    
    report.append(f"│  Total Fragmentations: {total:>4}                                             │")
    report.append(f"│                                                                            │")
    report.append(f"│  🔴 Critical (≥0.8): {critical:>4}   {create_bar_chart(critical/total if total > 0 else 0, 30):30}  │")
    report.append(f"│  🟠 High (≥0.6):     {high:>4}   {create_bar_chart(high/total if total > 0 else 0, 30):30}  │")
    report.append(f"│  🟡 Medium (≥0.4):   {medium:>4}   {create_bar_chart(medium/total if total > 0 else 0, 30):30}  │")
    report.append(f"│  🟢 Low (<0.4):      {low:>4}   {create_bar_chart(low/total if total > 0 else 0, 30):30}  │")
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Top Fragmented Components
    report.append("┌─ TOP 10 MOST FRAGMENTED COMPONENTS ───────────────────────────────────────┐")
    report.append("│                                                                            │")
    
    top_comps = frag_data['top_fragmented_components'][:10]
    max_count = max(c['fragment_count'] for c in top_comps) if top_comps else 1
    
    for i, comp in enumerate(top_comps, 1):
        name = comp['component'][:20]
        count = comp['fragment_count']
        severity = comp['average_severity']
        bar = create_bar_chart(count / max_count, 25)
        report.append(f"│  {i:2}. {name:20} {count:>4} {bar} ({severity:.2f})  │")
    
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Multi-Dimensional Scores
    report.append("┌─ MULTI-DIMENSIONAL ANALYSIS ──────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    
    dimensions = [
        ("Ontological (Being)", min(data['dimensional_insights']['ontological']['architectural_completeness'], 1.0)),
        ("Teleological (Purpose)", data['dimensional_insights']['teleological']['actualization_trajectory']),
        ("Cognitive (Cognition)", data['dimensional_insights']['cognitive']['cognitive_completeness']),
        ("Integrative (Integration)", data['dimensional_insights']['integrative']['integration_health']),
        ("Evolutionary (Growth)", data['dimensional_insights']['evolutionary']['evolutionary_potential']),
    ]
    
    for name, value in dimensions:
        bar = create_bar_chart(value, 35)
        status = "🟢" if value >= 0.8 else "🟡" if value >= 0.6 else "🔴"
        report.append(f"│  {name:25} {status} {bar} {value:5.1%}  │")
    
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Code Health Markers
    report.append("┌─ CODE HEALTH MARKERS ─────────────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    
    todo = data['metrics']['todo_count']
    fixme = data['metrics']['fixme_count']
    stub = data['metrics']['stub_count']
    total_markers = todo + fixme + stub
    
    report.append(f"│  📋 TODO:   {todo:>5}  {create_bar_chart(todo/total_markers if total_markers > 0 else 0, 40):40}  │")
    report.append(f"│  🔧 FIXME:  {fixme:>5}  {create_bar_chart(fixme/total_markers if total_markers > 0 else 0, 40):40}  │")
    report.append(f"│  🚧 STUB:   {stub:>5}  {create_bar_chart(stub/total_markers if total_markers > 0 else 0, 40):40}  │")
    report.append(f"│  ═══════════════                                                           │")
    report.append(f"│  📊 TOTAL:  {total_markers:>5}                                                          │")
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Key Insights
    report.append("┌─ KEY INSIGHTS ────────────────────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    report.append("│  ✓ All 14 major components present and integrated (128.6% completeness)   │")
    report.append("│  ✓ 100% completion across all 5 development phases                        │")
    report.append("│  ✓ Perfect dependency satisfaction (27/27)                                 │")
    report.append("│  ✓ Excellent build and test integration                                   │")
    report.append("│  ✓ Strong meta-cognitive capabilities (AUTOGNOSIS, ONTOGENESIS)            │")
    report.append("│                                                                            │")
    report.append("│  ⚠ 2,591 implementation markers requiring attention                        │")
    report.append("│  ⚠ 76 critical fragmentations needing immediate repair                    │")
    report.append("│  ⚠ Code health at 0% due to high marker density                           │")
    report.append("│                                                                            │")
    report.append("│  → Primary challenge: Implementation depth, not architecture              │")
    report.append("│  → Opportunity: System has self-awareness of fragmentations               │")
    report.append("│  → Path forward: Systematic marker resolution                             │")
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    # Actualization Path
    report.append("┌─ ACTUALIZATION PATH ──────────────────────────────────────────────────────┐")
    report.append("│                                                                            │")
    report.append("│  Current State:  72% Actualized ████████████████████████░░░░░░░░░░░░░░░░  │")
    report.append("│  Target State:   95% Actualized █████████████████████████████████░░░░░░░  │")
    report.append("│                                                                            │")
    report.append("│  Gap Analysis: 23% improvement needed                                     │")
    report.append("│                                                                            │")
    report.append("│  Recommended Actions:                                                      │")
    report.append("│    1. Address 76 critical fragmentations              (Immediate)         │")
    report.append("│    2. Resolve 475 high-priority issues                (1-2 weeks)         │")
    report.append("│    3. Complete medium-priority improvements            (1-2 months)        │")
    report.append("│    4. Activate AUTOGNOSIS autonomous repair            (Strategic)        │")
    report.append("│                                                                            │")
    report.append("│  Estimated Timeline: 3-6 months to 95% actualization                      │")
    report.append("│                                                                            │")
    report.append("└────────────────────────────────────────────────────────────────────────────┘")
    report.append("")
    
    report.append("=" * 80)
    report.append("")
    report.append("💡 The system demonstrates healthy vital force with clear pathways to full")
    report.append("   actualization. Fragmentations represent conscious self-awareness of")
    report.append("   potential, not failure. With systematic repair, 95%+ realization is")
    report.append("   achievable through application of inherent meta-cognitive capabilities.")
    report.append("")
    report.append("=" * 80)
    
    return "\n".join(report)


if __name__ == '__main__':
    print("📊 Generating ASCII Dashboard...")
    dashboard = generate_ascii_dashboard()
    
    # Save to file
    with open('ENTELECHY_DASHBOARD.txt', 'w') as f:
        f.write(dashboard)
    
    # Print to console
    print(dashboard)
    
    print("\n✅ Dashboard saved to: ENTELECHY_DASHBOARD.txt")
