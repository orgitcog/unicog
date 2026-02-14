#!/usr/bin/env python3
"""
Complete Entelechy Framework Demonstration

This script demonstrates all major features of the Entelechy framework
for OpenCog Unified vital actualization assessment and optimization.
"""

import sys
import json
from pathlib import Path

# Add current directory to path
sys.path.insert(0, str(Path(__file__).parent))

from entelechy import (
    EntelechyIntrospector,
    EntelechyGenome,
    EntelechyTracker,
    SelfTranscendence,
    EntelechyOptimizer,
    detect_resonance,
)
from entelechy.resonance import calculate_dimensional_balance


def print_banner(text: str):
    """Print formatted banner"""
    print("\n" + "=" * 70)
    print(f"  {text}")
    print("=" * 70)


def demonstrate_introspection():
    """Demonstrate deep introspection"""
    print_banner("🧠 DEEP INTROSPECTION")
    
    introspector = EntelechyIntrospector(repo_path=".")
    report = introspector.perform_deep_introspection()
    
    assessment = report['entelechy_assessment']
    
    print(f"\n📊 Entelechy Assessment:")
    print(f"  Actualization:  {assessment['actualization_score']:.1%}")
    print(f"  Completeness:   {assessment['completeness_score']:.1%}")
    print(f"  Coherence:      {assessment['coherence_score']:.1%}")
    print(f"  Vitality:       {assessment['vitality_score']:.1%}")
    print(f"  Alignment:      {assessment['alignment_score']:.1%}")
    print(f"  Fitness:        {assessment['fitness']:.2f}")
    print(f"  Stage:          {assessment['development_stage']}")
    
    print(f"\n🔍 Fragmentation Analysis:")
    frag = report['fragmentation_analysis']
    print(f"  Total Fragments: {frag['total_fragments']}")
    print(f"  Critical:        {len(frag['critical_fragments'])}")
    
    if frag['critical_fragments']:
        print(f"\n⚠️  Critical Fragmentations:")
        for fragment in frag['critical_fragments'][:3]:
            print(f"    • {fragment['description']}")
            print(f"      Location: {fragment['location']}")
            print(f"      Severity: {fragment['severity']:.1%}")
    
    return introspector


def demonstrate_genome_tracking(introspector):
    """Demonstrate genome tracking"""
    print_banner("🧬 GENOME TRACKING")
    
    # Create genome from current metrics
    genome = EntelechyGenome.from_metrics(introspector.metrics)
    
    print(f"\n📋 Genome Information:")
    print(f"  ID:              {genome.id}")
    print(f"  Generation:      {genome.generation}")
    print(f"  Fitness:         {genome.fitness:.2f}")
    print(f"  Actualization:   {genome.actualization_level:.1%}")
    print(f"  Age:             {genome.age} iterations")
    
    print(f"\n🧬 Genetic Material:")
    for dimension, genes in genome.genes.items():
        if genes:
            print(f"  {dimension:15s}: {genes[-1]:.2f}")
    
    # Calculate genetic diversity
    diversity = genome.calculate_genetic_diversity()
    print(f"\n🌈 Genetic Diversity: {diversity:.2f}")
    
    return genome


def demonstrate_resonance(introspector):
    """Demonstrate resonance detection"""
    print_banner("🎵 RESONANCE DETECTION")
    
    metrics = introspector.metrics
    resonance = detect_resonance(metrics, threshold=0.7)
    
    print(f"\n🎶 Resonance Status:")
    print(f"  Resonating:      {resonance['resonating']}")
    print(f"  Quality:         {resonance['quality']}")
    print(f"  Strength:        {resonance['resonance_strength']:.1%}")
    print(f"  Mean Level:      {resonance['mean_level']:.1%}")
    print(f"  Variance:        {resonance['variance']:.4f}")
    
    print(f"\n📊 Dimensional Scores:")
    for dim, score in resonance['dimension_scores'].items():
        bar = "█" * int(score * 50)
        print(f"  {dim:15s}: {bar} {score:.1%}")
    
    print(f"\n🎯 Strongest: {resonance['strongest_dimension']}")
    print(f"  Weakest:   {resonance['weakest_dimension']}")
    
    # Check dimensional balance
    balance = calculate_dimensional_balance(metrics)
    print(f"\n⚖️  Dimensional Balance:")
    print(f"  Health:          {balance['health']:.1%}")
    print(f"  Imbalance:       {balance['imbalance']:.4f}")
    print(f"  Balanced:        {balance['balanced']}")


def demonstrate_transcendence(introspector):
    """Demonstrate transcendence assessment"""
    print_banner("🌟 TRANSCENDENCE ASSESSMENT")
    
    transcendence = SelfTranscendence(transcendence_threshold=0.8)
    assessment = transcendence.assess_transcendence_readiness(introspector.metrics)
    
    print(f"\n✨ Transcendence Status:")
    print(f"  Ready:           {assessment['ready_to_transcend']}")
    print(f"  Readiness Score: {assessment['readiness_score']:.1%}")
    print(f"  Actualization:   {assessment['actualization_level']:.1%}")
    print(f"  Threshold:       {assessment['threshold']:.1%}")
    print(f"  Stage:           {assessment['development_stage']}")
    
    if assessment['blocking_factors']:
        print(f"\n⚠️  Blocking Factors:")
        for factor in assessment['blocking_factors']:
            gap = factor['gap']
            print(f"  • {factor['factor']}")
            print(f"    Current: {factor['current']:.1%}, Need: {factor['required']:.1%} (gap: {gap:.1%})")
    
    if assessment['enabling_capabilities']:
        print(f"\n✅ Enabling Capabilities:")
        for cap in assessment['enabling_capabilities']:
            print(f"  • {cap}")
    
    if assessment['recommendations']:
        print(f"\n📋 Recommendations:")
        for rec in assessment['recommendations']:
            print(f"  • {rec}")
    
    # Discover emergent capabilities
    capabilities = transcendence.discover_emergent_capabilities(introspector.metrics)
    if capabilities:
        print(f"\n🚀 Emergent Capabilities Detected:")
        for cap in capabilities:
            print(f"  • {cap}")
    
    # Initiate if ready
    if assessment['ready_to_transcend']:
        print(f"\n🎆 Initiating Transcendence...")
        result = transcendence.initiate_transcendence(introspector.metrics)
        print(f"  Status: {result['status']}")
        print(f"  Autonomous: {result['autonomous']}")
        print(f"\n  Pathway Activated:")
        for key, value in result['pathway'].items():
            print(f"    • {key}: {value}")


def demonstrate_tracker():
    """Demonstrate evolutionary tracking"""
    print_banner("📈 EVOLUTIONARY TRACKING")
    
    tracker = EntelechyTracker(repo_path=".")
    
    # Take snapshot
    print(f"\n📸 Taking Snapshot...")
    snapshot = tracker.snapshot()
    
    print(f"  Timestamp:       {snapshot.timestamp}")
    print(f"  Actualization:   {snapshot.metrics['actualization_score']:.1%}")
    print(f"  Fitness:         {snapshot.fitness:.2f}")
    print(f"  Stage:           {snapshot.development_stage}")
    print(f"  Genome ID:       {snapshot.genome_id}")
    print(f"  Fragmentations:  {snapshot.fragmentations}")
    
    # Analyze trajectory if history exists
    trajectory = tracker.analyze_trajectory()
    
    if trajectory['status'] == 'analyzed':
        print(f"\n📊 Trajectory Analysis:")
        print(f"  Trajectory:      {trajectory['trajectory']}")
        print(f"  Actual. Gain:    {trajectory['actualization_gain']:+.2%}")
        print(f"  Fitness Gain:    {trajectory['fitness_gain']:+.2f}")
        print(f"  Frag. Reduction: {trajectory['fragmentation_reduction']:+d}")
        print(f"  Velocity:        {trajectory['velocity']:.4f} per day")
        print(f"  Time Span:       {trajectory['time_span_days']:.1f} days")
        
        # Predict future
        prediction = tracker.predict_future_state(days_ahead=30)
        print(f"\n🔮 Prediction (30 days ahead):")
        print(f"  Current:         {prediction['current_actualization']:.1%} ({prediction['current_stage']})")
        print(f"  Predicted:       {prediction['predicted_actualization']:.1%} ({prediction['predicted_stage']})")
        print(f"  Confidence:      {prediction['confidence']:.1%}")
    else:
        print(f"\n  {trajectory['message']}")


def demonstrate_optimization():
    """Demonstrate optimization"""
    print_banner("🎯 OPTIMIZATION")
    
    print(f"\n⚙️  Initializing Optimizer...")
    optimizer = EntelechyOptimizer(repo_path=".", learning_rate=0.1)
    
    # Generate improvements without actually applying them
    print(f"\n📝 Generating Improvement Suggestions...")
    
    introspector = EntelechyIntrospector(repo_path=".")
    report = introspector.perform_deep_introspection()
    
    from entelechy.types import EntelechyDimension
    
    for dimension in EntelechyDimension:
        improvements = optimizer.generate_improvements(dimension, introspector.metrics)
        if improvements:
            print(f"\n  {dimension.value.upper()}:")
            for imp in improvements[:2]:  # Show first 2
                print(f"    • {imp['description']}")
                print(f"      Priority: {imp['priority']}, Expected Gain: {imp['expected_gain']:.1%}")


def main():
    """Main demonstration"""
    print("\n" + "=" * 70)
    print("  🧠 ENTELECHY FRAMEWORK - COMPLETE DEMONSTRATION")
    print("  OpenCog Unified Vital Actualization System")
    print("=" * 70)
    print("\nThis demonstration showcases all major features of the Entelechy")
    print("framework for assessing and optimizing cognitive system actualization.")
    
    # 1. Deep Introspection
    introspector = demonstrate_introspection()
    
    # 2. Genome Tracking
    genome = demonstrate_genome_tracking(introspector)
    
    # 3. Resonance Detection
    demonstrate_resonance(introspector)
    
    # 4. Transcendence Assessment
    demonstrate_transcendence(introspector)
    
    # 5. Evolutionary Tracking
    demonstrate_tracker()
    
    # 6. Optimization
    demonstrate_optimization()
    
    # Final Summary
    print_banner("✨ DEMONSTRATION COMPLETE")
    print(f"\nThe Entelechy framework provides comprehensive vital actualization")
    print(f"assessment across five dimensions:")
    print(f"  🏛️  Ontological (BEING) - What the system IS")
    print(f"  🎯 Teleological (PURPOSE) - What it is BECOMING")
    print(f"  🧩 Cognitive (COGNITION) - How it THINKS")
    print(f"  🔗 Integrative (INTEGRATION) - How parts UNITE")
    print(f"  🌱 Evolutionary (GROWTH) - How it GROWS")
    print(f"\nFor more information, see ENTELECHY_README.md")
    print(f"Or run: python3 entelechy_cli.py --help")
    print()


if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nDemonstration interrupted.")
        sys.exit(0)
    except Exception as e:
        print(f"\n\nError: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
