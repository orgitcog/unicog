"""
OpenCog Unified Entelechy Framework

Comprehensive vital actualization framework for cognitive systems.
Implements self-actualizing, self-organizing, and self-transcending intelligence
through multi-dimensional developmental processes.
"""

from .introspector import EntelechyIntrospector
from .genome import EntelechyGenome
from .tracker import EntelechyTracker
from .resonance import detect_resonance
from .transcendence import SelfTranscendence
from .optimizer import EntelechyOptimizer
from .types import (
    EntelechyDimension,
    FragmentationType,
    FragmentationSignature,
    EntelechyMetrics,
    ComponentState,
    DevelopmentStage
)

__version__ = "1.0.0"
__all__ = [
    "EntelechyIntrospector",
    "EntelechyGenome",
    "EntelechyTracker",
    "detect_resonance",
    "SelfTranscendence",
    "EntelechyOptimizer",
    "EntelechyDimension",
    "FragmentationType",
    "FragmentationSignature",
    "EntelechyMetrics",
    "ComponentState",
    "DevelopmentStage",
]
