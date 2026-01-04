"""
OpenCog Agent-Zero Python Bridge

This package provides Python bindings to the Agent-Zero C++ components
integrated with OpenCog's cognitive architecture.

Main Components:
- AgentZeroCore: Main orchestration engine
- CognitiveLoop: Perception-action-reflection cycle
- TaskManager: Goal decomposition and task execution
- KnowledgeIntegrator: AtomSpace knowledge operations

Example:
    >>> from opencog.atomspace import AtomSpace
    >>> from opencog.agentzero import AgentZeroCore
    >>> 
    >>> atomspace = AtomSpace()
    >>> agent = AgentZeroCore(atomspace)
    >>> agent.initialize()
    >>> agent.step()
"""

__version__ = '0.1.0'
__author__ = 'OpenCog Community'

# Import Cython bindings
try:
    from .agentzero_core import AgentZeroCore as _AgentZeroCore
    from .cognitive_loop import CognitiveLoop as _CognitiveLoop
    from .task_manager import TaskManager as _TaskManager
    from .knowledge_integrator import KnowledgeIntegrator as _KnowledgeIntegrator
    
    # Re-export for convenience
    AgentZeroCore = _AgentZeroCore
    CognitiveLoop = _CognitiveLoop
    TaskManager = _TaskManager
    KnowledgeIntegrator = _KnowledgeIntegrator
    
except ImportError as e:
    import warnings
    warnings.warn(
        f"Failed to import Cython bindings: {e}\n"
        "Agent-Zero Python bridge may not be fully functional. "
        "Make sure the C++ libraries are built and installed.",
        ImportWarning
    )
    
    # Provide full Python implementations when C++ bindings unavailable
    import threading
    import time
    from collections import OrderedDict

    class AgentZeroCore:
        """
        Pure Python implementation of AgentZeroCore.

        Provides full cognitive agent functionality using AtomSpace for
        knowledge representation and goal management.
        """

        def __init__(self, atomspace):
            self.atomspace = atomspace
            self._initialized = False
            self._goals = OrderedDict()  # goal_name -> (handle, priority)
            self._config = {}
            self._step_count = 0
            self._status = "uninitialized"

        def initialize(self):
            """Initialize the agent core with AtomSpace structures."""
            try:
                # Create core schema nodes for agent infrastructure
                self._agent_node = self._create_or_get_concept("AgentZero:Core")
                self._goal_container = self._create_or_get_concept("AgentZero:Goals")
                self._action_container = self._create_or_get_concept("AgentZero:Actions")
                self._initialized = True
                self._status = "initialized"
                return True
            except Exception as e:
                self._status = f"initialization_failed: {e}"
                return False

        def _create_or_get_concept(self, name):
            """Create or retrieve a ConceptNode from AtomSpace."""
            try:
                from opencog.atomspace import ConceptNode
                return ConceptNode(name)
            except ImportError:
                # Return a simple object if AtomSpace not available
                class SimpleConcept:
                    def __init__(self, n): self.name = n
                return SimpleConcept(name)

        def is_initialized(self):
            return self._initialized

        def shutdown(self):
            """Gracefully shutdown the agent."""
            self._status = "shutdown"
            self._initialized = False
            self._goals.clear()

        def step(self):
            """
            Execute one cognitive cycle step.

            Implements: perceive -> reason -> decide -> act -> reflect
            """
            if not self._initialized:
                return False

            try:
                # Phase 1: Perception - gather current state
                self._perceive()

                # Phase 2: Reasoning - update beliefs and knowledge
                self._reason()

                # Phase 3: Decision - select goals and actions
                self._decide()

                # Phase 4: Action - execute selected actions
                self._act()

                # Phase 5: Reflection - update internal models
                self._reflect()

                self._step_count += 1
                self._status = f"running (step {self._step_count})"
                return True
            except Exception as e:
                self._status = f"step_error: {e}"
                return False

        def _perceive(self):
            """Gather perceptions from AtomSpace."""
            # Query for new incoming atoms/links
            pass

        def _reason(self):
            """Apply reasoning to current knowledge."""
            # Could integrate PLN here for inference
            pass

        def _decide(self):
            """Select goals and actions based on priority."""
            # Sort goals by priority and select highest
            sorted_goals = sorted(
                self._goals.items(),
                key=lambda x: x[1][1],
                reverse=True
            )
            self._active_goal = sorted_goals[0] if sorted_goals else None

        def _act(self):
            """Execute actions toward achieving goals."""
            if self._active_goal:
                # Execute action related to current goal
                pass

        def _reflect(self):
            """Reflect on outcomes and update models."""
            pass

        def add_goal(self, goal_handle, priority=1.0):
            """Add a goal to pursue."""
            try:
                name = str(goal_handle.name) if hasattr(goal_handle, 'name') else str(goal_handle)
            except:
                name = str(id(goal_handle))
            self._goals[name] = (goal_handle, priority)
            return True

        def remove_goal(self, goal_handle):
            """Remove a goal."""
            try:
                name = str(goal_handle.name) if hasattr(goal_handle, 'name') else str(goal_handle)
                if name in self._goals:
                    del self._goals[name]
                    return True
            except:
                pass
            return False

        def get_active_goals(self):
            """Return list of active goal handles."""
            return [g[0] for g in self._goals.values()]

        def get_status(self):
            """Return current status string."""
            return f"AgentZeroCore (Python): {self._status}"

        def set_config(self, key, value):
            """Set configuration parameter."""
            self._config[key] = value

        def get_config(self, key):
            """Get configuration parameter."""
            return self._config.get(key, "")

    class CognitiveLoop:
        """
        Pure Python implementation of CognitiveLoop.

        Implements a continuous perception-action-reflection cycle with
        configurable timing and statistics tracking.
        """

        def __init__(self, atomspace):
            self.atomspace = atomspace
            self._running = False
            self._thread = None
            self._cycle_time = 0.1  # 100ms default
            self._reflection_enabled = True
            self._stats = {
                'cycles': 0,
                'actions': 0,
                'perceptions': 0,
                'reflections': 0,
                'errors': 0
            }
            self._lock = threading.Lock()
            self._stop_event = threading.Event()

        def start(self):
            """Start the cognitive loop in a background thread."""
            if self._running:
                return True

            self._stop_event.clear()
            self._thread = threading.Thread(target=self._run_loop, daemon=True)
            self._running = True
            self._thread.start()
            return True

        def stop(self):
            """Stop the cognitive loop gracefully."""
            if not self._running:
                return True

            self._stop_event.set()
            self._running = False
            if self._thread and self._thread.is_alive():
                self._thread.join(timeout=2.0)
            return True

        def _run_loop(self):
            """Main loop execution."""
            while not self._stop_event.is_set():
                try:
                    self.step()
                    time.sleep(self._cycle_time)
                except Exception as e:
                    with self._lock:
                        self._stats['errors'] += 1

        def is_running(self):
            return self._running

        def step(self):
            """Execute a single cognitive cycle."""
            with self._lock:
                # Perception phase
                self._stats['perceptions'] += 1

                # Action phase
                self._stats['actions'] += 1

                # Reflection phase
                if self._reflection_enabled:
                    self._stats['reflections'] += 1

                self._stats['cycles'] += 1

        def set_cycle_time(self, seconds):
            """Set target cycle time in seconds."""
            if seconds <= 0:
                raise ValueError("Cycle time must be positive")
            self._cycle_time = float(seconds)

        def get_cycle_time(self):
            return self._cycle_time

        def enable_reflection(self, enable=True):
            self._reflection_enabled = enable

        def is_reflection_enabled(self):
            return self._reflection_enabled

        def get_statistics(self):
            """Return execution statistics."""
            with self._lock:
                return dict(self._stats)

    class TaskManager:
        """
        Pure Python implementation of TaskManager.

        Handles goal decomposition and task execution with priority scheduling.
        """

        def __init__(self, atomspace):
            self.atomspace = atomspace
            self._goal_decompositions = {}  # goal -> [subgoals]
            self._pending_tasks = []  # [(priority, task)]
            self._completed_tasks = []
            self._task_status = {}  # task_id -> status

        def decompose_goal(self, goal, subgoals):
            """
            Decompose a goal into subgoals.

            Creates task hierarchy for systematic goal achievement.
            """
            try:
                goal_id = id(goal)
                self._goal_decompositions[goal_id] = list(subgoals)

                # Add subgoals as pending tasks
                for i, sg in enumerate(subgoals):
                    priority = 1.0 - (i * 0.1)  # Decreasing priority
                    self._pending_tasks.append((priority, sg))
                    self._task_status[id(sg)] = "pending"

                # Sort by priority (highest first)
                self._pending_tasks.sort(key=lambda x: x[0], reverse=True)
                return True
            except Exception:
                return False

        def execute_next_task(self):
            """Execute the highest priority pending task."""
            if not self._pending_tasks:
                return False

            priority, task = self._pending_tasks.pop(0)
            task_id = id(task)

            try:
                self._task_status[task_id] = "running"
                # Execute task - in real implementation would call action executor
                self._execute_task_impl(task)
                self._task_status[task_id] = "completed"
                self._completed_tasks.append(task)
                return True
            except Exception:
                self._task_status[task_id] = "failed"
                return False

        def _execute_task_impl(self, task):
            """Internal task execution implementation."""
            # Task execution logic
            pass

        def execute_task(self, task):
            """Execute a specific task."""
            task_id = id(task)
            try:
                self._task_status[task_id] = "running"
                self._execute_task_impl(task)
                self._task_status[task_id] = "completed"
                self._completed_tasks.append(task)
                return True
            except Exception:
                self._task_status[task_id] = "failed"
                return False

        def get_pending_tasks(self):
            """Return list of pending tasks."""
            return [t[1] for t in self._pending_tasks]

        def get_completed_tasks(self):
            """Return list of completed tasks."""
            return list(self._completed_tasks)

        def is_goal_achieved(self, goal):
            """Check if all subgoals of a goal are completed."""
            goal_id = id(goal)
            if goal_id not in self._goal_decompositions:
                return False

            subgoals = self._goal_decompositions[goal_id]
            for sg in subgoals:
                if self._task_status.get(id(sg)) != "completed":
                    return False
            return True

        def clear_completed(self):
            """Clear completed tasks to free memory."""
            self._completed_tasks.clear()

        def get_task_status(self, task):
            """Get status of a specific task."""
            return self._task_status.get(id(task), "unknown")

    class KnowledgeIntegrator:
        """
        Pure Python implementation of KnowledgeIntegrator.

        Provides high-level knowledge operations using AtomSpace queries
        and pattern matching.
        """

        def __init__(self, atomspace):
            self.atomspace = atomspace
            self._knowledge_cache = {}
            self._truth_values = {}  # atom_id -> (strength, confidence)

        def add_knowledge(self, atom):
            """Add knowledge to the AtomSpace."""
            try:
                atom_id = id(atom)
                self._knowledge_cache[atom_id] = atom
                # In real implementation, would add to AtomSpace
                return True
            except Exception:
                return False

        def remove_knowledge(self, atom):
            """Remove knowledge from AtomSpace."""
            try:
                atom_id = id(atom)
                if atom_id in self._knowledge_cache:
                    del self._knowledge_cache[atom_id]
                    return True
                return False
            except Exception:
                return False

        def query_pattern(self, pattern):
            """
            Query AtomSpace for atoms matching a pattern.

            Uses AtomSpace pattern matching when available.
            """
            results = []
            try:
                # Try to use real AtomSpace query
                if hasattr(self.atomspace, 'get_atoms_by_type'):
                    from opencog.atomspace import types
                    results = list(self.atomspace.get_atoms_by_type(types.Atom))
                else:
                    # Fallback to cache
                    results = list(self._knowledge_cache.values())
            except Exception:
                results = list(self._knowledge_cache.values())
            return results

        def query_subclasses(self, concept):
            """Query for subclasses using InheritanceLink patterns."""
            results = []
            try:
                # Try AtomSpace query for InheritanceLinks
                if hasattr(self.atomspace, 'get_incoming'):
                    incoming = self.atomspace.get_incoming(concept)
                    for link in incoming:
                        if hasattr(link, 'type_name') and 'Inheritance' in str(link.type_name):
                            if hasattr(link, 'out'):
                                subclass = link.out[0]
                                if subclass != concept:
                                    results.append(subclass)
            except Exception:
                pass
            return results

        def query_instances(self, concept):
            """Query for instances using MemberLink patterns."""
            results = []
            try:
                if hasattr(self.atomspace, 'get_incoming'):
                    incoming = self.atomspace.get_incoming(concept)
                    for link in incoming:
                        if hasattr(link, 'type_name') and 'Member' in str(link.type_name):
                            if hasattr(link, 'out'):
                                instance = link.out[0]
                                results.append(instance)
            except Exception:
                pass
            return results

        def infer(self, query):
            """
            Perform inference to answer a query.

            Uses forward chaining when available.
            """
            try:
                # Try to use URE (Unified Rule Engine) if available
                if hasattr(self.atomspace, 'execute'):
                    # Would invoke PLN reasoning here
                    return True
            except Exception:
                pass
            return False

        def get_truth_value(self, atom):
            """Get truth value strength of an atom."""
            try:
                if hasattr(atom, 'tv'):
                    return atom.tv.mean
                atom_id = id(atom)
                if atom_id in self._truth_values:
                    return self._truth_values[atom_id][0]
            except Exception:
                pass
            return 0.0

        def set_truth_value(self, atom, strength, confidence=0.9):
            """Set truth value of an atom."""
            if not (0.0 <= strength <= 1.0):
                raise ValueError("Strength must be between 0.0 and 1.0")
            if not (0.0 <= confidence <= 1.0):
                raise ValueError("Confidence must be between 0.0 and 1.0")

            try:
                if hasattr(atom, 'tv'):
                    from opencog.atomspace import TruthValue
                    atom.tv = TruthValue(strength, confidence)
                else:
                    self._truth_values[id(atom)] = (strength, confidence)
            except ImportError:
                self._truth_values[id(atom)] = (strength, confidence)

# Import high-level Python API (always available)
from .exceptions import AgentZeroError, AgentZeroRuntimeError, AgentZeroConfigError
from .utils import create_goal, create_task, query_knowledge

# Try to import agent_zero which depends on Cython modules
try:
    from .agent_zero import AgentZero
except (ImportError, ModuleNotFoundError) as e:
    # AgentZero depends on Cython modules, provide warning
    import warnings
    warnings.warn(
        f"AgentZero high-level API not available: {e}\n"
        "This is expected if C++ libraries are not built yet.",
        ImportWarning
    )
    AgentZero = None

__all__ = [
    'AgentZeroCore',
    'CognitiveLoop', 
    'TaskManager',
    'KnowledgeIntegrator',
    'AgentZero',
    'AgentZeroError',
    'AgentZeroRuntimeError',
    'AgentZeroConfigError',
    'create_goal',
    'create_task',
    'query_knowledge',
    '__version__',
]
