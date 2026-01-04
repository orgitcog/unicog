"""
Agent-Zero Utility Functions

Helper functions for working with Agent-Zero.
"""

try:
    from opencog.atomspace import ConceptNode, PredicateNode, ListLink, ExecutionLink
except ImportError:
    # Stubs when atomspace not available
    class ConceptNode:
        def __init__(self, name):
            self.name = name
    class PredicateNode:
        def __init__(self, name):
            self.name = name
    class ListLink:
        pass
    class ExecutionLink:
        pass


def create_goal(name, description=None, priority=1.0, atomspace=None):
    """
    Create a goal atom with metadata.

    Args:
        name (str): Goal name
        description (str): Optional description
        priority (float): Priority level (0.0 to 1.0)
        atomspace: Optional AtomSpace to add the goal to

    Returns:
        ConceptNode: The goal atom

    Example:
        >>> goal = create_goal("LearnPython", "Learn Python programming", 0.9)
    """
    goal = ConceptNode(f"goal:{name}")

    if description:
        try:
            # Add description as a StateLink
            from opencog.atomspace import StateLink, ConceptNode as CN
            desc_node = CN(f"description:{name}")
            desc_node.name = description
        except ImportError:
            pass

    if priority != 1.0:
        try:
            from opencog.atomspace import TruthValue
            goal.tv = TruthValue(priority, 0.9)
        except (ImportError, AttributeError):
            pass

    return goal


def create_task(name, goal=None, dependencies=None, atomspace=None):
    """
    Create a task atom linked to a goal.

    Args:
        name (str): Task name
        goal: Optional goal this task contributes to
        dependencies: Optional list of prerequisite tasks
        atomspace: Optional AtomSpace to add the task to

    Returns:
        ConceptNode: The task atom

    Example:
        >>> task = create_task("InstallPython", goal=learn_goal)
    """
    task = ConceptNode(f"task:{name}")

    if goal:
        try:
            from opencog.atomspace import EvaluationLink, PredicateNode as PN, ListLink as LL
            # Create a link showing task contributes to goal
            EvaluationLink(
                PN("contributes_to"),
                LL(task, goal)
            )
        except ImportError:
            pass

    if dependencies:
        try:
            from opencog.atomspace import EvaluationLink, PredicateNode as PN, ListLink as LL
            for dep in dependencies:
                # Create dependency links
                EvaluationLink(
                    PN("depends_on"),
                    LL(task, dep)
                )
        except ImportError:
            pass

    return task


def query_knowledge(atomspace, concept_name, query_type="instances"):
    """
    Query knowledge about a concept.
    
    Args:
        atomspace: The AtomSpace to query
        concept_name (str): Name of the concept
        query_type (str): Type of query - "instances", "subclasses", or "related"
    
    Returns:
        list: Query results
    
    Example:
        >>> results = query_knowledge(atomspace, "Animal", "subclasses")
    """
    from .knowledge_integrator import KnowledgeIntegrator
    
    integrator = KnowledgeIntegrator(atomspace)
    concept = ConceptNode(concept_name)
    
    if query_type == "instances":
        return integrator.query_instances(concept)
    elif query_type == "subclasses":
        return integrator.query_subclasses(concept)
    else:
        return integrator.query_pattern(concept)


def format_handle_list(handles):
    """
    Format a list of handles for display.
    
    Args:
        handles: List of AtomSpace handles
    
    Returns:
        str: Formatted string
    """
    if not handles:
        return "[]"
    
    items = []
    for h in handles[:10]:  # Limit to first 10
        try:
            items.append(str(h))
        except:
            items.append(repr(h))
    
    result = ", ".join(items)
    if len(handles) > 10:
        result += f", ... ({len(handles) - 10} more)"
    
    return f"[{result}]"


def create_action(name, parameters=None, atomspace=None):
    """
    Create an action atom with parameters.

    Args:
        name (str): Action name
        parameters: Optional dict of action parameters
        atomspace: Optional AtomSpace to add the action to

    Returns:
        ConceptNode: The action atom
    """
    action = ConceptNode(f"action:{name}")

    if parameters and isinstance(parameters, dict):
        try:
            from opencog.atomspace import EvaluationLink, PredicateNode as PN, ListLink as LL, NumberNode
            for key, value in parameters.items():
                # Create parameter links
                if isinstance(value, (int, float)):
                    param_node = NumberNode(str(value))
                else:
                    param_node = ConceptNode(str(value))
                EvaluationLink(
                    PN(f"param:{key}"),
                    LL(action, param_node)
                )
        except ImportError:
            pass

    return action


def get_agent_statistics(agent):
    """
    Get comprehensive statistics about an agent.
    
    Args:
        agent: AgentZero instance
    
    Returns:
        dict: Statistics dictionary
    """
    stats = {
        'running': agent.is_running,
        'goals': len(agent.goals),
        'pending_tasks': len(agent.pending_tasks),
        'status': agent.status,
    }
    
    # Add loop statistics if available
    try:
        loop_stats = agent.statistics
        stats.update(loop_stats)
    except (AttributeError, TypeError):
        # Statistics not available or agent not initialized properly
        pass
    
    return stats
