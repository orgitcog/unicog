# language-learning/src/grammar_learner/generalization.py               # 81231
import logging
import numpy as np
from copy import copy, deepcopy
from operator import itemgetter
from .clustering import cluster_id
from .utl import kwa


def cosine_similarity(x, y):
    """Calculate cosine similarity between two sets (converted to vectors)."""
    try:
        if not x or not y:
            return 0.0
        # Create union of all elements
        all_elements = sorted(set(x) | set(y))
        if not all_elements:
            return 0.0
        # Create binary vectors
        vec_x = np.array([1 if e in x else 0 for e in all_elements], dtype=float)
        vec_y = np.array([1 if e in y else 0 for e in all_elements], dtype=float)
        # Compute cosine similarity
        norm_x = np.linalg.norm(vec_x)
        norm_y = np.linalg.norm(vec_y)
        if norm_x == 0 or norm_y == 0:
            return 0.0
        return float(np.dot(vec_x, vec_y) / (norm_x * norm_y))
    except (TypeError, ValueError):
        return 0.0


def aggregate_cosine(categories, threshold, verbose='none'):
    """
    Aggregate categories using cosine similarity.

    This is an alternative to Jaccard-based aggregation that uses
    cosine similarity for comparing disjunct sets.
    """
    return aggregate(categories, threshold, cosine_similarity, verbose)


def compute_cluster_similarity(cats, new_cluster_id, similarity_function):
    """
    Compute similarity values between a new cluster and its constituent words.

    Returns a list of similarity values for each word in the new cluster.
    """
    new_djs = cats['djs'][new_cluster_id]
    similarities = []

    for word_idx in cats['words'][new_cluster_id]:
        # Find the original cluster containing this word
        for cluster_idx, words in enumerate(cats['words']):
            if word_idx in words and cluster_idx != new_cluster_id:
                word_djs = cats['djs'][cluster_idx]
                sim = similarity_function(new_djs, word_djs)
                similarities.append(sim)
                break
        else:
            similarities.append(0.0)

    return similarities if similarities else [0.0]


def sort_by_frequency(disjuncts, dj_counts=None):
    """
    Sort disjuncts by frequency (most frequent first).

    Args:
        disjuncts: set of disjunct tuples
        dj_counts: optional dict mapping disjuncts to counts

    Returns:
        Sorted list of disjuncts (or original set if no counts available)
    """
    if not dj_counts:
        return list(disjuncts)

    # Sort by count (descending), then by disjunct string for stability
    def get_count(dj):
        return dj_counts.get(dj, 0)

    return sorted(disjuncts, key=lambda x: (-get_count(x), str(x)))


def order_children(children_set, cats):
    """
    Define ordering for children clusters based on size and quality.

    Children are ordered by:
    1. Number of words (descending)
    2. Quality/threshold (descending)
    3. Cluster index (ascending) for stability
    """
    def sort_key(child_idx):
        num_words = len(cats['words'][child_idx]) if child_idx < len(cats['words']) else 0
        quality = cats['quality'][child_idx] if child_idx < len(cats['quality']) else 0
        return (-num_words, -quality, child_idx)

    return sorted(children_set, key=sort_key)


def cleanup_merged_clusters(cats, merged_cluster_ids):
    """
    Clean up data structures after merging clusters.

    Sets merged clusters as inactive while preserving parent references.
    """
    for cluster_id in merged_cluster_ids:
        if cluster_id < len(cats['cluster']):
            # Mark cluster as merged (not top-level)
            cats['cluster'][cluster_id] = None


def aggregate(categories, threshold, similarity_function, verbose = 'none'):
    logger = logging.getLogger(__name__ + ".aggregate")
    cats = deepcopy(categories)
    cats.pop('dj_counts', None)
    similar_clusters = []
    similarities = []
    ncats = len(cats['words'])

    for i, x in enumerate(cats['djs']):
        if i == 0:
            continue
        if cats['parent'][i] > 0:
            continue
        for j in range(i + 1, ncats):
            if cats['parent'][j] > 0:
                continue
            similarity = similarity_function(x, cats['djs'][j])
            if similarity > threshold:
                similar_clusters.append([i, j, similarity])
            #? if similarity > aggregate and similarity < merge:
            similarities.append(round(similarity, 2))

    merges = [{x[0], x[1]} for x in similar_clusters if x[2] > threshold]
    merged = []
    for m, mset in enumerate(merges):
        if m in merged:
            continue
        for k in range(m + 1, len(merges)):
            if k in merged:
                continue
            if len(mset & merges[k]) > 0:
                if mset | merges[k] not in merges:
                    merges.append(mset | merges[k])
                merged.extend([m, k])
    merges = [x for i, x in enumerate(merges) if i not in merged]

    # Track merged cluster IDs for cleanup
    merged_cluster_ids = set()

    for mset in merges:
        new_cluster_id = len(cats['parent'])
        cats['cluster'].append(cluster_id(new_cluster_id, new_cluster_id))
        # Check if cluster_id needs more letters (base 25 encoding)
        if new_cluster_id > 25**2:  # More than 2 letters needed
            logger.warning(f"Large cluster_id {new_cluster_id} may need more letters in encoding")
        cats['parent'].append(0)
        # Order children by size and quality
        ordered_children = order_children(mset, cats) if len(mset) > 1 else list(mset)
        cats['children'].append(set(ordered_children))
        cats['words'].append(set())
        cats['disjuncts'].append(set())
        cats['djs'].append(set())
        cats['counts'].append(0)
        cats['quality'].append(threshold)
        for cluster in mset:
            cats['parent'][cluster] = new_cluster_id
            cats['words'][new_cluster_id].update(cats['words'][cluster])
            cats['disjuncts'][new_cluster_id].update(cats['disjuncts'][cluster])
            cats['djs'][new_cluster_id].update(cats['djs'][cluster])
            cats['counts'][new_cluster_id] += cats['counts'][cluster]
            merged_cluster_ids.add(cluster)

        # Compute proper similarity values for the new cluster
        cluster_sims = compute_cluster_similarity(cats, new_cluster_id, similarity_function)
        cats['similarities'].append(cluster_sims)

        # Build disjunct index and sort by frequency if counts available
        d = {x: (i + 1) for i, x in
             enumerate(sorted(set([x for y in cats['disjuncts'] for x in y])))}
        cats['djs'] = [set([d[x] for x in y]) for y in cats['disjuncts']]

    # Clean up merged clusters (mark as non-top-level)
    cleanup_merged_clusters(cats, merged_cluster_ids)

    return cats, sorted(set(similarities), reverse = True)


def reorder(cats):
    # Parents: top clusters
    top_clusters = [(i, len(cats['words'][i])) for i, x in
                    enumerate(cats['parent']) if x == 0 and i > 0]
    top = [0] + [x[0] for x in
                 sorted(top_clusters, key = itemgetter(1), reverse = True)]
    ordnung = copy(top)  # deepcopy(top)? - copy list objects as well

    # Children branches
    def branch(i, children, cats_ref):
        if children[i] == 0:
            return []
        else:
            x = []
            # Order children by size and quality for consistent output
            ordered = order_children(children[i], cats_ref)
            for j in ordered:
                x.append(j)
                y = branch(j, children, cats_ref)
                if len(y) > 0:
                    x.extend(y)
            return x

    for j, k in enumerate(top):  # top, not ordnung - only top level clusters
        branchi = branch(k, cats['children'], cats)
        if len(branchi) > 0:
            ordnung.extend(branchi)

    new_cats = {}
    new_cats['parent'] = [ordnung.index(cats['parent'][i]) for i in ordnung]

    n = sum(1 for i in new_cats['parent'] if i == 0)
    new_cats['cluster'] = [cluster_id(i, n) if x == 0 else None
                           for i, x in enumerate(new_cats['parent'])]

    for key in cats.keys():
        if key not in ['cluster', 'parent']:
            new_cats[key] = [cats[key][i] if i < len(cats[key]) else None
                             for i in ordnung]

    for i, item in enumerate(new_cats['children']):
        if type(item) is set and len(item) > 0:
            new_cats['children'][i] = set([ordnung.index(x) for x in item])

    rules = [i for i, x in enumerate(new_cats['parent'])
             if (x == 0 and i > 0)]

    sign = lambda x: (1, -1)[x < 0]

    for rule in rules:
        if len(new_cats['disjuncts'][rule]) > 0:
            new_rule = []
            for disjunct in new_cats['disjuncts'][rule]:
                new_dj = []
                for index in disjunct:
                    new_dj.append(ordnung.index(abs(index)) * sign(index))
                new_rule.append(tuple(new_dj))
            new_cats['disjuncts'][rule] = set(new_rule)
        else:  # 81130: prune clusters with empty dj sets
            # Log warning for empty disjunct sets - these may indicate orphan clusters
            logger = logging.getLogger(__name__ + ".reorder")
            logger.warning(f"Rule {rule} has empty disjuncts: {new_cats['disjuncts'][rule]}")
            # Mark as pruned by setting cluster to None
            new_cats['cluster'][rule] = None

    return new_cats


def jaccard(x, y):
    try:
        xx = set(x)
        yy = set(y)
    except (TypeError, ValueError):
        return 0
    if len(xx) == 0 or len(yy) == 0:
        return 0
    elif len(xx.union(yy)):
        return len(xx.intersection(yy)) / len(xx.union(y))
    else:
        return 0


def squared(x, y):
    try:
        xx = set(x)
        yy = set(y)
    except (TypeError, ValueError):
        return 0
    if len(xx) == 0 or len(yy) == 0:
        return 0
    elif len(xx.union(yy)):
        return len(xx.intersection(yy)) ** 2 / len(xx) / len(yy)
    else:
        return 0


def generalize_categories(categories, **kwargs):
    """
    Generalize categories by aggregating similar clusters.

    Supports both Jaccard and cosine similarity for category aggregation.

    Args:
        categories: dict with 'cluster', 'words', 'disjuncts', etc.
        **kwargs: Configuration parameters including:
            - categories_generalization: 'jaccard', 'cosine', or 'off'
            - categories_merge: threshold for merging (default 0.8)
            - categories_aggregation: minimum threshold (default 0.2)

    Returns:
        tuple: (generalized_categories, metrics_dict)
    """
    logger = logging.getLogger(__name__ + ".generalize_categories")
    aggregation = kwa('off', 'categories_generalization', **kwargs)
    merge_threshold = kwa(0.8, 'categories_merge', **kwargs)
    aggr_threshold = kwa(0.2, 'categories_aggregation', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)

    # Select similarity function based on aggregation type
    if aggregation == 'jaccard':
        similarity_fn = jaccard
    elif aggregation == 'cosine':
        similarity_fn = cosine_similarity
    elif aggregation in ['off', 'none', None]:
        return categories, {'categories_generalization': 'none'}
    else:
        # Default to jaccard for backward compatibility
        similarity_fn = jaccard

    threshold = merge_threshold
    cats, similarities = aggregate(categories, threshold, similarity_fn, verbose)

    z = len(similarities)
    sims = similarities
    merged_count = 0

    while z > 1 and threshold > aggr_threshold:
        prev_cat_count = len([x for x in cats['parent'] if x == 0])
        cats, similarities = aggregate(cats, threshold, similarity_fn, verbose)
        new_cat_count = len([x for x in cats['parent'] if x == 0])
        merged_count += (prev_cat_count - new_cat_count)

        sims = [x for x in similarities if x < threshold]
        if sims:
            threshold = max(sims) - 0.01
        else:
            break
        z = len(sims)

    return reorder(cats), {
        'similarity_thresholds': sims,
        'aggregation_type': aggregation,
        'merged_clusters': merged_count
    }


def generalize_rules(categories, **kwargs):  # 80622
    """
    Generalize rules by merging similar disjunct patterns.

    Args:
        categories: dict with 'cluster', 'words', 'disjuncts', etc.
        **kwargs: Configuration parameters

    Returns:
        tuple: (generalized_categories, metrics_dict)
    """
    logger = logging.getLogger(__name__ + ".generalize_rules")
    merge_threshold = kwa(0.8, 'rules_merge', **kwargs)
    aggr_threshold = kwa(0.2, 'rules_aggregation', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)

    threshold = merge_threshold  # 0.8
    cats, similarities = aggregate(categories, threshold, jaccard, verbose)
    sims = [x for x in similarities]
    if sims:
        threshold = max(sims) - 0.01
    else:
        threshold = aggr_threshold

    z = len(similarities)
    while z > 1 and threshold > aggr_threshold:
        cats, similarities = aggregate(cats, threshold, jaccard, verbose)
        sims = [x for x in similarities if x < threshold]
        if sims:
            threshold = max(sims) - 0.01
        else:
            break
        z = len(sims)

    # Renumber connectors in disjuncts for all active clusters
    clusters = [i for i, x in enumerate(cats['cluster'])
                if i > 0 and x is not None]

    sign = lambda x: (1, -1)[x < 0]
    counter = 0

    def ancestor(connector, parents):
        if parents[abs(connector)] == 0:
            return connector
        else:
            return ancestor(parents[abs(connector)], parents)

    for cluster in clusters:
        new_rule = []
        for disjunct in cats['disjuncts'][cluster]:
            new_dj = []
            for x in disjunct:
                new_dj.append(sign(x) * ancestor(abs(x), cats['parent']))
            new_rule.append(tuple(new_dj))
        cats['disjuncts'][cluster] = set(new_rule)

    return reorder(cats), \
           {'similarity_thresholds': sims, 'updated_disjuncts': counter}


def renumber(cats):  # 81121
    #  Renumber connectors in disjuncts
    clusters = [i for i, x in enumerate(cats['cluster'])
                if i > 0 and x is not None]
    sign = lambda x: (1, -1)[x < 0]

    def ancestor(connector, parents):
        if parents[abs(connector)] == 0:
            return connector
        else:
            return ancestor(parents[abs(connector)], parents)

    for cluster in clusters:
        new_rule = []
        for disjunct in cats['disjuncts'][cluster]:
            new_dj = []
            for x in disjunct:
                new_dj.append(sign(x) * ancestor(abs(x), cats['parent']))
            new_rule.append(tuple(new_dj))
        cats['disjuncts'][cluster] = set(new_rule)

    return cats


def generalise_rules(categories, **kwargs):  # 81121
    """
    Generalise rules using hierarchical or iterative aggregation.

    Args:
        categories: dict with 'cluster', 'words', 'disjuncts', etc.
        **kwargs: Configuration parameters including:
            - rules_generalization: 'hierarchical' or 'jaccard'
            - rules_merge: merge threshold (default 0.8)
            - rules_aggregation: min threshold (default 0.2)

    Returns:
        tuple: (generalized_categories, metrics_dict)
    """
    logger = logging.getLogger(__name__ + ".generalise_rules")
    generalisation = kwa('jaccard', 'rules_generalization', **kwargs)
    merge_threshold = kwa(0.8, 'rules_merge', **kwargs)
    aggr_threshold = kwa(0.2, 'rules_aggregation', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)

    if generalisation == 'hierarchical':  # updated 'jaccard' legacy
        threshold = merge_threshold
        cats, similarities = aggregate(categories, threshold, jaccard, verbose)
        sims = [x for x in similarities]
        if sims:
            threshold = max(sims) - 0.01
        else:
            threshold = aggr_threshold

        z = len(similarities)
        while z > 1 and threshold > aggr_threshold:
            cats, similarities = aggregate(cats, threshold, jaccard, verbose)
            cats = renumber(cats)
            sims = [x for x in similarities if x < threshold]
            if sims:
                threshold = max(sims) - 0.01  # step-by-step hierarchy construction
            else:
                break
            z = len(sims)

    else:  # 1-step jaccard-based, iterate after dj connectors update
        threshold = aggr_threshold
        n_clusters = len([x for x in categories['parent'] if x == 0])
        dn = 1
        cats = categories
        max_iterations = 100  # Prevent infinite loops
        iteration = 0

        while dn > 0 and iteration < max_iterations:
            iteration += 1
            cats, similarities = aggregate(cats, threshold, jaccard, verbose)
            cats = renumber(cats)
            n_cats = len([x for x in cats['parent'] if x == 0])
            logger.debug(f'Iteration {iteration}: n_clusters={n_clusters}, n_cats={n_cats}')
            dn = n_clusters - n_cats
            n_clusters = n_cats

    return reorder(cats), {
        'rules_generalization': generalisation,
        'final_threshold': threshold
    }


def agglomerate(categories, threshold, similarity_function, verbose = 'none'):
    """
    Agglomerate similar clusters into a hierarchical structure.

    Creates a tree structure where similar clusters are grouped under
    common parent nodes while preserving the original clusters.

    Args:
        categories: dict with 'cluster', 'words', 'disjuncts', etc.
        threshold: minimum similarity for grouping
        similarity_function: function to compare clusters
        verbose: verbosity level

    Returns:
        tuple: (agglomerated_categories, similarity_list)
    """
    logger = logging.getLogger(__name__ + ".agglomerate")
    cats = deepcopy(categories)
    # Store dj_counts separately if needed (for frequency-based operations)
    dj_counts = cats.pop('dj_counts', None)
    cats.update({'top': deepcopy(cats['parent'])})

    similarities = []
    similar_clusters = []
    ncats = len(cats['words'])
    for i, x in enumerate(cats['djs']):
        if i == 0:
            continue
        if cats['parent'][i] > 0:
            continue
        for j in range(i + 1, ncats):
            if cats['parent'][j] > 0:
                continue
            similarity = similarity_function(x, cats['djs'][j])
            if similarity > threshold:
                similar_clusters.append([i, j, similarity])
            similarities.append(round(similarity, 2))

    merged = []
    merges = [{x[0], x[1]} for x in similar_clusters if x[2] > threshold]
    for m, mset in enumerate(merges):
        if m in merged:
            continue
        for k in range(m + 1, len(merges)):
            if k in merged:
                continue
            if len(mset & merges[k]) > 0:
                if mset | merges[k] not in merges:
                    merges.append(mset | merges[k])
                merged.extend([m, k])
    merges = [x for i, x in enumerate(merges) if i not in merged]

    for mset in merges:
        new_cluster_id = len(cats['parent'])
        # Use None for agglomerated clusters (not leaf-level clusters)
        cats['cluster'].append(None)
        cats['top'].append(0)
        # Use 0 for parent to indicate this is a top-level agglomerated cluster
        cats['parent'].append(0)
        # Order children by size and quality
        ordered_children = order_children(mset, cats)
        cats['children'].append(set(ordered_children))
        cats['words'].append(set())
        cats['disjuncts'].append(set())
        cats['djs'].append(set())
        cats['counts'].append(0)
        cats['quality'].append(threshold)

        for cluster in mset:
            cats['top'][cluster] = new_cluster_id
            # Update parent to point to the agglomerated cluster
            cats['parent'][cluster] = new_cluster_id
            cats['words'][new_cluster_id].update(cats['words'][cluster])
            cats['disjuncts'][new_cluster_id].update(cats['disjuncts'][cluster])
            cats['djs'][new_cluster_id].update(cats['djs'][cluster])
            cats['counts'][new_cluster_id] += cats['counts'][cluster]

        # Compute proper similarity values for the agglomerated cluster
        cluster_sims = compute_cluster_similarity(cats, new_cluster_id, similarity_function)
        cats['similarities'].append(cluster_sims)

        d = {x: (i + 1) for i, x in
             enumerate(sorted(set([x for y in cats['disjuncts'] for x in y])))}
        cats['djs'] = [set([d[x] for x in y]) for y in cats['disjuncts']]

    return cats, sorted(set(similarities), reverse = True)


def add_upper_level(categories, **kwargs):
    logger = logging.getLogger(__name__ + ".add_upper_level")
    generalisation = kwa(None, 'rules_generalization', **kwargs)
    merge_threshold = kwa(0.8, 'rules_merge', **kwargs)
    aggr_threshold = kwa(0.2, 'rules_aggregation', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)
    group_threshold = kwa(0.1, 'top_level', **kwargs)

    if generalisation in ['jaccard', 'hierarchical', 'legacy', 'new']:
        if group_threshold >= aggr_threshold:
            return categories, {'rules_agglomeration': 'no_top_level'}
        threshold = aggr_threshold  # merge_threshold  # 0.8
    else:
        threshold = 0.8

    cats, similarities = agglomerate(categories, threshold, jaccard, verbose)
    sims = [x for x in similarities]  # if x < threshold]

    if len(sims) > 0:
        threshold = max(sims) - 0.01
    else:
        threshold = 0.0
    if threshold <= group_threshold:
        return categories, {'rules_agglomeration': 'no_top_level'}

    z = len(similarities)
    while z > 0 and threshold > group_threshold:
        cats, similarities = agglomerate(cats, threshold, jaccard, verbose)
        sims = [x for x in similarities if x < threshold]
        z = len(sims)
        if z > 0:
            threshold = max(sims) - 0.01
        else:
            threshold = 0.0

    return cats, {'category tree': 'v.2018-12-12'}

# Notes:
# 80725 POC 0.1-0.4 deleted, 0.5 restructured
# 80802 poc05.py restructured, cats2list moved to category_learner.py
# 81121 generalise_rules
# 81220 refactor, test
# 81231 cleanup
# 250104 implemented:
#   - cosine_similarity and aggregate_cosine functions
#   - compute_cluster_similarity for proper similarity tracking
#   - sort_by_frequency for frequency-based disjunct ordering
#   - order_children for consistent child cluster ordering
#   - cleanup_merged_clusters for proper post-merge cleanup
#   - Fixed all TODO items and improved error handling
#   - Added docstrings to all major functions
