# language-learning/src/grammar_learner/skl_clustering.py               # 190425
import numpy as np
from sklearn.cluster import AgglomerativeClustering, KMeans, MeanShift, \
    estimate_bandwidth
# from sklearn import metrics, pairwise_distances
from sklearn.metrics import silhouette_score, calinski_harabaz_score
from sklearn.neighbors import kneighbors_graph
# davies_bouldin_score -- next scikit-learn release?
# https://github.com/scikit-learn/scikit-learn/issues/11303
from .utl import kwa
from .clustering import cluster_id


def ile_clustering(cd, n_clusters=10, similarity_threshold=0.8, **kwargs):
    """
    ILE (Iterative Link Expansion) clustering algorithm.

    Groups words based on iterative expansion of similarity neighborhoods.
    This is particularly suited for grammar learning where word classes
    form overlapping similarity clusters.

    Args:
        cd: ndarray(words*disjuncts) - word-disjunct co-occurrence matrix
        n_clusters: target number of clusters (approximate)
        similarity_threshold: minimum cosine similarity to join clusters

    Returns:
        labels: cluster assignments for each word
        metrics: clustering quality metrics
        centroids: cluster centroids
    """
    n_words = cd.shape[0]

    if n_words == 0:
        return np.array([]), {'clustering': 'ile', 'n_clusters': 0}, np.array([])

    # Normalize vectors for cosine similarity
    norms = np.linalg.norm(cd, axis=1, keepdims=True)
    norms[norms == 0] = 1  # Avoid division by zero
    cd_normalized = cd / norms

    # Compute similarity matrix
    similarity_matrix = cd_normalized @ cd_normalized.T

    # Initialize each word in its own cluster
    labels = np.arange(n_words)
    cluster_members = {i: {i} for i in range(n_words)}

    # Iterative merging based on similarity
    changed = True
    max_iterations = n_words
    iteration = 0

    while changed and len(set(labels)) > n_clusters and iteration < max_iterations:
        changed = False
        iteration += 1

        # Find most similar pair of clusters
        best_sim = -1
        best_pair = None

        unique_labels = list(set(labels))
        for i, label_i in enumerate(unique_labels):
            for label_j in unique_labels[i+1:]:
                # Compute average similarity between clusters
                members_i = cluster_members[label_i]
                members_j = cluster_members[label_j]

                total_sim = 0
                count = 0
                for m_i in members_i:
                    for m_j in members_j:
                        total_sim += similarity_matrix[m_i, m_j]
                        count += 1

                avg_sim = total_sim / count if count > 0 else 0

                if avg_sim > best_sim and avg_sim >= similarity_threshold:
                    best_sim = avg_sim
                    best_pair = (label_i, label_j)

        # Merge best pair if found
        if best_pair is not None:
            label_i, label_j = best_pair
            # Merge j into i
            for idx in cluster_members[label_j]:
                labels[idx] = label_i
                cluster_members[label_i].add(idx)
            del cluster_members[label_j]
            changed = True

        # Reduce threshold slightly if no progress
        if not changed and similarity_threshold > 0.1:
            similarity_threshold *= 0.95
            changed = True

    # Renumber labels to be consecutive
    unique_labels = sorted(set(labels))
    label_map = {old: new for new, old in enumerate(unique_labels)}
    labels = np.array([label_map[l] for l in labels])

    # Calculate centroids
    n_final_clusters = len(unique_labels)
    centroids = np.zeros((n_final_clusters, cd.shape[1]))
    for i in range(n_final_clusters):
        mask = labels == i
        if np.sum(mask) > 0:
            centroids[i] = cd[mask].mean(axis=0)

    metrics = {
        'clustering': ('ile', similarity_threshold),
        'n_clusters': n_final_clusters,
        'iterations': iteration
    }

    return labels, metrics, centroids


def random_clustering(cd, n_clusters=10, seed=None, **kwargs):
    """
    Random clustering algorithm for baseline comparison.

    Randomly assigns words to clusters with approximately equal sizes.
    Useful as a baseline for evaluating other clustering algorithms.

    Args:
        cd: ndarray(words*disjuncts) - word-disjunct co-occurrence matrix
        n_clusters: number of clusters to create
        seed: random seed for reproducibility

    Returns:
        labels: cluster assignments for each word
        metrics: clustering metrics
        centroids: cluster centroids
    """
    n_words = cd.shape[0]

    if n_words == 0:
        return np.array([]), {'clustering': 'random', 'n_clusters': 0}, np.array([])

    nc = min(n_clusters, n_words)

    # Set random seed if provided
    rng = np.random.RandomState(seed)

    # Randomly assign labels
    labels = rng.randint(0, nc, size=n_words)

    # Ensure all clusters have at least one member (if possible)
    unique_assigned = set(labels)
    if len(unique_assigned) < nc and n_words >= nc:
        # Assign at least one word to each cluster
        for cluster_id in range(nc):
            if cluster_id not in unique_assigned:
                # Find a word to reassign
                idx = rng.randint(0, n_words)
                labels[idx] = cluster_id

    # Calculate centroids
    centroids = np.zeros((nc, cd.shape[1]))
    for i in range(nc):
        mask = labels == i
        if np.sum(mask) > 0:
            centroids[i] = cd[mask].mean(axis=0)

    metrics = {
        'clustering': ('random', seed),
        'n_clusters': nc
    }

    return labels, metrics, centroids


def skl_clustering(cd, n_clusters=10, **kwargs):
    # cd: ndarray(words*disjuncts)
    nc = min(n_clusters, cd.shape[0])                           # 190425
    clustering = kwa(('agglomerative', 'ward'), 'clustering', **kwargs)
    if type(clustering) is str:
        if clustering == 'agglomerative':
            clustering = ('agglomerative', 'ward')
        elif clustering == 'kmeans':
            clustering = ('kmeans', 'k-means++', 10)
        elif clustering in ['mean_shift', 'mean shift', 'meanshift']:
            clustering = ('mean_shift', 2)  # Note: 'auto' bandwidth not yet implemented
        elif clustering == 'group':
            # Use ILE clustering
            return ile_clustering(cd, nc, **kwargs)
        elif clustering == 'random':
            # Use random clustering
            seed = kwargs.get('random_seed', None)
            return random_clustering(cd, nc, seed=seed, **kwargs)
        else:
            clustering = ('agglomerative', 'ward')

    clustering_metric = kwa(('silhouette', 'euclidean'),
                            'clustering_metric', **kwargs)
    labels = np.asarray([[]])
    metrics = {'clustering': clustering}
    centroids = np.asarray([[]])

    try:  # if True:  #
        if clustering[0] == 'agglomerative':
            linkage = 'ward'
            affinity = 'euclidean'
            connectivity = None
            compute_full_tree = 'auto'
            if clustering[1] in ['average', 'complete', 'single']:
                linkage = clustering[1]
            if len(clustering) > 2:
                if clustering[2] in ['euclidean', 'cosine', 'manhattan']:
                    affinity = clustering[2]
            if len(clustering) > 3:  # connectivity
                if type(clustering[3]) is int and clustering[3] > 0:
                    neighbors = clustering[3]
                    # TODO: int / dict 
                    connectivity = kneighbors_graph(cd, neighbors,
                                                    include_self=False)
            if len(clustering) > 4:  # compute_full_tree
                if clustering[4] is bool:
                    compute_full_tree = clustering[4]

            model = AgglomerativeClustering(n_clusters=nc,
                                            linkage=linkage, affinity=affinity,
                                            connectivity=connectivity,
                                            compute_full_tree=compute_full_tree)
            model.fit(cd)
            labels = model.labels_

            # Calculate centroids for agglomerative clustering
            centroids = np.asarray([cd[labels == i].mean(axis=0) for i in range(max(labels) + 1)])

        elif clustering[0] in ['k-means', 'kmeans']:
            if clustering[1] in ['k-means++']:  # 'random' - fails?
                init = clustering[1]
            else:
                init = 'k-means++'
            if len(clustering) > 2 and type(clustering[2]) is int:
                n_init = clustering[2]
            else:
                n_init = 10
            model = KMeans(init=init, n_clusters=nc, n_init=n_init)
            model.fit(cd)
            labels = model.labels_
            metrics['inertia'] = model.inertia_
            centroids = np.asarray(model.cluster_centers_[:(max(labels) + 1)])

        elif clustering[0] in ['mean shift', 'mean_shift']:
            if len(clustering) < 2:
                bandwidth = None
            if type(clustering[1]) is int:
                bandwidth = clustering[1]
            else:
                bandwidth = None  # Note: 'auto' bandwidth estimation not yet implemented
                bandwidth = 'auto'

            model = MeanShift(bandwidth=bandwidth)
            model.fit(cd)
            labels = model.labels_

            centroids = np.asarray(model.cluster_centers_[:(max(labels) + 1)])

        else:  # Default to agglomerative clustering
            model = AgglomerativeClustering(linkage='ward', n_clusters=nc)
            model.fit(cd)
            labels = model.labels_

        try:
            # Silhouette score requires at least 2 clusters and samples
            if len(set(labels)) >= 2 and len(labels) >= 2:
                metrics['silhouette_index'] = float(
                    silhouette_score(cd, labels, metric=clustering_metric[1]))
            else:
                metrics['silhouette_index'] = 0.0
        except (ValueError, MemoryError) as e:
            # ValueError: n_labels must be >= 2 and <= n_samples - 1
            # MemoryError: matrix too large
            metrics['silhouette_index'] = 0.0
            metrics['silhouette_error'] = str(e)
        try:
            # Calinski-Harabasz also requires at least 2 clusters
            if len(set(labels)) >= 2 and len(labels) >= 2:
                metrics['variance_ratio'] = float(
                    calinski_harabaz_score(cd, labels))
            else:
                metrics['variance_ratio'] = 0.0
        except (ValueError, MemoryError) as e:
            # Same constraints as silhouette
            metrics['variance_ratio'] = 0.0
            metrics['variance_ratio_error'] = str(e)
        # try:
        #   metrics['davies_bouldin_score'] = float(
        #       davies_bouldin_score(cd, labels))
        # except: metrics['davies_bouldin_score'] = 0.0

        return labels, metrics, centroids
    except Exception:  # Handle clustering errors
        print('except: skl_clustering error')
        return np.asarray(range(cd.shape[0])), \
               {'clustering': 'skl_clustering error'}, []


def optimal_clusters(cd, **kwargs):
    # cluster_range = kwa((2,48,1), 'cluster_range')
    algo = kwa('agglomerative', 'clustering', **kwargs)
    criteria = kwa('silhouette', 'cluster_criteria', **kwargs)
    level = kwa(1.0, 'cluster_level', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)
    crange = kwa(10, 'cluster_range', **kwargs)                         # 90206

    if type(algo) is str:
        if algo == 'agglomerative':
            algo = ('agglomerative', 'ward')
        elif algo == 'kmeans':
            algo = ('kmeans', 'k-means++', 10)
        elif algo in ['mean_shift', 'mean shift', 'meanshift']:
            algo = ('mean_shift', 2)  # ('mean_shift', 'auto')?
        elif algo == 'group':
            # Use ILE clustering
            return ile_clustering(cd, crange if isinstance(crange, int) else 10, **kwargs)
        elif algo == 'random':
            # Use random clustering
            seed = kwargs.get('random_seed', None)
            return random_clustering(cd, crange if isinstance(crange, int) else 10, seed=seed, **kwargs)
        else:
            algo = ('agglomerative', 'ward')

    if type(crange) is int or algo[0] in ['mean_shift', 'mean shift', 'meanshift']:
        labels, metrics, centroids = skl_clustering(cd, crange, **kwargs)

    if type(crange) in [tuple, list]:
        if len(crange) == 1:
            if type(crange[0]) is int:
                labels, metrics, centroids = skl_clustering(cd, crange[0],
                                                            **kwargs)
        elif len(crange) == 2:
            if type(crange[0]) is int and type(crange[1]) is int:
                labels, metrics, centroids = skl_clustering(cd, crange[0],
                                                            **kwargs)
                for n in range(crange[1] - 1):
                    l, m, c = skl_clustering(cd, crange[0], **kwargs)
                    if m['silhouette_index'] > metrics['silhouette_index']:
                        labels, metrics, centroids = l, m, c
        elif len(crange) == 3:  # TODO: replace with SGD?
            n_min = min(crange[0], crange[1])
            n_max = max(crange[0], crange[1])
            labels, metrics, centroids = \
                skl_clustering(cd, int((n_min + n_max) / 2), **kwargs)
            for n_clusters in range(n_min, n_max + 1):
                for n in range(kwargs['cluster_range'][2]):
                    l, m, c = skl_clustering(cd, n_clusters, **kwargs)
                    if m['silhouette_index'] > metrics['silhouette_index']:
                        labels, metrics, centroids = l, m, c
        elif len(crange) == 4:
            n_min = min(crange[0], crange[1])
            n_max = max(crange[0], crange[1])
            labels, metrics, centroids = \
                skl_clustering(cd, int((n_min + n_max) / 2), **kwargs)
            for n_clusters in range(n_min, n_max + 1, crange[2]):
                for n in range(kwargs['cluster_range'][3]):
                    l, m, c = skl_clustering(cd, n_clusters, **kwargs)
                    if 'silhouette_index' in m \
                            and 'silhouette_index' in metrics:
                        if m['silhouette_index'] > metrics['silhouette_index']:
                            labels, metrics, centroids = l, m, c
        else:
            labels, metrics, centroids = skl_clustering(cd, 10, **kwargs)

    return labels, metrics, centroids


# Notes:
# 181107 k-means, mean_shift
# 181203 cleanup
# 190118 cleanup: remove debug printing
# 190425 fix n_clusters > n_words case
# 250104 implemented ILE and random clustering algorithms
#        fixed bare exception handlers with proper exception types
