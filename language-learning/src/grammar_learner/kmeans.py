#language-learning/src/grammar_learner/kmeans.py POC.0.5 80725 as-was tmp
import logging
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances, silhouette_score
from statistics import mode
from .utl import round1, round2, round3
# -from kmeans import cluster_words_kmeans   #this module


def cluster_words_kmeans(words_df, n_clusters):
    words_list = words_df['word'].tolist()
    df = words_df.copy()
    del df['word']
    kmeans_model = KMeans(init='k-means++', n_clusters=n_clusters, n_init=10)
    kmeans_model.fit(df)
    labels = kmeans_model.labels_
    inertia  = kmeans_model.inertia_
    centroids = np.asarray(kmeans_model.cluster_centers_[:(max(labels)+1)])
    silhouette = silhouette_score(df, labels, metric ='euclidean')

    cdf = pd.DataFrame(centroids)
    cdf = cdf.applymap(lambda x: x if abs(x) > 1e-12 else 0.)
    cdf.columns = [x+1 if type(x)==int else x for x in cdf.columns]
    cols = cdf.columns.tolist()
    def cluster_word_list(i):
        return [words_list[j] for j,x in enumerate(labels) if x==i]
    cdf['cluster'] = cdf.index
    cdf['cluster_words'] = cdf['cluster'].apply(cluster_word_list)
    #+cdf = cdf.sort_values(by=[1,2,3], ascending=[True,True,True])
    cdf = cdf.sort_values(by=[1,2], ascending=[True,True])
    cdf.index = range(1, len(cdf)+1)
    def cluster_id(row): return 'C' + str(row.name).zfill(2)
    cdf['cluster'] = cdf.apply(cluster_id, axis=1)
    cols = ['cluster', 'cluster_words'] + cols
    cdf = cdf[cols]

    return cdf, silhouette, inertia


def number_of_clusters(vdf, cluster_range, algorithm='kmeans', \
        criteria='silhouette', level=0.9, verbose='none'):

    logger = logging.getLogger(__name__ + ".number_of_clusters")

    if(len(cluster_range) < 2 or cluster_range[2] < 1):
        return cluster_range[0]

    sil_range = pd.DataFrame(columns=['Np','Nc','Silhouette','Inertia'])
    # if verbose == 'debug':
    #     print('clustering/poc.py/number_of_clusters: vdf:\n', \
    #         vdf.applymap(round2).sort_values(by=[1,2], ascending=[True,True]))
    logger.debug('clustering/poc.py/number_of_clusters: vdf:\n{}'.format(
        vdf.applymap(round2).sort_values(by=[1,2], ascending=[True,True])))

    # Check number of clusters <= word vector dimensionality
    max_clusters = min(cluster_range[1], len(vdf), \
                       max([x for x in list(vdf) if isinstance(x,int)]))
    # Handle low-dimensional word spaces
    max_dimension = max([x for x in list(vdf) if isinstance(x,int)])
    if max_dimension == 2:
        # For 2D word spaces, limit to 4 clusters as a reasonable upper bound
        # This prevents over-clustering in low-dimensional spaces
        logger.info('2D word space detected -- limiting to 4 clusters')
        return min(4, len(vdf))  # Ensure we don't exceed number of words

    # if verbose in ['max', 'debug']:
    #     print('number_of_clusters: max_clusters =', max_clusters)
    logger.info('number_of_clusters: max_clusters = {}'.format(max_clusters))

    n_clusters = max_clusters   #80623: cure case max < range.min

    # Multiple attempts to ensure stable cluster count determination
    lst = []
    attempts = 3  # Increased from 1 to improve stability
    for k in range(attempts):
        for i,j in enumerate(range(cluster_range[0], max_clusters, cluster_range[2])):
            cdf, silhouette, inertia = cluster_words_kmeans(vdf, j)
            # if verbose in ['debug']:
            #     print(j, 'clusters ⇒ silhouette =', silhouette)
            logger.debug(f'{j} clusters ⇒ silhouette = {silhouette}')

            sil_range.loc[i] = [j, len(cdf), round(silhouette,4), round(inertia,2)]
            if level > 0.9999:   # 1 - max Silhouette index
                n_clusters = sil_range.loc[sil_range['Silhouette'].idxmax()]['Nc']
            elif level < 0.0001: # 0 - max number pf clusters
                n_clusters = sil_range.loc[sil_range['Nc'].idxmax()]['Nc']
            else:
                thresh = level * sil_range.loc[sil_range['Silhouette'].idxmax()]['Silhouette']
                n_clusters = min(sil_range.loc[sil_range['Silhouette'] > thresh]['Nc'].tolist())
        lst.append(int(n_clusters))

    dct = dict()
    for n in lst:
        if n in dct:
            dct[n] += 1
        else: dct[n] = 1
    n_clusters = int(round(np.mean(lst),0))
    n2 = list(dct.keys())[list(dct.values()).index(max(list(dct.values())))]
    if n2 != n_clusters:
        if len(list(dct.values())) == len(set(list(dct.values()))):
            try:
                n3 = mode(lst)
            except Exception:
                # If mode() fails (e.g., no unique mode), use the mean
                n3 = n_clusters
                logger.debug(f"mode() failed, using mean value: {n3}")
        else: n3 = n_clusters
        n_clusters = int(round((n_clusters + n2 + n3)/3.0, 0))

    # if verbose in ['max', 'debug']:
    #     if len(dct) > 1:
    #         print('Clusters:', sorted(lst), '⇒', n_clusters)
    if len(dct) > 1:
        logger.info(f'Clusters: {sorted(lst)} ⇒ {n_clusters}')
    return int(n_clusters)


#80219 update cluster_kmeans 80209 ⇒ cluster_words_kmeans: DataFrames, in and out
#80617 kmeans_model = KMeans(init='random', n_clusters=n_clusters, n_init=30)   #fails?
#80725 POC 0.1-0.4 deleted, 0.5 restructured
#80802 cluster_words_kmeans ⇒ clustering.py for further dev,
    #number_of_clusters copied here from clustering.py,
    # This file maintained for POC.0.5 legacy compatibility
