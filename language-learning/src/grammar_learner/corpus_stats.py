# language-learning/src/grammar_learner/corpus_stats.py                 # 90219
from collections import Counter


def corpus_stats(lines, extended = False):
    """
    Calculate statistics for a parsed corpus.

    Args:
        lines: list of strings from parsed file
        extended: if True, include detailed statistics

    Returns:
        dict with corpus statistics
    """
    # lines :: [str] -- parses file converted to a list of strings
    words = Counter()   # words in sentences
    pw = Counter()      # parsed words
    npw = Counter()     # non-parsed words
    lefts = Counter()   # left words in links
    rights = Counter()  # right words in links
    links = Counter()   # tuples: (left_word, right_word)
    lw = Counter()      # linked words
    nlw = Counter()     # non-linked words (words that appear but are never linked)
    nlws = set()        # a set of non-linked word indices in current sentence
    nnlws = 0           # number of non-linked word occasions in all sentences
    sentence = []       # a list of words (used within loops)
    sentence_lengths = []  # a list of sentence lengths (to find max, mean)

    for line in lines:
        if(len(line)) > 1:
            x = line.split()
            if len(x) in [4, 5] and x[0].isdigit() and x[2].isdigit():
                if x[1] != '###LEFT-WALL###' and x[3] != '.':
                    links[(x[1], x[3])] += 1
                    lefts[x[1]] += 1
                    rights[x[3]] += 1
                    lw[x[1]] += 1
                    lw[x[3]] += 1
                    nlws.discard(int(x[0]) - 1)
                    nlws.discard(int(x[2]) - 1)
            elif len(x) > 0:  # sentence:
                # Count non-linked words in the previous sentence
                if len(nlws) > 0:  # indices of non-parsed words in sentence
                    nnlws += len(nlws)  # number of non-linked words
                    for j in nlws:
                        if j < len(sentence):
                            nlw[sentence[j]] += 1  # Track non-linked word counts
                    # nlws = set()
                # sentence = []
                # Count only parsed words (excluding ###LEFT-WALL### and .)
                parsed_word_count = len([w for w in x if w not in ['###LEFT-WALL###', '.']])
                sentence_lengths.append(parsed_word_count)
                sentence = x
                nlws = set()  # non-linked words in sentence (indices: 0,1,...)
                for i, word in enumerate(x):
                    if word not in ['###LEFT-WALL###', '.']:
                        if word[0] == '[' and word[-1] == ']':
                            npw[word[1:-1]] += 1  # npw: non-parsed words
                            words[word[1:-1]] += 1
                        else:
                            pw[word] += 1  # pw: parsed words
                            words[word] += 1
                            nlws.add(i)    # non-linked word index in sentence
            else: continue  # len(line.split) == 0 :: empty line
        else: continue  # len(line) == 0 :: empty line

    asl, msl, awc, alc, alw = 0, 0, 0, 0, 0  # see keys in response below
    if len(sentence_lengths) > 0:
        msl = max(sentence_lengths)
        asl = int(round(sum(words.values())/len(sentence_lengths), 0))
    if len(words) > 0:
        awc = int(round(sum(words.values())/len(words), 0))
    if len(links) > 0:
        alc = round(sum(links.values())/len(links), 1)
        if len(lw) > 0:
            alw = int(round(sum(links.values())/len(lw), 0))
    unpws = set(npw) - set(pw)      # unique non-parsed words
    unlws = set(words) - set(lw)    # unique non-linked words
    lost_words = set(words) - (set(lefts) | set(rights))  # unique never linked

    response = {'corpus_stats': [
        ['Number of sentences    ', len(sentence_lengths)],
        ['Maximum sentence length', msl],
        ['Average sentence length', asl],
        ['Number of unique words in sentences', len(words)],
        ['Number of unique parsed words      ', len(pw)],
        ['Number of unique non-parsed [words]', len(unpws)],
        ['Number of unique linked words      ', len(lw)],
        ['Number of unique non-linked words  ', len(unlws)],
        ['Total  words count in sentences    ', sum(words.values())],
        ['Parsed words count in sentences    ', sum(pw.values())],
        ['Non-parsed [words] in sentences    ', sum(npw.values())],
        ['Non-linked words (excl.non-parsed) ', len(lost_words)],  # nlws?
        ['Average word count ', awc],
        ['Unique links number', len(links)],
        ['Total  links count ', sum(links.values())],
        ['Average link count ', alc],
        ['Average links per linked word', alw]
    ]}
    if extended:
        response.update({
            'links_stats': {
                'unique_left_words': len(lefts),
                'unique_right_words': len(rights),
                'left_&_right_intersection': len(lefts & rights),
                'left_|_right_union': len(lefts | rights),
                'lost_words': len(lost_words),
                'non_parsed|lost_words': len(unpws | lost_words),
                'non_linked_word_occasions': nnlws,
                'unique_non_linked_words': len(nlw)
            },
            'unique non-parsed words': unpws,
            'unique non-linked words': unlws,
            'lost_words': lost_words,
            'non_linked_word_counts': dict(nlw)  # Include the actual counts
        })

    return response


# Notes:
# 80802 poc05 restructured: moved here from pparser.py
# 80829,31 unpws, unlws
# 81231 cleanup
# 90217 update for use with filtered dataset
# 90219 count non-linked words, not marked as [not parsed] -- nlw, nlws, nnlws
# 250104 Fixed: nlw counter now properly returned in extended stats
#        Sentence lengths now count only parsed words (excl. ###LEFT-WALL### and .)
