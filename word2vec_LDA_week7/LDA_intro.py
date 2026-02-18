#This program assesses meaning in The Wealth of Nations with two different models

#Word2Vec uses local context and word order to learn meaning
#instead of storingi explicit co-occurrence counts, Word2Vec learns dense vectors for each word
#the vectors are learned by optimizing a predictive task

#LDA requires a bag-of-words representation to model documents as mixtures of topics

from pathlib import Path
import re
from collections import Counter

path = Path("wealth.txt")
text = path.read_text(encoding="utf-8", errors="replace")
print("Characters: ", len(text))
print("Lines: ", text.count("\n") + 1)

print("\n--- START ---\n")
print(text[:800])

mid = len(text)//2
print("\n--- MIDDLE SLICE ---\n")
print(text[mid:mid+800])

#tokenize text and lowercase
tokens = re.findall(r"[A-Za-z]+(?:'[A-Za-z]+)?", text.lower())
print("Tokens: ", len(tokens))
print("Unique tokens: ", len(set(tokens)))

counts = Counter(tokens)
print("\n Top 25 tokens:")
for w, c in counts.most_common(25):
    print(f"{w:>12} {c}")

import sklearn
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS
stopwords = set(ENGLISH_STOP_WORDS)
#convert list of pre-defined stopwords into a set to make filtering tokens more efficient
#add custom stopwords to standard stopword set
custom_stopwords = {"barnacle"}
stopwords |= custom_stopwords
print("Stopwords loaded: ", len(stopwords))
print("Sample: ", sorted(list(stopwords))[:25])

from collections import Counter
clean_tokens = [t for t in tokens if t not in stopwords and len(t) >= 3]
print("Tokens (raw): ", len(tokens))
print("Tokens (clean): ", len(clean_tokens))
print("Unique tokens (clean): ", len(set(clean_tokens)))

clean_counts = Counter(clean_tokens)
print("\nTop 25 tokens after cleaning:")
for w, c in clean_counts.most_common(25):
    print(f"{w:>12} {c}")

    #segment The Wealth of Nations into fixed-sized (800-token) segments
    #this approach ensure that no single segment dominates the model simply because of its length

    #Segmenting
SEGMENT_SIZE = 800
segments = [
    clean_tokens[i:i+SEGMENT_SIZE]
    for i in range(0, len(clean_tokens), SEGMENT_SIZE)
]
print("Number of segments: ", len(segments))
print("First segment length:", len(segments[0]))
print("Last segment length:", len(segments[-1]))

segment_counts = [Counter(seg) for seg in segments]

# check: top words in segment 0
print("Top words in segment 0:")
for w,c in segment_counts[0].most_common(15):
    print(f"{w:>12} {c}")

# Key step: Word2Vec training

from gensim.models import Word2Vec  

w2v = Word2Vec(
    sentences=segments,     # each segment is a list of tokens
    vector_size=100,        # dimensionality
    window=5,               # context window (parallel to your co-occurrence window)
    min_count=10,           # ignore very rare words
    workers=4,              # number of CPU cores for parallel processing
    sg=1                    # 1=skip-gram, 0=CBOW
)

print("Vocabulary size:", len(w2v.wv.key_to_index))

# Nearest neighbors (cosine similarity)

def show_neighbors(word, topn=10):
    if word not in w2v.wv:
        print(f"'{word}' not in vocabulary.")
        return
    print(f"\nNearest neighbors for '{word}':")
    for w, sim in w2v.wv.most_similar(word, topn=topn):
        print(f"{w:>12}  {sim:.3f}")

for target in ["trade", "labor", "price", "capital", "market"]:
    show_neighbors(target, topn=10)

# Co-occurrence neighbors (window = 5)

from collections import Counter, defaultdict

WINDOW = 5
cooc = defaultdict(Counter)

for seg in segments:
    for i, token in enumerate(seg):
        start = max(0, i - WINDOW)
        end = min(len(seg), i + WINDOW + 1)
        for j in range(start, end):
            if i != j and seg[j] != token: # otherwise "trade" will be the closest to "trade" etc.
                cooc[token][seg[j]] += 1

def show_cooc_neighbors(word, topn=10):
    print(f"\nCo-occurrence neighbors for '{word}':")
    for w, c in cooc[word].most_common(topn):
        print(f"{w:>12}  {c}")

for target in ["trade", "labor", "price"]:
    show_cooc_neighbors(target, topn=10)


#LDA topic modeling
from gensim.corpora import Dictionary

# Build a dictionary from the segments
dictionary = Dictionary(segments)

# do some cleaning
dictionary.filter_extremes(
    no_below=10,    # must appear in at least 10 segments
    no_above=0.5   # must appear in no more than 50% of segments
)

print("Vocabulary size after filtering:", len(dictionary))

# Convert segments to bag-of-words format
corpus = [dictionary.doc2bow(seg) for seg in segments]

#Take a look
print("First document (bow format):")

print(corpus[0][:10])

# Train LDA model

from gensim.models import LdaModel

lda = LdaModel(
    corpus=corpus,
    id2word=dictionary,
    num_topics=8,
    random_state=42,
    passes=10,
    alpha="auto",
    eta="auto"
)

print("LDA model trained.")

for topic_id in range(lda.num_topics):
    print(f"\nTopic {topic_id}:")
    for word, weight in lda.show_topic(topic_id, topn=12):
        print(f"{word:>12}  {weight:.3f}")

# After training, inspect learned alpha values
print("Learned alpha values:", lda.alpha)

import numpy as np
import matplotlib.pyplot as plt

K = lda.num_topics
topic_mass = np.zeros(K)

# Sum topic probabilities over all segments
for bow in corpus:
    doc_topics = lda.get_document_topics(bow, minimum_probability=0)
    for k, p in doc_topics:
        topic_mass[k] += p

# Normalize to proportions (so bars sum to 1)
topic_share = topic_mass / topic_mass.sum()

# Plot
plt.figure()
plt.bar(range(K), topic_share)
plt.xticks(range(K), [f"T{k}" for k in range(K)])

plt.ylabel("Share of topic mass (across segments)")
plt.xlabel("Topic")
plt.title("LDA topic prevalence in Wealth of Nations (by segment)")
plt.show()

# Print the numeric values too (useful for interpretation)
for k, s in enumerate(topic_share):
    print(f"Topic {k}: {s:.3f}")

#visualize topic prevalence by segment

# Get per-segment topic distributions
doc_topic_matrix = np.array([
    [p for _, p in lda.get_document_topics(bow, minimum_probability=0)]
    for bow in corpus
])

plt.figure()

# Plot only the top 4 topics by overall prevalence (less clutter)
top_topics = np.argsort(topic_share)[-4:]

for k in top_topics:
    plt.plot(doc_topic_matrix[:, k], label=f"Topic {k}")

plt.xlabel("Segment index (approx. book progression)")
plt.ylabel("Topic proportion")
plt.title("Topic prevalence across Wealth of Nations")
plt.legend()
plt.show()