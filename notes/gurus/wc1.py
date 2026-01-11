#!/usr/bin/env python3 -B
"""Word frequency counter - refactored following SE heuristics"""

#--- Policy (data - easy to change) ---
CONFIG = {
  'top_n': 10,
  'stopwords': ["the", "a", "an", "and", "or", "but", "in", "on", "at", 
                "to", "for", "of", "is", "was", "are", "were", "be", 
                "been", "with"],
  'punctuation': '.,!?;:"()[]-'
}

#--- Model (pure functions, no I/O) ---
def readText(file):
  """Read text from file"""
  with open(file) as f:
    return f.read()

def cleanWord(word, punctuation):
  """Remove punctuation from word"""
  return word.strip(punctuation)

def isValid(word, stopwords):
  """Check if word should be counted"""
  return word and word not in stopwords

def countWords(text, stopwords, punctuation):
  """Count word frequencies"""
  words = text.lower().split()
  counts = {}
  for word in words:
    word = cleanWord(word, punctuation)
    if isValid(word, stopwords):
      counts[word] = counts.get(word, 0) + 1
  return counts

def topN(counts, n):
  """Get top N words by frequency"""
  return sorted(counts.items(), key=lambda x: x[1], reverse=True)[:n]

def analyze(file, config=CONFIG):
  """Analyze word frequencies in file"""
  text = readText(file)
  counts = countWords(text, config['stopwords'], config['punctuation'])
  top = topN(counts, config['top_n'])
  return {
    'total': sum(counts.values()),
    'unique': len(counts),
    'top': top,
    'counts': counts
  }

#--- Presentation (I/O only, no logic) ---
def showHeader(file):
  print(f"\n{'='*50}")
  print(f"WORD FREQUENCY ANALYSIS - {file}")
  print(f"{'='*50}\n")

def showStats(result):
  print(f"Total words (after removing stopwords): {result['total']}")
  print(f"Unique words: {result['unique']}\n")

def showTop(result, n):
  print(f"Top {n} most frequent words:\n")
  for i, (word, count) in enumerate(result['top'], 1):
    bar = "*" * count
    print(f"{i:2}. {word:15} {count:3} {bar}")
  print()

def report(file, result):
  showHeader(file)
  showStats(result)
  showTop(result, CONFIG['top_n'])

#--- Main ---
def count_words(file="essay.txt"):
  result = analyze(file)
  report(file, result)
  return result

count_words("essay.txt")
