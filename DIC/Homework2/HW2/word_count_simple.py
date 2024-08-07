import string
def countWords(sc, file):
  lines = sc.textFile(file) 
  stopwords = ["a", "about", "after", "all", "also", "an", "and", "any", "are", "as", "at", "be", 
                "because", "been", "before", "being", "between", "both", "but", "by", "can", "could", "did", 
                "do", "does", "during", "each", "either", "for", "from", "had", "has", "have", "having", "he", 
                "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just",
                "like", "made", "many", "me", "might", "more", "most", "much", "must", "my", "never", "no", "not", 
                "now", "of", "on", "only", "or", "other", "our", "out", "over", "said", "same", "see", "should", "since",
                "so", "some", "still", "such", "take", "than", "that", "the", "their", "them", "then", "there", "these", 
                "they", "this", "those", "through", "to", "too", "under", "up","use", "very", "want", "was", "way", "we",
                "well", "were", "what", "when", "where", "which", "while", "who", "will", "with", "would", "you", "your"]
  translator = str.maketrans('', '', string.punctuation + string.digits)
  counts = (lines.flatMap(lambda x: x.lower().translate(translator).split())
          .map(lambda x: (x, 1))
          .filter(lambda x: x[0] not in stopwords)
          .reduceByKey(lambda a, b: a + b))

  return counts

if __name__ == '__main__':
  from pyspark.context import SparkContext
  sc = SparkContext('local', 'test')

  counts = countWords(sc, "BunnyBrownandhisSister.txt")
  counts.sortBy(lambda x: x[1], False).saveAsTextFile("output")
  #counts.reduceByKey(lambda a,b: a + b).sortBy(lambda x: x[1], False).saveAsTextFile("output") 
