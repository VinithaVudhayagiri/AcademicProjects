def countWords(sc, file):
  lines = sc.textFile(file) 
  counts = lines.flatMap(lambda x: x.split(' ')) \
                .map(lambda x: (x.lower(), 1)) \
		.filter(lambda x: x[0] not in ["the","and","is"])\
                .reduceByKey(lambda a,b: a + b)

  return counts

if __name__ == '__main__':
  from pyspark.context import SparkContext
  sc = SparkContext('local', 'test')

  counts = countWords(sc, "frankenstein.txt")
  counts.sortBy(lambda x: x[1], False).saveAsTextFile("output")
