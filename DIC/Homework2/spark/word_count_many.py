def countWords(sc, files, weirdPartition = False):
  output = None
  for file in files:
    if weirdPartition:
      # DON'T ACTUALLY DO THIS! Just for demo purposes
      lines = sc.textFile(file, minPartitions = len(file)) 
    else:
      lines = sc.textFile(file) 
    counts = lines.flatMap(lambda x: x.split(' ')) \
                  .map(lambda x: (x, 1)) \
                  .reduceByKey(lambda a,b: a + b)

    if output == None:
      output = counts
    else:
      output = output.fullOuterJoin(counts)
  return output

if __name__ == '__main__':
  from pyspark.context import SparkContext
  sc = SparkContext('local', 'test')

  countWords(sc, ["frankenstein.txt", "dracula.txt", "war_and_peace.txt"]).saveAsTextFile("output")
