#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

"""
This is an example implementation of PageRank. For more conventional use,
Please refer to PageRank implementation provided by graphx
"""
import re
import sys
from operator import add
from typing import Iterable, Tuple

from pyspark.resultiterable import ResultIterable

def computeContribs(urls: ResultIterable[str], rank: float) -> Iterable[Tuple[str, float]]:
    """Calculates URL contributions to the rank of other URLs."""
    num_urls = len(urls)
    for url in urls:
        yield (url, rank / num_urls)


def parseNeighbors(urls: str) -> Tuple[str, str]:
  """Parses a urls pair string into urls pair."""
  parts = re.split(r'\s+', urls)
  return parts[0], parts[1]

def pageRank(sc, file, iters, cache = False):
  # Loads in input file. It should be in format of:
  #     URL         neighbor URL
  #     URL         neighbor URL
  #     URL         neighbor URL
  #     ...
  lines = sc.textFile(file)

  # Loads all URLs from input file and initialize their neighbors.
  links = lines.map(lambda urls: parseNeighbors(urls)) \
               .groupByKey()
  if cache:
    links.cache()
  N = links.count()

  # Loads all URLs with other URL(s) link to from input file and initialize ranks to 1/N.
  ranks = links.map(lambda u: (u[0], 1.0/N))

  # Calculates and updates URL ranks continuously using PageRank algorithm.
  for i in range(iters):
    # Calculates URL contributions to the rank of other URLs.
    contribs = links.join(ranks) \
                    .flatMap(lambda u: computeContribs(u[1][0], u[1][1]))

    # Re-calculates URL ranks based on neighbor contributions.
    ranks = contribs.reduceByKey(lambda a,b: a+b) \
                    .mapValues(lambda rank: rank * 0.85 + 0.15*(1.0/N))
  return ranks

if __name__ == '__main__':
  from pyspark.context import SparkContext
  sc = SparkContext('local', 'test')

  pageRank(sc, "page_rank_data.txt", 10, True).saveAsTextFile("output")
