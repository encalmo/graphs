<a href="https://github.com/encalmo/graphs">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/graphs_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/graphs_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/graphs/scaladoc/org/encalmo/data.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# graphs

Scala library for processing graphs.

## Table of contents

- [Dependencies](#dependencies)
- [Usage](#usage)
- [Project content](#project-content)

## Dependencies

   - JVM >= 21
   - [Scala](https://www.scala-lang.org) >= 3.5.2

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" % "graphs_3" % "0.9.3"

or with SCALA-CLI

    //> using dep org.encalmo::graphs:0.9.3


## Project content

```
├── .github
│   └── workflows
│       ├── pages.yaml
│       ├── release.yaml
│       └── test.yaml
│
├── .gitignore
├── .scalafmt.conf
├── Graph.scala
├── Graph.test.scala
├── Heap.scala
├── Heap.test.scala
├── IntTraversable.scala
├── LICENSE
├── project.scala
├── QuickSort.scala
├── README.md
├── test-resources
│   ├── dijkstraData.txt
│   ├── graph1.txt
│   ├── HashInt.txt
│   ├── inversions.txt
│   ├── Median.txt
│   ├── quicksort.txt
│   └── SCC.txt
│
├── test.sh
├── Traversable.scala
└── Traversable.test.scala
```

