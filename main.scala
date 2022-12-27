import scala.io.Source
import java.nio.file.{FileSystems, Files}
import scala.collection.JavaConverters._
import scala.util.Random


def kshingles(s: String, k: Int): Set[String] = {
  s.sliding(k).toSet
}


def jaccardSimilarity(s1: Set[String], s2: Set[String], precision: Int = 5): Double = {
  BigDecimal(s1.intersect(s2).size.toFloat / s1.union(s2).size.toFloat).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble
}


def task1(filepath: String): Unit = {

  def mapFunction(line: String): List[Map[Int, (Int, Int)]] = {
    var l = line.split("\t")
    List(Map(l(0).toInt -> (1, 0)), Map(l(1).toInt -> (0, 1)))
  }

  def reduceFunction(a: Map[Int, (Int, Int)], b: Map[Int, (Int, Int)]): Map[Int, (Int, Int)] = {
    var aKeys = a.keys.toSeq
    var bKeys = b.keys.toSeq

    if (aKeys contains bKeys(0)) {
      a.updatedWith(bKeys(0)) {
        case Some(value) => Some(value(0) + b.values.toList(0)(0), value(1) + b.values.toList(0)(1))
        case None => None
      }
    } else 
      a ++ b

  }

  var a = Source.fromFile(filepath).getLines.flatMap(mapFunction).reduce(reduceFunction)
  println(a)
}


def listFolder(folderPath: String): Vector[String] = {
  var fPath = FileSystems.getDefault.getPath(folderPath)
  var folderFiles = Files.list(fPath).iterator().asScala.toVector.map(x => x.toString)
  folderFiles
}


def task2(folderPath: String, k: Int): Unit = {
  var folderFiles = listFolder(folderPath)
  folderFiles.combinations(2).map(books => { 
    Map((books(0), books(1)) -> jaccardSimilarity(kshingles(Source.fromFile(books(0)).getLines.mkString, k), 
                                                  kshingles(Source.fromFile(books(1)).getLines.mkString, k)))
  }).foreach(println)
}

def boolToInt(b: Boolean): Int = {
  if (b) 1 else 0
}


def findOnes(charMatrix: Vector[Vector[Int]]): Vector[Int] = {
  charMatrix.transpose.map(matr => matr.indexOf(1))
}


def findIndexes(vec: Vector[Int]): Vector[Int] = {
  vec.zipWithIndex.filter(pair => pair._1 == 1).map(pair => pair._2).toVector
}


def task3(folderPath: String, k: Int, n: Int): Unit = {
  var folderFiles: Vector[String] = listFolder(folderPath)
  var shingles: Vector[Set[String]] = folderFiles.map(x => kshingles(Source.fromFile(x).getLines.mkString, k))
  var joinedShingles: Vector[Any] = shingles.foldLeft(Set())(_ ++ _).toVector

  var characteristicMatrix: Vector[Vector[Int]] = Vector.tabulate(joinedShingles.size, folderFiles.length)((i, j) => 0)
  characteristicMatrix = characteristicMatrix.zipWithIndex.map((row, index) => shingles.map(shingle => boolToInt(shingle contains joinedShingles(index).toString))) 

  var hashTable: Vector[Vector[Int]] = Vector.tabulate(n)(i => scala.util.Random.shuffle(Vector.range(0, characteristicMatrix.length))).transpose
  var hashes: Vector[Vector[Int]] = Vector.tabulate(characteristicMatrix(0).length, hashTable(0).length)((i, j) => Int.MaxValue)

  (0 to characteristicMatrix.length - 1).toList.foreach(i => {
    findIndexes(characteristicMatrix(i)).map(j => (j, hashes(j))).foreach(x => {
      hashes = hashes.updated(x._1, hashTable(i).zip(x._2).map((a, b) => a.min(b)))
    })
  })
  
  hashes.zipWithIndex.combinations(2).map(x => Map((folderFiles(x(0)(1)), folderFiles(x(1)(1))) -> 
                                                jaccardSimilarity(x(0)(0).toSet.map(_.toString), x(1)(0).toSet.map(_.toString)))).toVector
                                                .foreach(println)
}



@main def main(): Unit = {
  task1("resources/task1/web-Stanford.txt")
  // task1("resources/task1/task1.txt")


  (4 to 13).toList.foreach(k => {
    println(s"######## For k = $k")
    task2("resources/task2", k)
  })

  // OUTPUT:
    // In out/task2_output.txt

  (4 to 8).foreach(k => List(10, 100, 250, 350).foreach(n => { println(s"######## For k = $k, n = $n"); task3("resources/task2", k, n) }))
  // OUTPUT
    // In out/task3_output.txt
}