package Lab1

import scala.io.Source
import java.io.FileWriter
import com.github.tototoshi.csv._

import scala.collection.immutable.ListMap

object Assignment {
  def main(args: Array[String]): Unit = {
    class WordCloudGenerator(var str: List[String]) {
      var WORDS: Map[String, List[String]] = this.create_words_list(this.str)
      var OCCURENCES: Map[String, List[(String, Int)]] = count_word_occurences()
      var NAMES: List[String] = List()

      def create_words_list(str: List[String]): Map[String, List[String]] = {
        var w: List[String] = List()
        var w1: Map[String, List[String]] = Map()
        var names: List[String] = List()
        var i = 0
        str.foreach(s => {
          i += 1
          if (s.contains(".txt")) {
            val fs = Source.fromFile(s)
            w = w :+ fs.mkString
            names = names :+ s
            fs.close
          } else {
            names = names :+ i.toString
            w = w :+ s
          }
        })
        this.NAMES = names;
        for ((content, name) <- w.zip(names)) {
          w1 += (name -> content.split("\\W+").filterNot(_ == "").toList.map(_.toLowerCase))
        }
        w1
      }

      def count_word_occurences(whole_collection:Boolean=false): Map[String, List[(String, Int)]] = {
        var res: Map[String, List[(String, Int)]] = Map()
        if (!whole_collection) 
          this.WORDS.foreach(x => {
            res += (x._1 -> x._2.map(_.toLowerCase).groupBy(n => n).toList.map { case (word, arr) => (word, arr.length) })
          })
        else {
          var key: String = "whole_collection"
          var all_words: List[String] = List()
          this.WORDS.foreach(x => all_words = all_words ++ x._2)
          res += (key -> all_words.map(_.toLowerCase).groupBy(n => n).toList.map { case (word, arr) => (word, arr.length) })
        }
        res
      }

      def k_most_common_words(map: Map[String, List[(String, Int)]]=this.OCCURENCES, k: Int): Map[String, List[(String, Int)]] = {
        var res: Map[String, List[(String, Int)]] = Map()
        map.foreach(x => {
          res += (x._1 -> x._2.sortBy(_._2)(Ordering[Int].reverse).take(k))
        })
        res
      }

      def remove_stopwords(stopwords_path: String = "resources/stop_words_english.txt"): Unit = {
        val stopwords: List[String] = this.create_words_list(List(stopwords_path)).values.flatten.toList
        var res: Map[String, List[String]] = Map()
        this.WORDS.foreach(x => {
          res += (x._1 -> x._2.filter(!stopwords.contains(_)))
        })
        this.WORDS = res
        this.OCCURENCES = count_word_occurences()
      }

      def _find_occurrence(word: String, text_name: String): Int = {
        this.OCCURENCES(text_name).filter(x => x._1 == word).map(x => x._2).head
      }

      def tfidf(): Map[String, Map[String, Double]] = {
        var res: Map[String, Map[String, Double]] = Map()
        for ((name, content) <- this.WORDS) {
          res += (name -> Map())
          content.foreach(word => {
            var tf: BigDecimal = 0.0
            var idf: BigDecimal = 0.0
            this.WORDS.foreach(x => {
              if (x._2.contains(word))
                idf += 1
            })
            tf = BigDecimal(this._find_occurrence(word, name).toDouble / this.WORDS(name).length.toDouble)
            idf = BigDecimal(Math.log10(this.WORDS.size.toDouble / idf.toDouble))
            res = res.updated(name, res(name) + ((word -> (tf * idf).toDouble)))
          })
        }
        res
      }
    }

    def print_tfidf(map: Map[String, Map[String, Double]]): Unit = {
      map.foreach(x => {
        println(s"Text: ${x._1}")
        x._2.foreach(i => {
          println(s"Word: ${i._1}, value: ${BigDecimal(i._2).toString}")
        })
        println()
      })
    }

    def k_most_common_tfidf(map: Map[String, Map[String, Double]], k: Int): Map[String, Map[String, Double]] = {
      var res: ListMap[String, Map[String, Double]] = ListMap()
      map.foreach(x => res += (x._1 -> ListMap(x._2.toSeq.sortWith(_._2 > _._2).take(k):_*)))
      res
    }


    // Testing on 3 different books
    var books: List[String] = List("resources/harry_potter_and_the_philosophers_stone.txt", "resources/alice_in_wonderland.txt", "resources/twenty_thousand_leagues_under_the_water.txt")
    var wcg: WordCloudGenerator = new WordCloudGenerator(books)
    wcg.remove_stopwords()
    var all_words_occurrences = wcg.count_word_occurences(whole_collection=true)

    println("=====Most common words for the whole collection=====")
    println(wcg.k_most_common_words(all_words_occurrences, k=10))
    println("=====Most common words per book=====")
    println(wcg.k_most_common_words(k=10))
    println("=====Words with the biggest tfidf value=====")
    print_tfidf(k_most_common_tfidf(wcg.tfidf(), 10))
    // OUTPUT:
      // =====Most common words for the whole collection=====
      // Map(whole_collection -> List((harry,1672), (captain,649), (_nautilus_,509), (potter,448), (stone,443), (ron,429), (alice,399), (nemo,388), (sea,383), (hagrid,370)))
      // =====Most common words per book=====
      // Map(resources/harry_potter_and_the_philosophers_stone.txt -> List((harry,1672), (potter,447), (ron,429), (stone,427), 
      // (hagrid,370), (philosophers,347), (rowling,347), (hermione,269), (professor,180), (snape,171)), 
      // resources/alice_in_wonderland.txt -> List((alice,399), (queen,75), (thought,74), (time,71), (king,63), (turtle,59), (began,58), (mock,56), (hatter,56), (gryphon,55)), 
      // resources/twenty_thousand_leagues_under_the_water.txt -> List((captain,644), (_nautilus_,509), (nemo,388), (sea,361), (ned,324), (conseil,285), (land,262),
      //  (water,236), (sir,197), (long,182)))
      // =====Words with the biggest tfidf value=====
      // Text: resources/harry_potter_and_the_philosophers_stone.txt
      // Word: harry, value: 0.02565349512465111
      // Word: ron, value: 0.006582146775403904
      // Word: hagrid, value: 0.005676909806292411
      // Word: philosophers, value: 0.005324020818333693
      // Word: rowling, value: 0.005324020818333693
      // Word: hermione, value: 0.004127266859169348
      // Word: snape, value: 0.0026236529104756816
      // Word: potter, value: 0.0025312021351863367
      // Word: dumbledore, value: 0.0024548799162345558
      // Word: stone, value: 0.002417949243231691

      // Text: resources/alice_in_wonderland.txt
      // Word: alice, value: 0.02446618437644844
      // Word: mock, value: 0.003433850438799781
      // Word: hatter, value: 0.003433850438799781
      // Word: gryphon, value: 0.0033725316809640705
      // Word: duchess, value: 0.002575387829099836
      // Word: dormouse, value: 0.002452750313428415
      // Word: hare, value: 0.0019008814929070215
      // Word: caterpillar, value: 0.0017169252193998904
      // Word: jury, value: 0.0013490126723856284
      // Word: turtle, value: 0.0013352248148419475

      // Text: resources/twenty_thousand_leagues_under_the_water.txt
      // Word: _nautilus_, value: 0.005696135068659744
      // Word: nemo, value: 0.004342044020903695
      // Word: ned, value: 0.0036258305741566936
      // Word: conseil, value: 0.00318938800504524
      // Word: captain, value: 0.0026598515499439124
      // Word: surface, value: 0.0016226710902861746
      // Word: canadian, value: 0.0016114802551807528
      // Word: ocean, value: 0.0014548085637048462
      // Word: vessel, value: 0.001175037686069299
      // Word: gutenberg, value: 0.0010967018403313456

    // Example: Word: harry, tfidf value: 0.01618555335724271 ====> 0.05 (TF: 1672 / 31097 ) * 0.3 (log10(2/1))


    // MapReduce 
    def _map(in: List[(Int, List[Int])]): List[(Int, Int)] = {
      var res: List[(Int, Int)] = List()

      for ((source, destinations) <- in) {
        destinations.foreach(destination => {
          res = res :+ (destination, source)
        })
      }
      res
    }

    def _reduce(in: List[(Int, Int)]): List[(Int, List[Int])] = {
      var res: List[(Int, List[Int])] = List()

      for ((source, destination) <- in) {
        var i = 0
        if (res.map(_._1).contains(source)) {
          i = res.map(_._1).indexOf(source)
          res = res.updated(i, (res(i)._1, res(i)._2 :+ destination))
        } else {
          res = res :+ (source, List(destination))
        }
      }
      res
    }

    def MapReduceEdgesInversion(in: List[(Int, List[Int])]): List[(Int, List[Int])] = {
      _reduce(_map(in))
    }

   val input: List[(Int, List[Int])] = List((1, List(2, 3)), (3, List(1, 5)), (2, List(5)), (5, List()))
   println(input)
   println(MapReduceEdgesInversion(input))
   println(MapReduceEdgesInversion(MapReduceEdgesInversion(input)))
   // OUTPUT:
    // List((1,List(2, 3)), (3,List(1, 5)), (2,List(5)), (5,List()))
    // List((2,List(1)), (3,List(1)), (1,List(3)), (5,List(3, 2)))
    // List((1,List(2, 3)), (3,List(1, 5)), (2,List(5)))
  }
}
