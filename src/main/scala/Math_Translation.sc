import scala.collection.mutable

val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
val validWords: List[String] = english ::: chinese
val map = new mutable.HashMap[String, String]()
val stringToInt = new mutable.HashMap[String, Int]()

// Test Lists
val cList: List[String] = List("yi", "nine", "six", "ba")
val eList: List[String] = List("one", "nine", "six", "eight")
val testList : List[String] = List("yi", "Josh", "three", "si")

def mapTranslation() = {
  for(i <- 0 to english.size-1) {
    map.put(chinese(i), english(i))
    stringToInt.put(english(i), i)
  }
}

var output : List[Int] = List()
var headers : Boolean = false

def translate(list : List[String]) : List[Int] = {
  mapTranslation()
  var result = list.filter(validWords.contains(_))
  result match {
    case Nil => Nil
    case head :: tail =>
      if(chinese contains head)
        output = stringToInt(chineseToEnglish(head)) :: output
      else
        output = stringToInt(head) :: output
      translate(tail)
      headers = true
  }
  if(!headers) {
    println("Translation: " + output.reverse.mkString(" "))
    println("Addition: " + output.reverse.mkString(" + ") + " = " + output.sum)
    println("Multiplication : " + output.reverse.mkString(" * ") + " = " + output.product)
  }
  output
}


def chineseToEnglish (chineseWord : String) : String = {
  map(chineseWord)
}

translate(eList)
translate(cList)
translate(testList)