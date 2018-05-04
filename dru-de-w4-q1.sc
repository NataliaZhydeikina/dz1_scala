// DataRoot University DE W4 Quiz #1
import scala.math._;

//======================================================================================================================
// Task #1
def stringXOR(string: String): Int = {
  def loop(arr: List[Int]) : Int =
    if(arr.length <= 1) arr(0)
    else {
      var resultArr = List[Int]()
      for(i <- 0 to arr.length-1 if i%2 == 0)
        if(i<arr.length-1){
          resultArr :+= arr(i)^arr(i+1)
        }
        else resultArr :+= arr(i)
      loop(resultArr)
    }
  loop(string.split(" ").map(_.toInt).toList)
} // todo: replace with your implementation

stringXOR("1 0 0 1 0") == 0
stringXOR("1 0 1 1 1 0 0 1 0 0 0 0") == 1
stringXOR("1 0 0 1 0 1 0 0 1 0 1 0 1 0 1 0 1 0") == 0


//======================================================================================================================
// Task #2
def multiples(n: Int): Int = (Range(3, n, 3) ++ Range(5, n, 5)).toSet.sum; // todo: replace with your implementation

multiples(10) == 23
multiples(23) == 119
multiples(119) == 3300
multiples(1000) == 233168


//======================================================================================================================
// Task #3
def automorphic(n: Int): Boolean = ((n*n - n) % pow(10, n.toString.length)) == 0;// todo: replace with your implementation

automorphic(1)
!automorphic(3)
automorphic(6)
!automorphic(9)
automorphic(25)
!automorphic(53)
automorphic(76)
!automorphic(95)
automorphic(625)
!automorphic(225)


//======================================================================================================================
// Task #4
def simpleIndices(string: String, startIndex: Int): Int ={
  if(string.charAt(startIndex) != '(') -1
  else {
    var openBrackets = 1; var i = startIndex;
    while (i < string.length && (openBrackets !=0 || i == startIndex) ){
      i+=1;
      if(string(i) == '(') openBrackets+=1
      if(string(i) == ')') openBrackets-=1 };
    if(i<string.length) i else -1 }}



simpleIndices("((1)23(45))(aB)", 0) == 10
simpleIndices("((1)23(45))(aB)", 1) == 3
simpleIndices("((1)23(45))(aB)", 2) == -1
simpleIndices("((1)23(45))(aB)", 6) == 9
simpleIndices("((1)23(45))(aB)", 11) == 14
simpleIndices("(g(At)IO(f)(tM(qk)YF(n)Nr(E)))", 11) == 28
simpleIndices("(g(At)IO(f)(tM(qk)YF(n)Nr(E)))", 0) == 29
simpleIndices("(>_(va)`?(h)C(as)(x(hD)P|(fg)))", 19) == 22


//======================================================================================================================
// Task #5
def tribonacci(triple: (Int, Int, Int), n: Int): List[Int] = Nil // todo: replace with your implementation

tribonacci((1, 1, 1), 10) == List(1, 1, 1, 3, 5, 9, 17, 31, 57, 105)
tribonacci((0, 0, 1), 10) == List(0, 0, 1, 1, 2, 4, 7, 13, 24, 44)
tribonacci((0, 1, 1), 10) == List(0, 1, 1, 2, 4, 7, 13, 24, 44, 81)
tribonacci((1, 0, 0), 10) == List(1, 0, 0, 1, 1, 2, 4, 7, 13, 24)
tribonacci((0, 0, 0), 10) == List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
tribonacci((1, 2, 3), 10) == List(1, 2, 3, 6, 11, 20, 37, 68, 125, 230)
tribonacci((3, 2, 1), 10) == List(3, 2, 1, 6, 9, 16, 31, 56, 103, 190)
tribonacci((1, 1, 1), 1) == List(1)
tribonacci((300, 200, 100), 0) == Nil


//======================================================================================================================
// Task #6
def reverseFun(string: String): String = {
  def loop( result:String, currentString: String):String =
    if(currentString.length <= 1) result+currentString
    else
      loop( result+currentString.reverse(0), currentString.reverse.substring(1, currentString.length))
  loop("", string)
}
 // todo: replace with your implementation

reverseFun("012345") == "504132"
reverseFun("jointhedarkside") == "ejdoiisnktrhaed"


//======================================================================================================================
// Task #7
def foldAnArray(list: List[Int], nTimes: Int): List[Int] = {
  if(nTimes == 0) list else {
    var resultList = List[Int]();
    for(i <- 0 to (list.length.toFloat/2).round-1)
      if(i != list.length-1-i)
        resultList = resultList :+ list(i)+list(list.length-1-i)
      else resultList = resultList :+ list(i)
    foldAnArray(resultList, nTimes-1)
  }
} // todo: replace with your implementation

foldAnArray(List(1, 2, 3, 4, 5), 1) == List(6, 6, 3)
foldAnArray(List(1, 2, 3, 4, 5), 2) == List(9, 6)


//======================================================================================================================
// Task #8
def stringExpansion(string: String): String = "" // todo: replace with your implementation

stringExpansion("3abc") == "aaabbbccc"
stringExpansion("3D2a5d2f") == "DDDaadddddff"
stringExpansion("0d0a0v0t0y") == ""
stringExpansion("3d332f2a") == "dddffaa"
stringExpansion("abcde") == "abcde"


//======================================================================================================================
// Task #9
def lifePathNumber(date: String): Int = 0 // todo: replace with your implementation

lifePathNumber("1879-03-14") == 6 // Albert Einstein
lifePathNumber("1815-12-10") == 1 // Ada Lovelace
lifePathNumber("1961-07-04") == 1 // Brendan Eich
lifePathNumber("1955-10-28") == 4 // Bill Gates
lifePathNumber("1452-04-15") == 4 // Leonardo da Vinci
lifePathNumber("1791-12-26") == 2 // Charles Babbage
lifePathNumber("1906-12-09") == 1 // Grace Hopper
lifePathNumber("1912-06-23") == 6 // Alan Turing
lifePathNumber("1950-08-11") == 7 // Steve Wozniak
lifePathNumber("1956-01-31") == 8 // Guido van Rossum
lifePathNumber("1965-04-14") == 3 // Yukihiro Matsumoto
lifePathNumber("1971-06-28") == 7 // Elon Musk


//======================================================================================================================
// Task #10
case class ChromosomePair(chromosome1: List[String], chromosome2: List[String]) {
  def crossover(crossIndices: List[Int]): ChromosomePair = ChromosomePair(Nil, Nil) // todo: replace with your implementation
}

ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(Nil) == ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2"))
ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(List(1)) == ChromosomePair(List("1", "2", "2", "2", "2"), List("2", "1", "1", "1", "1"))
ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(List(1, 1)) == ChromosomePair(List("1", "2", "2", "2", "2"), List("2", "1", "1", "1", "1"))
ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(List(1, 3)) == ChromosomePair(List("1", "2", "2", "1", "1"), List("2", "1", "1", "2", "2"))
ChromosomePair(List("1", "1", "1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2", "2", "2")).crossover(List(1, 3, 5)) == ChromosomePair(List("1", "2", "2", "1", "1", "2", "2"), List("2", "1", "1", "2", "2", "1", "1"))
ChromosomePair(List("1", "1", "1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2", "2", "2")).crossover(List(3, 5, 1)) == ChromosomePair(List("1", "2", "2", "1", "1", "2", "2"), List("2", "1", "1", "2", "2", "1", "1"))


//======================================================================================================================
//Task #11
def fatFingers(string: String): String = "" // todo: replace with your implementation

fatFingers("The quick brown fox jumps over the lazy dog.") == "The quick brown fox jumps over the lZY DOG."
fatFingers("aAaaaaAaaaAAaAa") == ""
fatFingers("The end of the institution, maintenance, and administration of government, is to secure the existence of the body politic, to protect it, and to furnish the individuals who compose it with the power of enjoying in safety and tranquillity their natural rights, and the blessings of life: and whenever these great objects are not obtained, the people have a right to alter the government, and to take measures necessary for their safety, prosperity and happiness.") == "The end of the institution, mINTENnce, ND dministrTION OF GOVERNMENT, IS TO SECURE THE EXISTENCE OF THE BODY POLITIC, TO PROTECT IT, nd to furnish the individuLS WHO COMPOSE IT WITH THE POWER OF ENJOYING IN Sfety ND TRnquillity their nTURl rights, ND THE BLESSINGS OF LIFE: nd whenever these greT OBJECTS re not obtINED, THE PEOPLE Hve  RIGHT TO lter the government, ND TO Tke meSURES NECESSry for their sFETY, PROSPERITY nd hPPINESS."


//======================================================================================================================
// Task #12
def xbonacci(signature: List[Int], n: Int): List[Int] = Nil // todo: replace with your implementation

xbonacci(List(0, 1), 10) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
xbonacci(List(1, 1), 10) == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
xbonacci(List(0, 0, 0, 0, 1), 10) == List(0, 0, 0, 0, 1, 1, 2, 4, 8, 16)
xbonacci(List(1, 0, 0, 0, 0, 0, 1), 10) == List(1, 0, 0, 0, 0, 0, 1, 2, 3, 6)
xbonacci(List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 20) == List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 4, 8, 16, 32, 64, 128, 256)
