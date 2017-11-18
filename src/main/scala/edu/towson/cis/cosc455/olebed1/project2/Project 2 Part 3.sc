val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
val number: List[Int] = List(0,1,2,3,4,5,6,7,8,9,10)


//TODO : it is not the solution as it uses
def go(str : List[String]) ={
  var list = new scala.collection.mutable.Queue[Int]
  str.map(x => {
    if (chinese.contains(x)) {list+=(chinese.indexOf(x,0).toInt)}
    if (english.contains(x)) {list+=(english.indexOf(x,0).toInt)}
  })
  print("Translation: ")
  (0 until list.length).map(x=> print(number(list(x)) + " "))
  var sum = 0
  (0 until list.length).map(x=>sum +=(number(list(x))))
  print("\nAddition: ")
  (0 until list.length -1 ).map(x=> print(number(list(x)) + " + "))
  print(number((list.length)) + " = " + sum)
  sum = 1
  (0 until list.length).map(x=>sum *=(number(list(x))))
  print("\nMulitplication: ")
  (0 until list.length -1 ).map(x=> print(number(list(x)) + " * "))
  print(number(list.length) + " = " + sum + "\n")
}
go(List("yi","nine","six","ba"))
go(List("yi","josh","three","si"))