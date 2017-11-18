import scala.collection.mutable.ListBuffer

def prime(a: Int): Boolean = {
  !((2 until (a - 1)) exists (a % _ == 0))
}

def twinprime(a: Int, b: Int): Boolean = {
  a match {
    case _ if Math.abs(a - b) == 2 => prime(a) && prime(b)
    case _ => false
  }
}

def twinprimeslist(n: Int): List[Int] = {
  var a = ListBuffer[Int]()
  (4 until (n)).map(x => if (prime(x) && prime(x - 2)) {
    a += (x - 2)
    a += x
  })
  var b = a.toList
  b.distinct
}

def goldbach(n: Int) = {
  n match {
    case _ if (n <= 2) || (n % 2 != 0) => println("ERROR: Integer has to be bigger than 2 and even")
    case _ => (3 until n).map(x => if (prime(x) && prime(n - x)) println(x + " + "+ (n-x) + " = " + n))
    }
}
var a = 41
var b = 43

prime(a)
prime(b)
prime(42)
twinprime(a, b)
twinprime(b, a)
twinprime(43, 47)
twinprimeslist(50)
goldbach(26)