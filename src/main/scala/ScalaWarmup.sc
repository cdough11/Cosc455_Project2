//Question1
def prime(n : Int) : Boolean = {
  if (n <= 1)
    false
  else if (n == 2)
    true
  else
    !(2 until (n-1)).exists(x => n % x == 0)
}
prime(7)
prime(4)

//////////////////////////////////////////////////////////
//Question2
def twinprimes(x : Int, y : Int) : AnyVal = {
  if(prime(x) && prime(y)){
    val max = math.max(x, y)
    val min = math.min(x, y)
    if(max - 2 == min)
      true
    else false
  }
}
twinprimes(41, 43)
twinprimes(43, 47)

//////////////////////////////////////////////////////////
//Question3
def twinprimelist(n : Int) : List[Int] ={
  n match{
    case x if x < 3 => Nil
    case _ => if(twinprimes(n-2, n) == true || twinprimes(n+2, n) == true) n::twinprimelist(n-1)
    else twinprimelist(n-1)
  }
}
twinprimelist(50).reverse

//////////////////////////////////////////////////////////
//Question4
def goldbach(n : Int) : Unit ={
  if(n % 2 != 0)
    println("Requires an even number")
  else if(n < 3)
    println("Number is not greater than 2")
  else
    goldbachAnalysis(1, n)
}

def goldbachAnalysis(x: Int, y: Int) : Unit ={
  if ((x + y - x == y) && prime(x) && prime(y - x))
    println("Goldbach sequence for " + y + " is " + x + " + " + (y-x))
  else if(x < y)
    goldbachAnalysis(x + 1, y)
}
goldbach(28)
goldbach(13)
goldbach(2)
goldbach(62)