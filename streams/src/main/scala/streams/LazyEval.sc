// Prime numbers
def primes(nums: LazyList[Int]): LazyList[Int] =
  nums.head #:: primes(nums.tail).filterNot(_ % nums.head == 0)

def numsFrom(from: Int): LazyList[Int] = from #:: numsFrom(from + 1)

primes(numsFrom(2)).take(10).toList


extension (list: LazyList[Int])
  def add(other: LazyList[Int]): LazyList[Int] =
    (list.head + other.head) #:: list.tail.add(other.tail)

// Fibonacci sequence
def fibonacci: LazyList[Int] =
  0 #:: 1 #:: fibonacci.add(fibonacci.tail)

fibonacci.take(10).toList

