package pp202002.project.impl

object Prime {
  // Problem 4: implement nthPrime.
  // nthPrime should return a function that calculates n-th prime number.
  val nthPrime: String =
    """
      |(let ( (def nthprime (n) (match (== n 1) (app primeIter (n 3)) (2)))  (def isPrime (x) (app isPrimeIter (x 3))) (def isPrimeIter (x n) (match (> x n)  ((inr 0) ((match (== n (% x n)) ((_ 0) (app isPrimeIter(x (+ n 1))))    ))) )) (def primeIter (n x) (match (isPrime(x)) ((_ (app primeIter (n (+ x 2)))) (_ (match (n > 0) (_ x) (_ (app primeIter ((- n 1) (+ x 2))))) )))       )   )         nthprime)
      |""".stripMargin
}