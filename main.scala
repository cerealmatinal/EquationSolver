import scala.math._

object ComplexEquationSolver {

  def main(args: Array[String]): Unit = {
    val equation = "(x + 2i) ^ 3 - (4x + 1 + 5i)"
    val solution = solve(equation)
    println(s"Solution for $equation is $solution")
  }

  def solve(equation: String): String = {
    val parsedEquation = parseEquation(equation)
    val coefficients = getCoefficients(parsedEquation)
    val roots = getRoots(coefficients)
    roots.map(formatComplexNumber).mkString(", ")
  }

  def parseEquation(equation: String): List[String] = {
    val regex = "([+-]?[0-9]*\\.?[0-9]*[i]?)".r
    regex.findAllMatchIn(equation).map(_.group(1)).toList
  }

  def getCoefficients(equationParts: List[String]): List[Complex] = {
    equationParts.map(part => parseComplexNumber(part.replaceAll("\\s", "")))
  }

  def getRoots(coefficients: List[Complex]): List[Complex] = {
    val n = coefficients.length - 1
    val a = Array.ofDim[Complex](n + 1, n + 1)
    val b = Array.ofDim[Complex](n + 1)
    for (i <- 0 to n) {
      for (j <- 0 to n) {
        a(i)(j) = if (j == n) coefficients(n - i) else Complex.zero
      }
    }
    a(0)(0) = Complex.one
    for (i <- 1 to n) {
      a(i)(i - 1) = coefficients(n)
      a(i)(i) = Complex.one
    }
    b(n) = coefficients(0)
    for (i <- 0 until n) {
      b(i) = Complex.zero
    }
    val roots = solveLinearSystem(a, b)
    roots
  }

  def solveLinearSystem(a: Array[Array[Complex]], b: Array[Complex]): List[Complex] = {
    val n = b.length
    for (p <- 0 until n) {
      var max = p
      for (i <- p + 1 until n) {
        if (a(i)(p).abs > a(max)(p).abs) max = i
      }
      val temp = a(p)
      a(p) = a(max)
      a(max) = temp
      val tempB = b(p)
      b(p) = b(max)
      b(max) = tempB
      if (a(p)(p) == Complex.zero) {
        throw new IllegalArgumentException("Singular matrix")
      }
      for (i <- p + 1 until n) {
        val alpha = a(i)(p) / a(p)(p)
        b(i) -= alpha * b(p)
        for (j <- p until n) {
          a(i)(j) -= alpha * a(p)(j)
        }
      }
    }
    val x = Array.ofDim[Complex](n)
    for (i <- n - 1 to 0 by -1) {
      var sum = Complex.zero
      for (j <- i + 1 until n) {
        sum += a(i)(j) * x(j)
      }
      x(i) = (b(i) - sum) / a(i)(i)
    }
      sum += a(i)(j) * x(j)
      x(i) = (b(i) - sum) / a(i)(i)
    }
    x.toList
  }

  def formatComplexNumber(z: Complex): String = {
    val realPart = if (z.real == 0) "" else "%.2f".format(z.real)
    val sign = if (z.imaginary >= 0) "+" else "-"
    val imaginaryPart = if (z.imaginary == 0) "" else "%.2fi".format(z.imaginary.abs)
    realPart + sign + imaginaryPart
  }

  def parseComplexNumber(number: String): Complex = {
    if (number.endsWith("i")) {
      val realPart = if (number.head == '+') "0" else number.init
      val imaginaryPart = if (number.head == '+' || number.head == '-') number.tail.init else number.init
      Complex(realPart.toDouble, imaginaryPart.toDouble)
    } else {
      Complex(number.toDouble)
    }
  }
}
