import scala.math._

abstract class Regressions(data: IndexedSeq[Seq[Double]]) {

  val n = this.data.size.toDouble

  def meanX: Double = this.sumX / this.n

  def meanY: Double = this.sumY / this.n

  def sumX: Double = {
    var sum = 0.0
    for (pair <- data)
      sum += pair.head
    sum
  }

  def sumY: Double = {
    var sum = 0.0
    for (pair <- data)
      sum += pair.last
    sum
  }

  def sumXSquared: Double = {
    var res = 0.0
    for (pair <- data)
      res += pow(pair.head, 2)
    res
  }

  def sumYSquared: Double = {
    var res = 0.0
    for (pair <- data)
      res += pow(pair.last, 2)
    res
  }

  def sumXY: Double = {
    var res = 0.0
    for (pair <- data)
      res += pair.head * pair.last
    res
  }
}

class LinearRegression(data: IndexedSeq[Seq[Double]]) extends Regressions(data) {

  /// linear regression: y = a * x + b

  def r: Double = {
    var r = (this.n * this.sumXY - this.sumX*this.sumY)/(sqrt(this.n * this.sumXSquared - pow(this.sumX, 2)) * sqrt(this.n * this.sumYSquared - pow(this.sumY, 2)))
    r
  }

  def sdX: Double = {
    var varianceNoN = 0.0
    for (pair <- data)
      varianceNoN += pow(pair.head - this.meanX, 2)
    var sdX = sqrt(varianceNoN/(this.n - 1))
    sdX
  }

  def sdY: Double = {
    var varianceNoN = 0.0
    for (pair <- data)
      varianceNoN += pow(pair.last - this.meanY, 2)
    var sdY = sqrt(varianceNoN/(this.n - 1))
    sdY
  }

  val a = this.r * sdY / sdX

  val b = this.meanY - this.a * this.meanX

  val rSquared = pow(this.r, 2)

}

class QuadraticRegression(data: IndexedSeq[Seq[Double]]) extends Regressions(data){

/// quadratic regression: y = a * x^2 + b * x + c

def sumXCube: Double = {
  var sum = 0.0
  for (pair <- data)
    sum += pow(pair.head, 3)
  sum
}

def sumXTo4thPower: Double = {
  var sum = 0.0
  for (pair <- data)
    sum += pow(pair.head, 4)
  sum
}

def sumXSquaredY: Double = {
  var sum = 0.0
  for (pair <- data)
    sum += pow(pair.head, 2)*pair.last
  sum
}

def sigmaXX: Double = this.sumXSquared - (pow(this.sumX, 2)/this.n)

def sigmaXY: Double = this.sumXY - ((this.sumX * this.sumY)/n)

def sigmaXXSquared: Double = this.sumXCube - ((this.sumXSquared * this.sumX)/n)

def sigmaXSquaredY: Double = this.sumXSquaredY - ((this.sumXSquared * this.sumY)/n)

def sigmaXSquaredXSquared: Double = this.sumXTo4thPower - (pow(this.sumXSquared, 2)/n)

val a = (this.sigmaXSquaredY * this.sigmaXX - this.sigmaXY * this.sigmaXXSquared)/(this.sigmaXX * this.sigmaXSquaredXSquared - pow(this.sigmaXXSquared, 2))

val b = (this.sigmaXY * this.sigmaXSquaredXSquared - this.sigmaXSquaredY * this.sigmaXXSquared)/(this.sigmaXX * this.sigmaXSquaredXSquared - pow(this.sigmaXXSquared, 2))

val c = (this.sumY/n) - b * (this.sumX/n) - a * (this.sumXSquared/n)

private def totalSumOfSquares: Double = {
  var sum = 0.0
  for (pair <- data)
    sum += pow(pair.last - this.meanY, 2)
  sum
}

private def yhat(x: Double): Double = a * pow(x, 2) + b * x + c

private def sumOfSquaresOfResiduals: Double = {
  var sum = 0.0
  for (pair <- data)
    sum += pow(pair.last - this.yhat(pair.head), 2)
  sum
}

val rSquared = 1 - (this.sumOfSquaresOfResiduals/this.totalSumOfSquares)
}