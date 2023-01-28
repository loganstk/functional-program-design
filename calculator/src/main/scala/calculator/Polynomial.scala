package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal(Math.pow(b(), 2) - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      delta() match {
        case d if d < 0 => Set()
        case d if d == 0 => Set(-b() / (2 * a()))
        case _ => {
          val aVal = a()
          val bVal = b()
          val sqrtDelta = Math.sqrt(delta.currentValue)
          Set(
            (-bVal + sqrtDelta) / (2 * aVal),
            (-bVal - sqrtDelta) / (2 * aVal)
          )
        }
      }
    }
