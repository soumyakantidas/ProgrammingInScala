/**
	* Created by soumyaka on 2/10/2017.
	*/
class Rational(numer: Int, denom: Int) {
	require(denom != 0, "Denominator cannot be zero!".toUpperCase)

	val g = gcd(numer.abs, denom.abs)

	val numerator = numer/g
	val denominator = denom/g

	def this(numer: Int) = this(numer, 1)

	override def toString: String = numerator + "/" + denominator

	def +(that: Rational): Rational = {
		new Rational(
			numerator * that.denominator + denominator * that.numerator,
			denominator * that.denominator
		)
	}

	def +(num: Int): Rational = {
		new Rational(
			this.numerator + this.denominator * num,
			this.denominator
		)
	}

	def -(that: Rational): Rational = {
		new Rational(
			numerator * that.denominator - denominator * that.numerator,
			denominator * that.denominator
		)
	}

	def -(num: Int): Rational = {
		new Rational(
			this.numerator + this.denominator * num,
			this.denominator
		)
	}

	def *(that: Rational): Rational = {
		new Rational(
			this.numerator * that.numerator,
			this.denominator * that.denominator
		)
	}

	def lessThan(that: Rational): Boolean = {
		this.numerator * that.denominator < that.numerator * this.denominator
	}

	def max(that: Rational): Rational = {
		if (this.lessThan(that)) that else this
	}

	private def gcd(a: Int, b: Int): Int = {
		if (b == 0) a else gcd(b, a % b)
	}
}
