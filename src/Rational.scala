/**
	* Created by soumyaka on 2/10/2017.
	*/
class Rational(numer: Int, denom: Int) {
	require(denom != 0, "Denominator cannot be zero!".toUpperCase)

	private val g = gcd(numer.abs, denom.abs)

	private val numerator = numer / g
	private val denominator = denom / g

	def this(numer: Int) = this(numer, 1)

	override def toString: String = {
		if (denominator == 1) numerator.toString else numerator + "/" + denominator
	}

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
			this.numerator - this.denominator * num,
			this.denominator
		)
	}

	def *(num: Int): Rational = {
		new Rational(
			this.numerator * num,
			this.denominator
		)
	}

	def /(that: Rational): Rational = {
		this * new Rational(that.denominator, that.numerator)
	}

	def *(that: Rational): Rational = {
		new Rational(
			this.numerator * that.numerator,
			this.denominator * that.denominator
		)
	}

	def /(num: Int): Rational = {
		this * new Rational(1, num)
	}

	def max(that: Rational): Rational = {
		if (this.lessThan(that)) that else this
	}

	def lessThan(that: Rational): Boolean = {
		this.numerator * that.denominator < that.numerator * this.denominator
	}

	private def gcd(a: Int, b: Int): Int = {
		if (b == 0) a else gcd(b, a % b)
	}
}
