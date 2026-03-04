import com.badlogic.gdx.math.Matrix4

object LorentzPolar {

  // ---------- low-level row-major helpers (4x4) ----------
  private def toRowMajor(m: Matrix4): Array[Double] = {
    // Matrix4.val is column-major: element (r,c) stored at val[c*4 + r]
    val v = m.`val`
    val out = Array.ofDim[Double](16)
    var r = 0
    while (r < 4) {
      var c = 0
      while (c < 4) {
        out(r*4 + c) = v(c*4 + r).toDouble
        c += 1
      }
      r += 1
    }
    out
  }

  private def fromRowMajor(row: Array[Double]): Matrix4 = {
    val m = new Matrix4()
    val v = m.`val`
    var r = 0
    while (r < 4) {
      var c = 0
      while (c < 4) {
        v(c*4 + r) = row(r*4 + c).toFloat
        c += 1
      }
      r += 1
    }
    m
  }

  private def matMul(a: Array[Double], b: Array[Double]): Array[Double] = {
    val c = Array.ofDim[Double](16)
    var r = 0
    while (r < 4) {
      var cc = 0
      while (cc < 4) {
        var sum = 0.0
        var k = 0
        while (k < 4) {
          sum += a(r*4 + k) * b(k*4 + cc)
          k += 1
        }
        c(r*4 + cc) = sum
        cc += 1
      }
      r += 1
    }
    c
  }

  private def matAdd(a: Array[Double], b: Array[Double]): Array[Double] = {
    val c = Array.ofDim[Double](16)
    var i = 0
    while (i < 16) { c(i) = a(i) + b(i); i += 1 }
    c
  }

  private def matSub(a: Array[Double], b: Array[Double]): Array[Double] = {
    val c = Array.ofDim[Double](16)
    var i = 0
    while (i < 16) { c(i) = a(i) - b(i); i += 1 }
    c
  }

  private def matScale(a: Array[Double], s: Double): Array[Double] = {
    val c = Array.ofDim[Double](16)
    var i = 0
    while (i < 16) { c(i) = a(i) * s; i += 1 }
    c
  }

  private def matIdentity(): Array[Double] = {
    val I = Array.ofDim[Double](16)
    var r = 0
    while (r < 4) { I(r*4 + r) = 1.0; r += 1 }
    I
  }

  private def matTranspose(a: Array[Double]): Array[Double] = {
    val t = Array.ofDim[Double](16)
    var r = 0
    while (r < 4) {
      var c = 0
      while (c < 4) {
        t(r*4 + c) = a(c*4 + r)
        c += 1
      }
      r += 1
    }
    t
  }

  private def frobeniusNormDiff(a: Array[Double], b: Array[Double]): Double = {
    var s = 0.0
    var i = 0
    while (i < 16) { val d = a(i) - b(i); s += d*d; i += 1 }
    Math.sqrt(s)
  }

  // ---------- G matrix for time-last: diag(1,1,1,-1) in row-major ----------
  private val Grow: Array[Double] = {
    val g = Array.ofDim[Double](16)
    g(0*4 + 0) = 1.0
    g(1*4 + 1) = 1.0
    g(2*4 + 2) = 1.0
    g(3*4 + 3) = -1.0
    g
  }

  // ---------- compute A = G * M^T * G * M  ----------
  private def buildArow(Mrow: Array[Double]): Array[Double] = {
    val Mt = matTranspose(Mrow)        // M^T (row-major)
    val tmp1 = matMul(Grow, Mt)       // G * M^T
    val tmp2 = matMul(tmp1, Grow)     // G * M^T * G
    val A = matMul(tmp2, Mrow)        // G * M^T * G * M
    A
  }

  // ---------- Denman–Beavers style inverse sqrt iteration ----------
  // returns (A^{-1/2}, successFlag)
  private def inverseSqrtDB(A: Array[Double], maxIter: Int = 30, tol: Double = 1e-12): (Array[Double], Boolean) = {
    // Y0 = I, Z0 = A
    var Y = matIdentity()
    var Z = A.clone()

    var iter = 0
    var converged = false
    while (iter < maxIter && !converged) {
      // compute B = 0.5 * (3I - Z * Y)
      val ZY = matMul(Z, Y)
      val threeI = matScale(matIdentity(), 3.0)
      val inside = matSub(threeI, ZY)
      val B = matScale(inside, 0.5)

      // Y_{k+1} = Y * B
      val Ynew = matMul(Y, B)
      // Z_{k+1} = B * Z
      val Znew = matMul(B, Z)

      // check residual: ||Ynew * Ynew * A - I||  (since Y ≈ A^{-1/2}, Y*Y*A ≈ I)
      val YY = matMul(Ynew, Ynew)
      val YYA = matMul(YY, A)
      val res = frobeniusNormDiff(YYA, matIdentity())

      Y = Ynew
      Z = Znew

      if (res < tol) converged = true
      iter += 1
    }
    (Y, converged)
  }

  // ---------- first-order approx A^{-1/2} ≈ I - 0.5*(A - I) ----------
  private def approxInverseSqrtFirstOrder(A: Array[Double]): Array[Double] = {
    val I = matIdentity()
    val diff = matSub(A, I)
    matSub(I, matScale(diff, 0.5))
  }

  // ---------- public API: polar projection ----------
  /** Returns the Lorentz-projected matrix L = M * (G M^T G M)^{-1/2}
    * If quickOnly==true uses only first-order approximation; otherwise attempts Newton iteration.
    */
  def polarProject(M: Matrix4, maxIter: Int = 30, tol: Double = 1e-10, quickOnly: Boolean = false): Matrix4 = {
    val Mrow = toRowMajor(M)
    val Arow = buildArow(Mrow)
    val I = matIdentity()
    val err = frobeniusNormDiff(Arow, I)

    // threshold to decide quick linear approx
    val threshold = 1e-3

    val AinvSqrtRow: Array[Double] =
      if (quickOnly || err < threshold) {
        // first-order approximate
        approxInverseSqrtFirstOrder(Arow)
      } else {
        // attempt iterative inverse sqrt
        val (y, ok) = inverseSqrtDB(Arow, maxIter = maxIter, tol = tol)
        if (ok) y else {
          // fallback to a couple Newton-style refinement steps beginning from first-order approx
          var approx = approxInverseSqrtFirstOrder(Arow)
          var it = 0
          var succ = false
          while (it < maxIter && !succ) {
            // refine: perform one Denman-Beavers iteration seeded with Y0 = approx, Z0 = A
            val Y0 = approx
            val Z0 = Arow
            val ZY = matMul(Z0, Y0)
            val B = matScale(matSub(matScale(matIdentity(), 3.0), ZY), 0.5)
            val Y1 = matMul(Y0, B)
            val YY = matMul(Y1, Y1)
            val res = frobeniusNormDiff(matMul(YY, Arow), matIdentity())
            approx = Y1
            if (res < tol) succ = true
            it += 1
          }
          approx
        }
      }

    // compute Lrow = Mrow * AinvSqrtRow
    val Lrow = matMul(Mrow, AinvSqrtRow)
    fromRowMajor(Lrow)
  }

  /** Convenience: returns Frobenius deviation || G M^T G M - I || */
  def lorentzDeviation(M: Matrix4): Double = {
    val Mrow = toRowMajor(M)
    val Arow = buildArow(Mrow)
    frobeniusNormDiff(Arow, matIdentity())
  }
}