C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)quinat.f	1.2	 7/20/92
C
      subroutine quinat(n, x, y, b, c, d, e, f)
c
      integer n
      real x(n), y(n), b(n), c(n), d(n), e(n), f(n)
c
c
c
c     quinat computes the coefficients of a quintic natural quintic spli
c     s(x) with knots x(i) interpolating there to given function values:
c               s(x(i)) = y(i)  for i = 1,2, ..., n.
c     in each interval (x(i),x(i+1)) the spline function s(xx) is a
c     polynomial of fifth degree:
c     s(xx) = ((((f(i)*p+e(i))*p+d(i))*p+c(i))*p+b(i))*p+y(i)    (*)
c           = ((((-f(i)*q+e(i+1))*q-d(i+1))*q+c(i+1))*q-b(i+1))*q+y(i+1)
c     where  p = xx - x(i)  and  q = x(i+1) - xx.
c     (note the first subscript in the second expression.)
c     the different polynomials are pieced together so that s(x) and
c     its derivatives up to s"" are continuous.
c
c        input:
c
c     n          number of data points, (at least three, i.e. n > 2)
c     x(1:n)     the strictly increasing or decreasing sequence of
c                knots.  the spacing must be such that the fifth power
c                of x(i+1) - x(i) can be formed without overflow or
c                underflow of exponents.
c     y(1:n)     the prescribed function values at the knots.
c
c        output:
c
c     b,c,d,e,f  the computed spline coefficients as in (*).
c         (1:n)  specifically
c                b(i) = s'(x(i)), c(i) = s"(x(i))/2, d(i) = s"'(x(i))/6,
c                e(i) = s""(x(i))/24,  f(i) = s""'(x(i))/120.
c                f(n) is neither used nor altered.  the five arrays
c                b,c,d,e,f must always be distinct.
c
c        option:
c
c     it is possible to specify values for the first and second
c     derivatives of the spline function at arbitrarily many knots.
c     this is done by relaxing the requirement that the sequence of
c     knots be strictly increasing or decreasing.  specifically:
c
c     if x(j) = x(j+1) then s(x(j)) = y(j) and s'(x(j)) = y(j+1),
c     if x(j) = x(j+1) = x(j+2) then in addition s"(x(j)) = y(j+2).
c
c     note that s""(x) is discontinuous at a double knot and, in
c     addition, s"'(x) is discontinuous at a triple knot.  the
c     subroutine assigns y(i) to y(i+1) in these cases and also to
c     y(i+2) at a triple knot.  the representation (*) remains
c     valid in each open interval (x(i),x(i+1)).  at a double knot,
c     x(j) = x(j+1), the output coefficients have the following values:
c       y(j) = s(x(j))          = y(j+1)
c       b(j) = s'(x(j))         = b(j+1)
c       c(j) = s"(x(j))/2       = c(j+1)
c       d(j) = s"'(x(j))/6      = d(j+1)
c       e(j) = s""(x(j)-0)/24     e(j+1) = s""(x(j)+0)/24
c       f(j) = s""'(x(j)-0)/120   f(j+1) = s""'(x(j)+0)/120
c     at a triple knot, x(j) = x(j+1) = x(j+2), the output
c     coefficients have the following values:
c       y(j) = s(x(j))         = y(j+1)    = y(j+2)
c       b(j) = s'(x(j))        = b(j+1)    = b(j+2)
c       c(j) = s"(x(j))/2      = c(j+1)    = c(j+2)
c       d(j) = s"'((x(j)-0)/6    d(j+1) = 0  d(j+2) = s"'(x(j)+0)/6
c       e(j) = s""(x(j)-0)/24    e(j+1) = 0  e(j+2) = s""(x(j)+0)/24
c       f(j) = s""'(x(j)-0)/120  f(j+1) = 0  f(j+2) = s""'(x(j)+0)/120
c
      integer i, m
      real b1, p, pq, pqqr, pr, p2, p3, q, qr, q2, q3, r, r2, s, t, u, v
c
      if (n.le.2) go to 190
c
c     coefficients of a positive definite, pentadiagonal matrix,
c     stored in d,e,f from 2 to n-2.
c
      m = n - 2
      q = x(2) - x(1)
      r = x(3) - x(2)
      q2 = q*q
      r2 = r*r
      qr = q + r
      d(1) = 0.
      e(1) = 0.
      d(2) = 0.
      if (q.ne.0.) d(2) = 6.*q*q2/(qr*qr)
c
      if (m.lt.2) go to 40
      do 30 i=2,m
        p = q
        q = r
        r = x(i+2) - x(i+1)
        p2 = q2
        q2 = r2
        r2 = r*r
        pq = qr
        qr = q + r
        if (q) 20, 10, 20
   10   d(i+1) = 0.
        e(i) = 0.
        f(i-1) = 0.
        go to 30
   20   q3 = q2*q
        pr = p*r
        pqqr = pq*qr
        d(i+1) = 6.*q3/(qr*qr)
        d(i) = d(i) + (q+q)*(15.*pr*pr+(p+r)*q*(20.*pr+7.*q2)+q2*(8.*
     *   (p2+r2)+21.*pr+q2+q2))/(pqqr*pqqr)
        d(i-1) = d(i-1) + 6.*q3/(pq*pq)
        e(i) = q2*(p*qr+3.*pq*(qr+r+r))/(pqqr*qr)
        e(i-1) = e(i-1) + q2*(r*pq+3.*qr*(pq+p+p))/(pqqr*pq)
        f(i-1) = q3/pqqr
   30 continue
c
   40 if (r.ne.0.) d(m) = d(m) + 6.*r*r2/(qr*qr)
c
c     first and second order divided differences of the given function
c     values, stored in b from 2 to n and in c from 3 to n
c     respectively. care is taken of double and triple knots.
c
      do 60 i=2,n
        if (x(i).ne.x(i-1)) go to 50
        b(i) = y(i)
        y(i) = y(i-1)
        go to 60
   50   b(i) = (y(i)-y(i-1))/(x(i)-x(i-1))
   60 continue
      do 80 i=3,n
        if (x(i).ne.x(i-2)) go to 70
        c(i) = b(i)*0.5
        b(i) = b(i-1)
        go to 80
   70   c(i) = (b(i)-b(i-1))/(x(i)-x(i-2))
   80 continue
c
c     solve the linear system with c(i+2) - c(i+1) as right-hand side.
c
      if (m.lt.2) go to 100
      p = 0.
      c(1) = 0.
      e(m) = 0.
      f(1) = 0.
      f(m-1) = 0.
      f(m) = 0.
      c(2) = c(4) - c(3)
      d(2) = 1./d(2)
c
      if (m.lt.3) go to 100
      do 90 i=3,m
        q = d(i-1)*e(i-1)
        d(i) = 1./(d(i)-p*f(i-2)-q*e(i-1))
        e(i) = e(i) - q*f(i-1)
        c(i) = c(i+2) - c(i+1) - p*c(i-2) - q*c(i-1)
        p = d(i-1)*f(i-1)
   90 continue
c
  100 i = n - 1
      c(n-1) = 0.
      c(n) = 0.
      if (n.lt.4) go to 120
      do 110 m=4,n
c        i = n-2, ..., 2
        i = i - 1
        c(i) = (c(i)-e(i)*c(i+1)-f(i)*c(i+2))*d(i)
  110 continue
c
c     integrate the third derivative of s(x).
c
  120 m = n - 1
      q = x(2) - x(1)
      r = x(3) - x(2)
      b1 = b(2)
      q3 = q*q*q
      qr = q + r
      if (qr) 140, 130, 140
  130 v = 0.
      t = 0.
      go to 150
  140 v = c(2)/qr
      t = v
  150 f(1) = 0.
      if (q.ne.0.) f(1) = v/q
      do 180 i=2,m
        p = q
        q = r
        r = 0.
        if (i.ne.m) r = x(i+2) - x(i+1)
        p3 = q3
        q3 = q*q*q
        pq = qr
        qr = q + r
        s = t
        t = 0.
        if (qr.ne.0.) t = (c(i+1)-c(i))/qr
        u = v
        v = t - s
        if (pq) 170, 160, 170
  160   c(i) = c(i-1)
        d(i) = 0.
        e(i) = 0.
        f(i) = 0.
        go to 180
  170   f(i) = f(i-1)
        if (q.ne.0.) f(i) = v/q
        e(i) = 5.*s
        d(i) = 10.*(c(i)-q*s)
        c(i) = d(i)*(p-q) + (b(i+1)-b(i)+(u-e(i))*p3-(v+e(i))*q3)/pq
        b(i) = (p*(b(i+1)-v*q3)+q*(b(i)-u*p3))/pq -
     *   p*q*(d(i)+e(i)*(q-p))
  180 continue
c
c     end points x(1) and x(n).
c
      p = x(2) - x(1)
      s = f(1)*p*p*p
      e(1) = 0.
      d(1) = 0.
      c(1) = c(2) - 10.*s
      b(1) = b1 - (c(1)+s)*p
c
      q = x(n) - x(n-1)
      t = f(n-1)*q*q*q
      e(n) = 0.
      d(n) = 0.
      c(n) = c(n-1) + 10.*t
      b(n) = b(n) + (c(n)-t)*q
  190 return
      end
