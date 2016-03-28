C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)quindf.f	1.2	 7/20/92
C
      subroutine quindf(n, x, y, b, c, d, e, f)
c
      integer n
      real x(n), y(n), b(n), c(n), d(n), e(n), f(n)
c
c
c
c     quindf computes the coefficients of a quintic natural quintic spli
c     s(x) with knots x(i) for which the function values y(i) and
c     the first derivatives b(i) are specified at x(i), i = 1,2, ...,n.
c     in each interval (x(i),x(i+1)) the spline function s(xx) is
c     a polynomial of fifth degree:
c     s(xx)=((((f(i)*p+e(i))*p+d(i))*p+c(i))*p+b(i))*p+y(i)    (*)
c     where  p = xx - x(i).
c
c        input:
c
c     n          number of data points, (at least two, i.e. n > 1)
c     x(1:n)     the strictly increasing or decreasing sequence of
c                knots.  the spacing must be such that the fifth
c                power of x(i+1) - x(i) can be formed without
c                overflow or underflow of exponents
c     y(1:n)     the prescribed function values at the knots
c     b(1:n)     the prescribed derivative values at the knots
c
c        output:
c
c     c,d,e,f    the computed spline coefficients as in (*).
c         (1:n)  e(n) and f(n) are neither used nor altered.
c                the arrays c,d,e,f must always be distinct.
c
      integer i, m, n1
      real cc, g, h, hh, h2, p, pp, q, qq, r, rr
c
      if (n.le.1) go to 40
      n1 = n - 1
      cc = 0.
      hh = 0.
      pp = 0.
      qq = 0.
      rr = 0.
      g = 0.
      do 10 i=1,n1
        h = 1./(x(i+1)-x(i))
        h2 = h*h
        d(i) = 3.*(hh+h) - g*hh
        p = (y(i+1)-y(i))*h2*h
        q = (b(i+1)+b(i))*h2
        r = (b(i+1)-b(i))*h2
        cc = 10.*(p-pp) - 5.*(q-qq) + r + rr + g*cc
        c(i) = cc
        g = h/d(i)
        hh = h
        pp = p
        qq = q
        rr = r
   10 continue
c
      c(n) = (-10.*pp+5.*qq+rr+g*cc)/(3.*hh-g*hh)
      i = n
      do 20 m=1,n1
c        i      = n-1, ..., 1
        i = i - 1
        d(i+1) = 1./(x(i+1)-x(i))
        c(i) = (c(i)+c(i+1)*d(i+1))/d(i)
   20 continue
c
      do 30 i=1,n1
        h = d(i+1)
        p = (((y(i+1)-y(i))*h-b(i))*h-c(i))*h
        q = ((b(i+1)-b(i))*h-c(i)-c(i))*h
        r = (c(i+1)-c(i))*h
        g = q - 3.*p
        rr = r - 3.*(p+g)
        qq = -rr - rr + g
        f(i) = rr*h*h
        e(i) = qq*h
        d(i) = -rr - qq + p
   30 continue
c
      d(n) = 0.
      e(n) = 0.
      f(n) = 0.
   40 return
      end
