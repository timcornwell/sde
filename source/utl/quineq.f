C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)quineq.f	1.2	 7/20/92
C
      subroutine quineq(n, y, b, c, d, e, f)
c
      integer n
      real y(n), b(n), c(n), d(n), e(n), f(n)
c
c
c
c     quineq computes the coefficients of quintic natural quintic spline
c     s(x) with equidistant knots x(i) interpolating there to given
c     function values:
c               s(x(i)) = y(i)  for i = 1,2, ..., n.
c     in each interval (x(i),x(i+1)) the spline function s(xx) is
c     a polynomial of fifth degree:
c     s(xx)=((((f(i)*p+e(i))*p+d(i))*p+c(i))*p+b(i))*p+y(i)    (*)
c          =((((-f(i)*q+e(i+1))*q-d(i+1))*q+c(i+1))*q-b(i+1))*q+y(i+1)
c     where  p = (xx - x(i))/x(i+1) - x(i))
c     and    q = (x(i+1) - xx)/(x(i+1) - x(i)).
c     (note the first subscript in the second expression.)
c     the different polynomials are pieced together so that s(x) and
c     its derivatives up to s"" are continuous.
c
c        input:
c
c     n          number of data points, (at least three, i.e. n > 2)
c     y(1:n)     the prescribed function values at the knots
c
c        output:
c
c     b,c,d,e,f  the computed spline coefficients as in (*).
c         (1:n)  if x(i+1) - x(i) = 1., then specifically:
c                b(i) = s'x(i)), c(i) = s"(x(i))/2, d(i) = s"'(x(i))/6,
c                e(i) = s""(x(i))/24,  f(i) = s""'(x(i)+0)/120.
c                f(n) is neither used nor altered.  the arrays
c                y,b,c,d must always be distinct.  if e and f are
c                not wanted, the call quineq(n,y,b,c,d,d,d) may
c                be used to save storage locations.
c
      integer i, m
      real p, q, r, s, t, u, v
c
      if (n.le.2) go to 50
c
      m = n - 3
      p = 0.
      q = 0.
      r = 0.
      s = 0.
      t = 0.
      d(m+1) = 0.
      d(m+2) = 0.
      if (m.le.0) go to 30
      do 10 i=1,m
        u = p*r
        b(i) = 1./(66.-u*r-q)
        r = 26. - u
        c(i) = r
        d(i) = y(i+3) - 3.*(y(i+2)-y(i+1)) - y(i) - u*s - q*t
        q = p
        p = b(i)
        t = s
        s = d(i)
   10 continue
c
      i = n - 2
      do 20 m=4,n
c        i    = n-3, ..., 1
        i = i - 1
        d(i) = (d(i)-c(i)*d(i+1)-d(i+2))*b(i)
   20 continue
c
   30 m = n - 1
      q = 0.
      r = d(1)
      t = r
      v = r
      do 40 i=2,m
        p = q
        q = r
        r = d(i)
        s = t
        t = p - q - q + r
        f(i) = t
        u = 5.*(-p+q)
        e(i) = u
        d(i) = 10.*(p+q)
        c(i) = 0.5*(y(i+1)+y(i-1)+s-t) - y(i) - u
        b(i) = 0.5*(y(i+1)-y(i-1)-s-t) - d(i)
   40 continue
c
      f(1) = v
      e(1) = 0.
      d(1) = 0.
      c(1) = c(2) - 10.*v
      b(1) = y(2) - y(1) - c(1) - v
      e(n) = 0.
      d(n) = 0.
      c(n) = c(n-1) + 10.*t
      b(n) = y(n) - y(n-1) + c(n) - t
   50 return
      end
