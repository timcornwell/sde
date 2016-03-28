C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fred.f	1.2    5/13/91
C
C Code for simulation courtesy FRS
C
C	Calculates Paralactic Angle
C				M.A.Holdaway	circa Spring 1991
C	Fix DOMAIN ERROR in ATAN2 by checking if Z ~ 0
C				M.A.Holdaway	May 12 1991	
C
      subroutine GETANGLS(h,delta,slat,az,el,parangle)
c  Given the hour-angle h, the source declination delta, and the 
c  geographic latitude, slat, of the site, this subroutine computes
c  the azimuth, the elevation, and the parallactic angle.
      implicit real*8 (a-h,o-z), integer*4 (i-n)
      pi=4d0*atan(1d0)
      z=acos(sin(delta)*sin(slat)+cos(delta)*cos(slat)*cos(h))
      el=pi/2d0-z
      if (abs(z).lt.1.D-7) then
         parangle=0d0
         az=0d0
      else
         sinq=sin(h)*cos(slat)/sin(z)
         cosq=sin(slat)/sin(z)/cos(delta)-tan(delta)/tan(z)
         parangle=atan2(sinq,cosq)
         sina=-cos(delta)*sin(h)/sin(z)
         cosa=(sin(delta)*cos(slat)-cos(delta)*cos(h)*sin(slat))/sin(z)
         az=atan2(sina,cosa)
         if (az.lt.0d0) az=az+2d0*pi
      end if
      return
      end

      subroutine GETANGL2 (az, el, slat, h, delta)
c Given az, el, and slat, we can find h, delta
c Opposite of  GETANGLS
c Note: angles in radians
      double precision	az, el, slat, h, delta
      double precision	hcos, hsin, z, pi
c
      pi=4d0*atan(1d0)
      z = pi/2.D0 - el
      delta = asin (cos(z)*sin(slat) + sin(z) * cos(slat)*cos(az))
      if (az .eq. pi) then
            h = 0.D0
      else
         hcos  =  cos(z)*cos(slat) - sin(z)*cos(az)*sin(slat)
         hsin  = -  sin(az) * sin(z) 
         h = atan2 (hsin, hcos)
      endif
      return
      end

      subroutine blockage(i,j,uant,vant,want,diam,fi,fj)
c  Subroutine to compute geometric shadowing.
c  Given i and j, antenna-based (u,v,w), and the antenna diameters,
c  this subroutine computes the fractional blockage fi of antenna i by
c  antenna j, and the fractional blockage fj of antenna j by antenna i.  
c  Whenever blockage occurs, if w<0 then it is the case that antenna j 
c  is blocked by antenna i, if w>0 then antenna i has been blocked by
c  antenna j, and if w=0 then the antennas have run into each other.
c  So generally, on return, one - but not both - of fi and fj may be
c  nonzero.
c  The antenna profiles are assumed to be circular, and the antennas
c  are assumed to be pointed the same way (a reasonably valid
c  assumption for the case of a compact array configuration, which 
c  is the only case where one generally worries about shadowing).
      implicit real*8 (a-h,o-z), integer*4 (i-n)
      real*8 uant(*),vant(*),want(*),diam(*)
      pi=4d0*atan(1d0)
      u=uant(j)-uant(i)
      v=vant(j)-vant(i)
      w=want(j)-want(i)
      SEPARATN=sqrt(u**2+v**2)
      rmin=.5d0*min(abs(diam(i)),abs(diam(j)))
      rmax=.5d0*max(abs(diam(i)),abs(diam(j)))
      if (SEPARATN.ge.rmin+rmax) then
         fi=0d0
         fj=0d0
      else if (SEPARATN+rmin.le.rmax) then
         fi=min(1d0,(abs(diam(j))/abs(diam(i)))**2)
         fj=min(1d0,(abs(diam(i))/abs(diam(j)))**2)
      else
         c=SEPARATN/(.5d0*abs(diam(i)))
         s=abs(diam(j))/abs(diam(i))        
         sinb=sqrt(2d0*((c*s)**2+c**2+s**2)-c**4-s**4-1d0)/(2d0*c)
c  Or can use the factored form:
c        sinb=sqrt((s+c+1d0)*(s+c-1d0)*(s-c+1d0)*(c-s+1d0))/(2d0*c)
c
         sina=sinb/s
c  Due to roundoff, sina or sinb might be ever so slightly larger than 1
c  in the case of unequal radii, with the center of one antenna pattern
c  inside the other:
         sinb=min(1d0,sinb)
         sina=min(1d0,sina)
c
         b=asin(sinb)
         a=asin(sina)
         area=(s**2*a+b)-(s**2*sina*cos(a)+sinb*cos(b))
         fi=area/pi
         fj=fi/s**2
      end if
      if (w.lt.0d0) fi=0d0
      if (w.gt.0d0) fj=0d0            
      return
      end



      subroutine uvant(h,delta,n,lx,ly,lz,uant,vant,want)
c  Given the hour-angle h, the source declination delta, the number
c  of elements n, and the element locations (lx(i),ly(i),lz(i)), 
c  i=1,...,n, this subroutine computes the antenna-based spatial
c  frequency coordinates (uant(i),vant(i),want(i)), i=1,...,n.
      implicit real*8 (a-h,o-z), integer*4 (i-n)
      real*8 lx(*),ly(*),lz(*),uant(*),vant(*),want(*)
      sh=sin(h)
      ch=cos(h)
      sd=sin(delta)
      cd=cos(delta)
      do 10 i=1,n
         uant(i)=sh*lx(i)+ch*ly(i)
         vant(i)=-sd*(ch*lx(i)-sh*ly(i))+cd*lz(i)
         want(i)=cd*(ch*lx(i)-sh*ly(i))+sd*lz(i)
  10  continue
      return
      end

