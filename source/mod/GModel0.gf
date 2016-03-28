/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
	%W%    %G%
*/
/*
 * MODEL 0
 *
 * This is a generic model file for the program GaussFit
 * It is used for the SDE task gfit
 *
 * The model is identical to that used by the SDE modeling suite.
 * It consists of an arbitrary number of components, of the types
 * 'POINT', 'GAUSS', 'RECT', 'DISK'
 * 
 * A point has three free parameters, and the rest have six.  In practice,
 * the Gaussian model should be the dominant one used, since all the others
 * lack derivative information in one axis or another.  All but the most
 * sophisticated or the most crude fitting algorithms will have difficulty
 * with them.
 *
 * Flux   (Jy)        
 * dRa0   (asec)
 * dDec0  (asec)
 * BMaj   (asec)
 * BMin   (asec)
 * BPa    (degrees)
 *
 * Audit trail:
 *	Original version
 *				D.S.Briggs	Apr 13 1992
 */

constant Ncomp, CDELT[AXIS], itype[comp];
parameter Flux[comp], dRA0[comp], dDec0[comp];
parameter BMaj[comp], BMin[comp], BPa[comp];
data dRA, dDec;
observation Img;
variable pi = 3.14159265359;

main() {
  variable Model, rnorm[50], cosPA[50], sinPA[50];
  variable dX, dY, dMaj, dMin, R;
  variable i, r0, fact, diff, tolRA, tolDec;

  fact = 4.0 * log(2.0);
  tolRA = abs(CDELT[1]/2) * 3600;
  tolDec = abs(CDELT[2]/2) * 3600;
  r0 = abs(CDELT[1] * CDELT[2]) * (3600)^2;
  for (i=1; i<=Ncomp; i=i+1) {
    rnorm[i] = r0 / abs(BMaj[i] * BMin[i]);
    if (itype[i] == 2)       /* GAUSS */
      rnorm[i] = rnorm[i] / 1.1331;
    if (itype[i] == 4)       /* DISK */
      rnorm[i] = rnorm[i] * pi / 4;
    cosPA[i] = cos(BPa[i]*pi/180.0);
    sinPA[i] = sin(BPa[i]*pi/180.0);
  }

  while (import()) {

    Model = 0;
    for (i=1; i<=Ncomp; i=i+1) {

      dX = dRA - dRA0[i];
      dY = dDec - dDec0[i];
      if (itype[i] != 1) {   /* !POINT */
        dMaj = dX * sinPA[i] + dY * cosPA[i];
        dMin = -dX * cosPA[i] + dY * sinPA[i];
      }

      if (itype[i] == 1) {
      /* POINT */
        if (abs(dX) <= tolRA)
          if (abs(dY) <= tolDec)
            Model = Model + abs(Flux[i]);
      } else if (itype[i] == 2) {
      /* GAUSS */
        R = (dMaj/BMaj[i])^2 + (dMin/BMin[i])^2;
        Model = Model + abs(Flux[i]) * rnorm[i] * exp(-fact * R);
      } else if (itype[i] == 3) {
      /* RECT */
	if (abs(dMaj < .5 * abs(BMaj[i])))
          if (abs(dMin < .5 * abs(BMin[i])))
            Model = Model + abs(Flux[i]) * rnorm[i];
      } else if (itype[i] == 4) {
      /* DISK */
        R = (dMaj/BMaj[i])^2 + (dMin/BMin[i])^2;
        if (R < .25)
          Model = Model + abs(Flux[i]) * rnorm[i];
      }
    }

    diff = Img - Model;
    export(diff);
  }
}

