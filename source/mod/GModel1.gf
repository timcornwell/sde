/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
	%W%    %G%
*/
/*
 * MODEL 1
 *
 * This is a model file for the program GaussFit
 * It is used for the SDE task gfit
 *
 * The model consists of N Gaussians, each with 1 free parameter and five
 * fixed parameter.  This is used for an initial linear fit to generate
 * a starting guess for the full non-linear fit in all parameters.
 *
 * Flux   (Jy)		(free)
 * dRa0   (asec)	(fixed)
 * dDec0  (asec)	(fixed)
 * BMaj   (asec)	(fixed)
 * BMin   (asec)	(fixed)
 * BPa    (degrees)	(fixed)
 *
 * Audit trail:
 *	Original version
 *				D.S.Briggs	Apr 13 1992
 */

constant Ncomp, CDELT[AXIS];
constant dRA0[comp], dDec0[comp];
constant BMaj[comp], BMin[comp], BPa[comp];
parameter Flux[comp];
data dRA, dDec;
observation Img;
variable pi = 3.14159265359;

main() {
  variable Model, rnorm[50], cosPA[50], sinPA[50];
  variable dX, dY, dMaj, dMin, R;
  variable i, r0, fact, diff;

  fact = 4.0 * log(2.0);
  r0 = abs(CDELT[1] * CDELT[2]) * (3600.0)^2 / 1.1331;
  for (i=1; i<=Ncomp; i=i+1) {
    rnorm[i] = r0 / (BMaj[i] * BMin[i]);
    cosPA[i] = cos(BPa[i]*pi/180.0);
    sinPA[i] = sin(BPa[i]*pi/180.0);
  }

  while (import()) {
    
    Model = 0;
    for (i=1; i<=Ncomp; i=i+1) {
      dX = dRA - dRA0[i];
      dY = dDec - dDec0[i];
      dMaj = dX * sinPA[i] + dY * cosPA[i];
      dMin = -dX * cosPA[i] + dY * sinPA[i];
      R = (dMaj/BMaj[i])^2 + (dMin/BMin[i])^2;
      Model = Model + abs(Flux[i]) * rnorm[i] * exp(-fact * R);
    }

    diff = Img - Model;
    export(diff);
  }
}

