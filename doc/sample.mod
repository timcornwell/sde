1.0,     0.0, 0.0, 0.0, 0.0, 0.0, 'POINT'
1000.0, 10.0, 0.0, 1.0, 5.0, 100.0, 'DISK'
1000.0,  0.0,10.0, 1.0, 5.0, 10.0, 'GAUS'
1000.0,-10.0, 0.0, 1.0, 5.0, 1.0, 'RECT'
------------------------------------------
Format is:
Flux, X Pos, Y Pos, Maj. Axis Size, Minor Axis size, Position angle, Type

Units:
Flux - Integrated over all of component.
X,Y Pos - Relative to centre of image (0.0, 0.0). In Arcsecs.
Maj, Minor Axis - In Arcsecs.
Position angle - 0 -> horizontal 90 -> vertical. In degrees
Type - Point, Gaus, Disk or Rect