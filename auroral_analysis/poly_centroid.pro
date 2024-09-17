function poly_centroid, x, y


  ; close the polygon
  tmp_x = [x,x[0]]
  tmp_y = [y,y[0]]
  
  ; calculate area of polygon
  a = 0
  cx = 0
  cy = 0
  for i=0L, tmp_x.length-2 do begin
    v = (tmp_x[i]*tmp_y[i+1]-tmp_x[i+1]*tmp_y[i])
    a = a + v
    cx = cx + (tmp_x[i]+tmp_x[i+1])*v
    cy = cy + (tmp_y[i]+tmp_y[i+1])*v
  endfor
  a = (a/2.)
  
  cx = cx/(6*a)
  cy = cy/(6*a)
  
  return, {area:abs(a), cx:cx, cy:cy}

end