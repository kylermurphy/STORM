FUNCTION pts_in_fast, x, y, px, py


  ; Number of vertices in the polygon
  nvert = px.length
  vertx = px
  verty = py
  ; Points that are inside
  points_inside = list()

  bb = intarr(x.length)
  ;For every candidate position within the bounding box
  
  for w=0L, x.length-1 do begin
    testx = x[w]
    testy = y[w]
    
    c=0
    
    for i=0L, nvert-1 do begin
      if i ne 0 then j=i-1 else j=nvert-1
      
      if ((verty[i] gt testy ) ne (verty[j] gt testy)) then begin
        if (testx lt (vertx[j] - vertx[i]) * (testy - verty[i]) / (verty[j] - verty[i]) + vertx[i]) then c += 1
      endif        
    endfor
    
    bb[w] = c
    if c mod 2 eq 1 then points_inside.Add, w 
  endfor
 
  return, points_inside.ToArray()
  
  
end





; main
; testing the two codes

restore,'C:\Users\murph\OneDrive\STORM_2024\GUVI Analysis\test_dat.sav', /verbose

t0=systime(/seconds)
ff = pts_in_fast(f_x,f_y,scan_x,scan_y)
t1=systime(/seconds)
xx = pts_inside(f_x,f_y,scan_x,scan_y)
scan_ind = where(xx eq 1)
t2=systime(/seconds)

print, t1-t0
print, t2-t1




end






