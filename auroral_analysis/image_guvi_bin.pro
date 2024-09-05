function image_guvi_bin, $
         km_bin, $
         lat_min = lat_min, $ 
         hgt = hgt
         
  
         
       
  ;create lat/lon bins
  ; start along the positive y-axis
  if keyword_set(lat_min) then lat_min=lat_min else lat_min=45.
  if keyword_set(hgt) then hgt=hgt else hgt=110
  
  
  ; radius of earth
  re = 6378.14
  h_map = re + hgt
  
  ; define the 
  dlat = km_bin*360./(2.*!pi*h_map)
  nlat = (90.-lat_min)/dlat
  lat_arr = findgen(nlat+2)*dlat
  lat_arr = 90-max(lat_arr)+lat_arr
  
  lon_l = fltarr(lat_arr.length)
  lon_l2 = lon_l
  lon_r = lon_l
  lon_r2 = lon_l
  
  for i=0L, lat_arr.length-1 do begin
    ; calculate the midpoint of the latitude
    theta = 90-lat_arr[i]
    ; calculate the radius of the circle at
    ; the midpoint
    r0 = sin(theta*!dtor)*h_map
    ; calculate the delta longitude
    ; corresponding to lon_res
    dlon = (km_bin/2)*360./(2.*!pi*r0)
    
    lon_l[i] = dlon
    lon_l2[i] = 180-dlon
    lon_r[i] = -1*dlon
    lon_r2[i] = 180+dlon
  endfor
  
  lon_lf = [lon_l,reverse(lon_l2[0:-2])]
  lon_rf = [lon_r,reverse(lon_r2[0:-2])]
  lat_f = [lat_arr, reverse(lat_arr[0:-2])]
  
  lat_f = lat_f[where(finite(lon_lf) eq 1)]
  lon_lf = lon_lf[where(finite(lon_lf) eq 1)]
  lon_rf = lon_rf[where(finite(lon_rf) eq 1)]
  
  x_fl = (90-lat_f)*cos(lon_lf*!dtor)
  x_fr = (90-lat_f)*cos(lon_rf*!dtor)
  
  y_fl = (90-lat_f)*sin(lon_lf*!dtor)
  y_fr = (90-lat_f)*sin(lon_rf*!dtor)
   
  
  loadct, 0, /silent
  window
  plot, [-1*lat_min, lat_min], [-1*lat_min, lat_min], /isotropic, /nodata 
  loadct,25,/silent
  color=bytscl(indgen(x_fl.length-1))
  for i=0L, x_fl.length-2 do begin
    x_vert = [x_fl[i],x_fl[i+1],x_fr[i+1],x_fr[i],x_fl[i]]
    y_vert = [y_fl[i],y_fl[i+1],y_fr[i+1],y_fr[i],y_fl[i]]
    polyfill, x_vert, y_vert, color=color[i]
  endfor
  loadct,0,/silent
  plots, x_fl, y_fl, psym=sym(1), symsize=0.5
  plots, x_fr, y_fr, psym=sym(1), symsize=0.5

  ; convert spherical coordinates to 
  ; cartesian, rotate about x-axis by dlat
  ; convert back and replot
  
  r_sp = lon_lf
  r_sp[*] = h_map
  sp_coord = [[lon_lf], [lat_f], [r_sp]]
  
  ct_coord = cv_coord(from_sphere=transpose(sp_coord), /to_rect, /degree)
  ; create a rotation matrix
  y_max = 0
  i = 1L
  loadct,25,/silent
  while y_max le lat_min do begin
    r_mx = mg_rotate([1,0,0],i*dlat)
    r_mx = r_mx[0:2,0:2]

    r_ct = r_mx#ct_coord

    sp_ct = cv_coord(from_rect=r_ct, /to_sphere,/degree)

    lon_rot = sp_ct[0,*]
    lat_rot = sp_ct[1,*]

    x_rot = (90-lat_rot)*cos(lon_rot*!dtor)
    y_rot = (90-lat_rot)*sin(lon_rot*!dtor)

    plots, x_rot, y_rot, psym=sym(1), symsize=0.5, color = i*5
    y_max = max(y_rot)
    i = i+1
  endwhile
  
  xlat=0.5
  
  ll = findgen((90-lat_min)/xlat+1)*xlat+lat_min

  x = h_map*sin((90-ll)*!dtor)
  
  ap = 30*re - h_map*cos((90-ll)*!dtor)
  rp = sqrt(ap*ap+x*x)
  
  y =  (0.26*1E-3)*rp
  
  y_u = y/2.
  y_l = -1*y_u
  
  zp = -1*ap
  
  z = h_map*cos((90-ll)*!dtor)
  
  theta = atan(x,zp)
  
  c0 = [[x],[y_l],[z]]
  c1 = [[x],[y_u],[z]]
  
  sp0 = cv_coord(from_rect=transpose(c0), /to_sphere,/degree)
  sp1 = cv_coord(from_rect=transpose(c1), /to_sphere,/degree)
  
  lon0 = sp0[0,*]
  lat0 = sp0[1,*]
  
  lon1 = sp1[0,*]
  lat1 = sp1[1,*]
  
  x_0 = (90-lat0)*cos(lon0*!dtor)
  x_1 = (90-lat1)*cos(lon1*!dtor)

  y_0 = (90-lat0)*sin(lon0*!dtor)
  y_1 = (90-lat1)*sin(lon1*!dtor)
  
  
  loadct, 0, /silent
  window, 2
  plot, [-1*lat_min, lat_min], [-1*lat_min, lat_min], /isotropic, /nodata
  loadct,25,/silent
  color=bytscl(indgen(y_0.length-1))
  for i=0L, y_0.length-2 do begin
    x_vert = [x_0[i],x_0[i+1],x_1[i+1],x_1[i],x_0[i]]
    y_vert = [y_0[i],y_0[i+1],y_1[i+1],y_1[i],y_0[i]]
    polyfill, x_vert, y_vert, color=color[i]
    ;stop
  endfor 
       
  stop
  
  dlat = 0.00030636787/!dtor
  y_max = 0
  i = 1L
  loadct,25,/silent
  
  ;prime coord system
  c_prime = transpose([[x],[y_l],[zp]])
  
  
  while y_max le lat_min do begin
    r_mx = mg_rotate([1,0,0],-1*i*dlat)
    r_mx = r_mx[0:2,0:2]

    r_ct = r_mx#c_prime
    
    ;now translate this
    r_ct[2,*] = 30*re+r_ct[2,*]

    sp_ct = cv_coord(from_rect=r_ct, /to_sphere,/degree)

    lon_rot = sp_ct[0,*]
    lat_rot = sp_ct[1,*]

    x_rot = (90-lat_rot)*cos(lon_rot*!dtor)
    y_rot = (90-lat_rot)*sin(lon_rot*!dtor)

    plots, x_rot, y_rot, psym=sym(1), symsize=0.5, color = i*5
    y_max = max(y_rot)
    i = i+1
  endwhile
       
       
       
  return, 0   
end


;main
;

a = image_guvi_bin(100)


end