function km_grid_guvi, $ 
  fov_a = fov_a, $ ; field of view of the sensor in radians
  pix_a = pix_a, $ ; field of view of a pixel in radians
  st_l = st_l, $ ; location of storm satellite
  colat_min = colat_min, $ ; minimum latitude to generate FOV for
  hgt = hgt
  
  if keyword_set(fov_a) then fov_a = fov_a else fov_a = 0.26*1E-3
  if keyword_set(pix_a) then pix_a = pix_a else pix_a = 0.26*1E-3
  if keyword_set(st_l) then st_l = st_l else st_l = 30
  if keyword_set(colat_min) then lat_min=colat_min else lat_min=45.
  if keyword_set(hgt) then hgt=hgt else hgt=110 

  ; radius of earth
  re = 6378.14
  h_map = re + hgt

  ; create an initial GUVI
  ; FOV grid
  
  lat_st = 90 ; starting latitude for grid
  
  rp_dist = list()
  zp_dist = list()
  z_dist = list()
  x_u = list()
  x_l = list()
  y_u = list()
  y_l = list()
  
  x_lat = list()
  
  
  while lat_st gt 90-lat_min do begin
    x = h_map*sin((90-lat_st)*!dtor)
    
    ap = st_l*re - h_map*cos((90-lat_st)*!dtor)
    rp = sqrt(ap*ap+x*x)
    
    y =  (fov_a)*rp
    
    
    ; width of slit in x-direction
    ; at this height
    x_w = pix_a*rp
    dlat = x_w/(h_map*!dtor)
    
    rp_dist.Add, rp
    x_u.Add, x
    x_l.Add, x
    y_u.Add, y/2.
    y_l.Add, -1*y/2.
    
    zp_dist.Add, -1*ap
    z_dist.Add, h_map*cos((90-lat_st)*!dtor)
    
    x_lat.Add, lat_st
    
    lat_st = lat_st-dlat
    
  endwhile
  
  rp_dist = rp_dist.ToArray()
  x_u = x_u.ToArray()
  x_l = x_l.ToArray()
  y_u = y_u.ToArray()
  y_l = y_l.ToArray()
  
  zp_dist = zp_dist.ToArray()
  z_dist = z_dist.ToArray()
  x_lat = x_lat.ToArray()
  
  x_u = [-1*reverse(x_u[1:-1]), x_u]
  x_l = [-1*reverse(x_l[1:-1]), x_l]
  y_u = [reverse(y_u[1:-1]), y_u]
  y_l = [reverse(y_l[1:-1]), y_l]
  z_dist = [reverse(z_dist[1:-1]), z_dist]
  zp_dist = [reverse(zp_dist[1:-1]), zp_dist]
  x_lat = [reverse(x_lat[1:-1]), x_lat]
  
  ; create an array of for cartesian 
  ; coords and determin the latitude and
  ; longitude for pixel vertices
  
  lat_l = list()
  lat_u = list()
  lon_l = list()
  lon_u = list()
  
  cl = [[x_l],[y_l],[z_dist]]
  cu = [[x_u],[y_u],[z_dist]]

  sp_u = cv_coord(from_rect=transpose(cu), /to_sphere,/degree)
  sp_l = cv_coord(from_rect=transpose(cl), /to_sphere,/degree)
  
  lon_u.Add, sp_u[0,*]
  lat_u.Add, sp_u[1,*]

  lon_l.Add, sp_l[0,*]
  lat_l.Add, sp_l[1,*]
  
  ; create a prime coordinates system
  ; this is where the origin is at the
  ; location of the storm satellite
  ; rotate around this coordinate system
  ; to simulate motion of the fov
  
  ; rotate around this angle
  rot_a = fov_a/!dtor
  
  cp_u = transpose([[x_u],[y_u],[zp_dist]])
  cp_l = transpose([[x_l],[y_l],[zp_dist]])
  
  r_sp = x_u
  r_sp[*] = h_map
  
  window, 2
  fixplot
  plot, [-1*lat_min, lat_min], [-1*lat_min, lat_min], /isotropic, /nodata
  loadct,25,/silent
  
  x_0 = (90-sp_u[1,*])*cos(sp_u[0,*]*!dtor)
  y_0 = (90-sp_u[1,*])*sin(sp_u[0,*]*!dtor)

  x_1 = (90-sp_l[1,*])*cos(sp_l[0,*]*!dtor)
  y_1 = (90-sp_l[1,*])*sin(sp_l[0,*]*!dtor)
 

  color=bytscl(indgen(x_0.length-1))
  for j=0L, x_0.length-2 do begin
    x_vert = [x_0[j],x_0[j+1],x_1[j+1],x_1[j],x_0[j]]
    y_vert = [y_0[j],y_0[j+1],y_1[j+1],y_1[j],y_0[j]]
    plots, x_vert, y_vert, color=color[j]
    ;stop
  endfor
  
  i=1L
  y_v=0
  while y_v le lat_min do begin
    
    ; create rotation matrix
    r_mx = mg_rotate([1,0,0],i*rot_a)
    r_mx = r_mx[0:2,0:2]

    ; rotate prime/satellite coord
    ; system
    r_ct_u = r_mx#cp_u
    r_ct_l = r_mx#cp_l
    
    ; calculate new z coord
    r_z_u = sqrt(h_map*h_map - reform(r_ct_u[0,*]^2.+r_ct_u[1,*]^2.))
    r_z_l = sqrt(h_map*h_map - reform(r_ct_l[0,*]^2.+r_ct_l[1,*]^2.))
    

    ;now translate the rotated matrix here
    r_ct_u[2,*] = r_z_u
    r_ct_l[2,*] = r_z_l
    
    ;transform to spherical coordinates
    ; centered on the earth and get the lat
    ; lon of the pixel vertics

    sp_ru = cv_coord(from_rect=r_ct_u, /to_sphere,/degree)
    sp_rl = cv_coord(from_rect=r_ct_l, /to_sphere,/degree)

    lon_u.Add, reform(sp_ru[1,*])
    lat_u.Add, reform(sp_ru[0,*])
    
    lat_l.Add, reform(sp_rl[1,*])
    lon_l.Add, reform(sp_rl[0,*])
    
    y_v = max(abs((90-sp_ru[1,*])*sin(sp_ru[0,*]*!dtor)))
    print, y_v, i
    i = i+1
    
    x_0 = (90-sp_ru[1,*])*cos(sp_ru[0,*]*!dtor)
    y_0 = (90-sp_ru[1,*])*sin(sp_ru[0,*]*!dtor)
    
    x_1 = (90-sp_rl[1,*])*cos(sp_rl[0,*]*!dtor)
    y_1 = (90-sp_rl[1,*])*sin(sp_rl[0,*]*!dtor)
    
    color=bytscl(indgen(x_0.length-1))
    for j=0L, y_1.length-2 do begin
      x_vert = [x_0[j],x_0[j+1],x_1[j+1],x_1[j],x_0[j]]
      y_vert = [y_0[j],y_0[j+1],y_1[j+1],y_1[j],y_0[j]]
      plots, x_vert, y_vert, color=color[j]
      ;stop
    endfor
  endwhile
  
  i=1L
  y_v=0
  while y_v le lat_min do begin

    ; create rotation matrix
    r_mx = mg_rotate([1,0,0],-1*i*rot_a)
    r_mx = r_mx[0:2,0:2]

    ; rotate prime/satellite coord
    ; system
    r_ct_u = r_mx#cp_u
    r_ct_l = r_mx#cp_l

    ; calculate new z coord
    r_z_u = sqrt(h_map*h_map - reform(r_ct_u[0,*]^2.+r_ct_u[1,*]^2.))
    r_z_l = sqrt(h_map*h_map - reform(r_ct_l[0,*]^2.+r_ct_l[1,*]^2.))


    ;now translate the rotated matrix here
    r_ct_u[2,*] = r_z_u
    r_ct_l[2,*] = r_z_l

    ;transform to spherical coordinates
    ; centered on the earth and get the lat
    ; lon of the pixel vertics

    sp_ru = cv_coord(from_rect=r_ct_u, /to_sphere,/degree)
    sp_rl = cv_coord(from_rect=r_ct_l, /to_sphere,/degree)

    lon_u.Add, reform(sp_ru[1,*])
    lat_u.Add, reform(sp_ru[0,*])

    lat_l.Add, reform(sp_rl[1,*])
    lon_l.Add, reform(sp_rl[0,*])

    y_v = max(abs((90-sp_ru[1,*])*sin(sp_ru[0,*]*!dtor)))
    print, y_v, i
    i = i+1

    x_0 = (90-sp_ru[1,*])*cos(sp_ru[0,*]*!dtor)
    y_0 = (90-sp_ru[1,*])*sin(sp_ru[0,*]*!dtor)

    x_1 = (90-sp_rl[1,*])*cos(sp_rl[0,*]*!dtor)
    y_1 = (90-sp_rl[1,*])*sin(sp_rl[0,*]*!dtor)

    color=bytscl(indgen(x_0.length-1))
    for j=0L, y_1.length-2 do begin
      x_vert = [x_0[j],x_0[j+1],x_1[j+1],x_1[j],x_0[j]]
      y_vert = [y_0[j],y_0[j+1],y_1[j+1],y_1[j],y_0[j]]
      plots, x_vert, y_vert, color=color[j]
      ;stop
    endfor
  endwhile
  
  stop
  
  
  
  
end


; main
; testing

a = km_grid_guvi(colat_min=20)


end

