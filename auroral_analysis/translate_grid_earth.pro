;Pass a single scan grid in lat and longitude 
;and translate it to get a series of scans 
;comprising a complete grid
;
;Account for the fact that we are mapping the 
;subsequnet grid to a new height/sphere

function translate_grid_earth, lat, lon_u, lon_d, r_map, tr_sp=tr_sp


  ;translate the initial grid
  
  lat_bound = max(90-lat)
  
  lat_ur = list()
  lon_ur = list()
  lat_dr = list()
  lon_dr = list()

  lat_ur.add, lat
  lat_dr.add, lat
  lon_ur.add, lon_u
  lon_dr.add, lon_d
  
  r_sp = lat
  r_sp[*] = r_map
  sp_up = [[lon_u], [lat], [r_sp]]
  sp_dn = [[lon_d], [lat], [r_sp]]

  ct_u = cv_coord(from_sphere=transpose(sp_up), /to_rect, /degree)
  ct_d = cv_coord(from_sphere=transpose(sp_dn), /to_rect, /degree)

  x_u = reform(ct_u[0,*])
  x_d = reform(ct_d[0,*])
  y_u = reform(ct_u[1,*])
  y_d = reform(ct_d[1,*])

  if keyword_set(tr_sp) then tr_sp=tr_sp else tr_sp=mean(abs(y_u-y_d),/nan)
  
  i=1L
  y_max=0
  while y_max lt lat_bound do begin
    y_ut = y_u+i*tr_sp
    y_dt = y_d+i*tr_sp

    z_ut = sqrt(r_sp*r_sp-y_ut*y_ut-x_u*x_u)
    z_dt = sqrt(r_sp*r_sp-y_dt*y_dt-x_d*x_d)

    ct_u = [[x_u],[y_ut],[z_ut]]
    ct_d = [[x_d],[y_dt],[z_dt]]

    sp_u = cv_coord(from_rect=transpose(ct_u), /to_sphere, /degree)
    sp_d = cv_coord(from_rect=transpose(ct_d), /to_sphere, /degree)

    lat_ur.add, reform(sp_u[1,*])
    lat_dr.add, reform(sp_d[1,*])

    lon_ur.add, reform(sp_u[0,*])
    lon_dr.add, reform(sp_d[0,*])

    i++
    y_max = max((90-sp_u[1,*])*sin(sp_u[0,*]*!dtor))
    ;stop
  endwhile
  
  i=1L
  y_max=0
  while y_max lt lat_bound do begin
    y_ut = y_u-i*tr_sp
    y_dt = y_d-i*tr_sp

    z_ut = sqrt(r_sp*r_sp-y_ut*y_ut-x_u*x_u)
    z_dt = sqrt(r_sp*r_sp-y_dt*y_dt-x_d*x_d)

    ct_u = [[x_u],[y_ut],[z_ut]]
    ct_d = [[x_d],[y_dt],[z_dt]]

    sp_u = cv_coord(from_rect=transpose(ct_u), /to_sphere, /degree)
    sp_d = cv_coord(from_rect=transpose(ct_d), /to_sphere, /degree)

    lat_ur.add, reform(sp_u[1,*])
    lat_dr.add, reform(sp_d[1,*])

    lon_ur.add, reform(sp_u[0,*])
    lon_dr.add, reform(sp_d[0,*])

    i++
    y_max = max(abs((90-sp_d[1,*])*sin(sp_d[0,*]*!dtor)))
    ;stop
  endwhile
  
  
  lon_ur = lon_ur.ToArray()
  lon_dr = lon_dr.ToArray()
  
  bd = where(lon_ur lt 0, c)
  if c gt 0 then lon_ur[bd] = 360+lon_ur[bd]
  bd = where(lon_dr lt 0, c)
  if c gt 0 then lon_dr[bd] = 360+lon_dr[bd]

    
  r_str = {name:'Lat/Lon Grid for binning IMAGE data', $
    description:'Created by translating an initial grid up or down', $
    lat_up:lat_ur.ToArray(), lat_dn:lat_dr.ToArray(), $
    lon_up:lon_ur, lon_dn:lon_dr, $
    tr_sp:tr_sp, h_aurora:hgt, re:re}

  return, r_str

end