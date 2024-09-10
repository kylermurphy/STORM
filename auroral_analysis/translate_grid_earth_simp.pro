; Pass a single scan grid in lat and longitude and 
; translate it to get a series of scans comprising a complete grid
; ignore that the surface we are mapping to is a sphere

function translate_grid_earth_simp, lat, lon_u, lon_d, tr_sp=tr_sp


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
  
  x_u = (90-lat)*cos(lon_u*!dtor)
  x_d = (90-lat)*cos(lon_d*!dtor)
  y_u = (90-lat)*sin(lon_u*!dtor)
  y_d = (90-lat)*sin(lon_d*!dtor)
  
  if keyword_set(tr_sp) then tr_sp=tr_sp else tr_sp=mean(abs(y_u-y_d),/nan)


  ; translate up
  i=1L
  y_max=0
  while y_max lt lat_bound do begin
    y_ut = y_u+i*tr_sp
    y_dt = y_d+i*tr_sp

    lat_u = 90 - sqrt(y_ut*y_ut + x_u*x_u)
    lat_d = 90 - sqrt(y_dt*y_dt + x_d*x_d)

    th_u = atan(y_ut,x_u)/!dtor
    th_d = atan(y_dt,x_d)/!dtor

    lat_ur.add, lat_u
    lat_dr.add, lat_d
    lon_ur.add, th_u
    lon_dr.add, th_d

    i++
    y_max = max(abs(y_dt))
  endwhile
 
  ; translate down
  i=1L
  y_max=0
  while y_max lt lat_bound do begin
    y_ut = y_u-i*tr_sp
    y_dt = y_d-i*tr_sp

    lat_u = 90 - sqrt(y_ut*y_ut + x_u*x_u)
    lat_d = 90 - sqrt(y_dt*y_dt + x_d*x_d)

    th_u = atan(y_ut,x_u)/!dtor
    th_d = atan(y_dt,x_d)/!dtor

    lat_ur.add, lat_u
    lat_dr.add, lat_d
    lon_ur.add, th_u
    lon_dr.add, th_d

    i++
    y_max = max(abs(y_dt))
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
    tr_sp:tr_sp}

  return, r_str
  
  end
  