; rotate a single lat/lon grid for binning image data
; around the x-axis to create a full 2d gid

function rot_grid_earth, lat, lon_u, lon_d, r_map, rot_angle


  ; convert spherical coordinates to
  ; cartesian, rotate about x-axis by rot_angle
  ; convert back to spherical
  lat_bound =  max(90-lat)
  
  rot_ang = list()
  rot_ang.add, 0

  lat_ur = list()
  lon_ur = list()
  lat_dr = list() 
  lon_dr = list()
  
  lat_ur.add, lat
  lat_dr.add, lat
  lon_ur.add, lon_u
  lon_dr.add, lon_d

  ; create spherical coord system
  r_sp = lat
  r_sp[*] = r_map
  sp_up = [[lon_u], [lat], [r_sp]]
  sp_dn = [[lon_d], [lat], [r_sp]]

  ct_up = cv_coord(from_sphere=transpose(sp_up), /to_rect, /degree)
  ct_dn = cv_coord(from_sphere=transpose(sp_dn), /to_rect, /degree)

  ;rotate positive degrees about x-axis first
  i=0L
  y_max = 0
  while y_max le lat_bound do begin
    ;create rotation matrix
    rot_ang.add,i*rot_angle
    r_mx = mg_rotate([1,0,0],i*rot_angle)
    r_mx = r_mx[0:2,0:2]
    
    rct_up = r_mx#ct_up
    rct_dn = r_mx#ct_dn
    
    ; rotated cartesian coord
    rct_up = cv_coord(from_rect=rct_up, /to_sphere, /degree) 
    rct_dn = cv_coord(from_rect=rct_dn, /to_sphere, /degree)
    
    lat_ur.add, reform(rct_up[1,*])
    lon_ur.add, reform(rct_up[0,*])
    
    lat_dr.add, reform(rct_dn[1,*])
    lon_dr.add, reform(rct_dn[0,*])
    
    ;convert back to polar coordinates to find the max
    ; in the y direction
    y_max = max((90-rct_up[1,*])*sin(rct_up[0,*]*!dtor))
    i = i+1
  endwhile
  
  ;rotate negative degrees around the x-axis
  i=0L
  y_max = 0
  while y_max le lat_bound do begin
    ;create rotation matrix
    rot_ang.add,-1*i*rot_angle
    r_mx = mg_rotate([1,0,0],-1*i*rot_angle)
    r_mx = r_mx[0:2,0:2]

    rct_up = r_mx#ct_up
    rct_dn = r_mx#ct_dn

    ; rotated cartesian coord
    rct_up = cv_coord(from_rect=rct_up, /to_sphere, /degree)
    rct_dn = cv_coord(from_rect=rct_dn, /to_sphere, /degree)

    lat_ur.add, reform(rct_up[1,*])
    lon_ur.add, reform(rct_up[0,*])

    lat_dr.add, reform(rct_dn[1,*])
    lon_dr.add, reform(rct_dn[0,*])

    ;convert back to polar coordinates to find the max
    ; in the y direction
    y_max = max(abs((90-rct_up[1,*])*sin(rct_up[0,*]*!dtor)))
    i = i+1
  endwhile

  r_str = {name:'Lat/Lon Grid for binning IMAGE data', $
           description:'Created by rotating an initial grid about an Earth centered x-axis', $
           lat_up:lat_ur.ToArray(), lat_dn:lat_dr.ToArray(), $
           lon_up:lon_ur.ToArray(), lon_dn:lon_dr.ToArray(), $
           rot_ang:rot_ang.ToArray()}

  return, r_str
end