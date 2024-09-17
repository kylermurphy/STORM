function km_grid_earth, $
         km_bin, $
         colat_min=colat_min, $
         hgt=hgt, $
         translate=translate, $
         plot_grid=plot_grid

         
  ; Guvi bin
  ;create lat/lon bins
  ; start along the positive y-axis
  if keyword_set(colat_min) then lat_min=colat_min else lat_min=45.
  if keyword_set(hgt) then hgt=hgt else hgt=110
  if keyword_set(translate) then translate=1 else translate=0
  
  slit_axis = list()
  slit_perp_up = list()
  slit_perp_dn = list()
  
  ; radius of earth
  re = 6378.14
  h_map = re + hgt
  
  ; define the latitude spacing corresponding
  ; to the desired km bin and create a latitude grid
  ; the initial grid is created along the positive x-axis
  dlat = km_bin*360./(2.*!pi*h_map)
  nlat = lat_min/dlat
  lat_arr = findgen(nlat+2)*dlat
  lat_arr = 90-max(lat_arr)+lat_arr
  
  ; calculate the corresponding longitude grid
  ; at each latitude point
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
  
  ; combine the arryays to get a grid across the
  ; full x-axis
  ; initial grid is only along the positive x-axis
  lon_lf = [lon_l,reverse(lon_l2[0:-2])]
  lon_rf = [lon_r,reverse(lon_r2[0:-2])]
  lat_f = [lat_arr, reverse(lat_arr[0:-2])]
  
  lat_f = lat_f[where(finite(lon_lf) eq 1)]
  lon_lf = lon_lf[where(finite(lon_lf) eq 1)]
  lon_rf = lon_rf[where(finite(lon_rf) eq 1)]
  
  ;append the initial grids to the 
  slit_axis.Add, lat_f
  slit_perp_up.Add, lon_lf
  slit_perp_dn.Add, lon_rf
  
  ;rotate or translate the initial grid
  ; to get a full set of grids
  if translate eq 1 then begin
    f_gr = translate_grid_earth(lat_f,lon_lf,lon_rf,h_map)
    ;f_gr = translate_grid_earth_simp(lat_f,lon_lf,lon_rf)
  endif else begin
    f_gr = rot_grid_earth(lat_f,lon_lf,lon_rf,h_map, dlat)
  endelse
  
  ; plot the grid
  if keyword_set(plot_grid) then begin
    if not keyword_set(overplot) then begin
      loadct, 0, /silent
      window, /free
      plot, [-1*lat_min, lat_min], [-1*lat_min, lat_min], /isotropic, /nodata
    endif

    
    for i=0L, n_elements(f_gr.lat_up[*,0])-1 do begin
      x_u = reform((90-f_gr.lat_up[i,*])*cos(f_gr.lon_up[i,*]*!dtor))
      x_d = reform((90-f_gr.lat_dn[i,*])*cos(f_gr.lon_dn[i,*]*!dtor))
      y_u = reform((90-f_gr.lat_up[i,*])*sin(f_gr.lon_up[i,*]*!dtor))
      y_d = reform((90-f_gr.lat_dn[i,*])*sin(f_gr.lon_dn[i,*]*!dtor))
      
      for j=0L, x_u.length-2 do begin
        x_vert = [x_u[j], x_u[j+1], x_d[j+1], x_d[j], x_u[j]]
        y_vert = [y_u[j], y_u[j+1], y_d[j+1], y_d[j], y_u[j]]
        
        plots, x_vert, y_vert, noclip=0
      endfor  
    endfor
  endif
  
  f_gr = create_struct(f_gr, 'h_aurora', hgt, 're', re, 'colat_min', colat_min)
  
  return, f_gr  
end




; main
; 
a = km_grid_earth(100, colat_min=20, /plot, translate=1)


end