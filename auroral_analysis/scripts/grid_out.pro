;output grid for gowtam

pro grid_out

  out_dir = 'C:\Users\murph\OneDrive\STORM_2024\GUVI Analysis\'
  out_file = 'GUVI_GRID.txt'


  cmin = 45
  pix_sz = 0.26*1E-3
  
  ;image_plot_sav,fn, /ns_scl, /clog
  guvi_grid = km_grid_guvi(colat_min=cmin, fov_a=pix_sz, pix_a=pix_sz)


  openw, outlun, out_dir+out_file, /get_lun
  
  printf, outlun, guvi_grid.name
  printf, outlun, guvi_grid.description
  printf, outlun, 'FOV (radians): '+strtrim(guvi_grid.FOV,2)
  printf, outlun, 'Pixel Size (radians): '+strtrim(guvi_grid.FOV)
  printf, outlun, 'Satellite Location (RE): '+strtrim(guvi_grid.SAT_R)
  printf, outlun, 'Assumed heigt of auroral emission (km): '+strtrim(guvi_grid.H_AURORA)
  printf, outlun, 'Radius of Earth (km): '+strtrim(guvi_grid.RE)
  printf, outlun, 'Minimum Colatitude of Grid (degrees): '+strtrim(guvi_grid.COLAT_MIN)
  
  loadct, 25, /silent
  for i=0l, n_elements(guvi_grid.LAT_UP[*,0])-1 do begin
    
    printf, outlun, i, format='("Scan: " (I03))'
    printf, outlun, guvi_grid.lat_up[i,*], format='("Lat Upper: " 211(" ",F0.4,","))'
    printf, outlun, guvi_grid.lon_up[i,*], format='("Lon Upper: " 211(" ",F0.4,","))'
    printf, outlun, guvi_grid.lat_dn[i,*], format='("Lat Down: " 211(" ",F0.4,","))'
    printf, outlun, guvi_grid.lon_dn[i,*], format='("Lon Down: " 211(" ",F0.4,","))'
    
    x_u = reform((90-guvi_grid.lat_up[i,*])*cos(guvi_grid.lon_up[i,*]*!dtor))
    y_u = reform((90-guvi_grid.lat_up[i,*])*sin(guvi_grid.lon_up[i,*]*!dtor))

    x_d = reform((90-guvi_grid.lat_dn[i,*])*cos(guvi_grid.lon_dn[i,*]*!dtor))
    y_d = reform((90-guvi_grid.lat_dn[i,*])*sin(guvi_grid.lon_dn[i,*]*!dtor))
    
    
    for j=0L, n_elements(guvi_grid.LAT_UP[0,*])-1 do begin
      if i MOD 5 eq 0 and j lt n_elements(guvi_grid.LAT_UP[0,*])-1 then begin

       
        x_vert = [x_u[j], x_u[j+1], x_d[j+1], x_d[j], x_u[j]]
        y_vert = [y_u[j], y_u[j+1], y_d[j+1], y_d[j], y_u[j]]

        plots, x_vert, y_vert, noclip=0, color = 44
      endif
    
    endfor
  endfor

end