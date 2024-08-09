function image_bin, $ 
  fn, $ ; file to load
  lat_res = lat_res, $ ; km resolution in the latitude direction
  lon_res = lon_res, $ ; km resolution in the longitude direction
  lat_min = lat_min, $ ; minimum latitude for grid
  hgt = hgt, $ ; height of the aurora
  clog = clog ; log before plotting
  
  
  if keyword_set(lat_res) then lat_res = lat_res else lat_res = 100.
  if keyword_set(lon_res) then lon_res = lon_res else lon_res = 100.
  if keyword_set(lat_min) then lat_min = lat_min else lat_min = 45.
  if keyword_set(hgt) then hgt = hgt else hgt = 110 
  if keyword_set(clog) then clog = 1 else clog = 0
  
  ; radius of earth
  re = 6378.14
  h_map = re + hgt
  
  ;open file
  restore, fn
  td = time_double(imageinfo.epoch, /epoch)
  ts = time_string(td)
  
  ; find 
  sm=fltarr(1,3)
  sm[0,0] = 6738.14
  dprint,getdebug=gd
  dprint,setdebug=-10
  cotrans, sm, gsm, td, /SM2GSM
  cotrans, gsm, gse, td, /GSM2GSE
  cotrans, gse, gei, td, /GSE2GEI
  cotrans, gei, geo, td, /GEI2GEO
  ;restore previous verbosity
  dprint,setdebug=gd
  
  ; when using mapset azimuth
  ; longitude is x and latitude is y
  sm_lon = atan(geo[1],geo[0])/!dtor
  if sm_lon lt 0 then sm_lon = 360+sm_lon
  
  ; setup latitude grid
  ; using arc legnth and lat_res
  dlat = lat_res*360./(2.*!pi*h_map)
  nlat = (90.-lat_min)/dlat
  lat_arr = findgen(nlat+1)*dlat
  lat_arr = 90-max(lat_arr)+lat_arr
  
  ; setup longitude grid
  lon_arr = list()
  for i=0L, lat_arr.length-2 do begin
    ; calculate the midpoint of the latitude
    lat_mid = (lat_arr[i]+lat_arr[i+1])/2.
    theta = 90-lat_mid
    ; calculate the radius of the circle at
    ; the midpoint
    r0 = sin(theta*!dtor)*h_map
    ; calculate the delta longitude 
    ; corresponding to lon_res
    dlon = lon_res*360./(2.*!pi*r0)
    nlon = 360/dlon
    lon_val = findgen(nlon+2)*dlon
    if lon_val[-1] gt 360. then lon_val[-1] = 360.
    lon_arr.Add, lon_val
  endfor
  
  ; construct the new binned image
  im = imageinfo.image
  im_lat = imageinfo.glat
  im_lon = imageinfo.glon
  bd_lon = where(im_lon lt 0, c)
  if c gt 0 then im_lon[bd_lon] = !values.f_nan
  im_lon = im_lon-sm_lon
  bd_lon = where(im_lon lt 0, c)
  if c gt 0 then im_lon[bd_lon] = 360+im_lon[bd_lon]
  
  
  ; setup values to store
  ; new binned image pixel
  ; vertice locations and 
  ; image max and min
  im_flat = list()
  lat_vert = list()
  lon_vert = list()
  
  im_max = -1 
  im_min = max(im)+1000
  
  for i=0L, lat_arr.length-2 do begin
    ; get pixel vertices
    lat_v = [lat_arr[i],lat_arr[i+1],lat_arr[i+1],lat_arr[i],lat_arr[i]]
    ; create a mask of all pixels within this latitude range
    gd_lat = where(im_lat gt lat_arr[i] and im_lat le lat_arr[i+1], lat_c)
    if lat_c lt 1 then continue
    
    lat_mask = fltarr(im_lat.dim)
    lat_mask[*] = !values.f_nan
    lat_mask[gd_lat] = 1
    
    
    ; grab the longitude grid for this lat range
    ; loop through the longitude grid to generate
    ; a mask where these values are in the image
    ; and combine into new image
    lon_vals = lon_arr[i]
    im_arr = fltarr(lon_vals.length-1)
    
    for j=0L, lon_vals.length-2 do begin
      ; get pixel vertices
      lon_v = [lon_vals[j],lon_vals[j],lon_vals[j+1],lon_vals[j+1],lon_vals[j]]
      ; create a mask of all pixles within this longitude range
      gd_lon = where(im_lon gt lon_vals[j] and im_lon le lon_vals[j+1], lon_c)

      lon_mask = fltarr(im_lon.dim)
      lon_mask[*] = !values.f_nan
      lon_mask[gd_lon] = 1
      
      if lon_c lt 1 then im_val = !values.f_nan else im_val = mean(lon_mask*lat_mask*im,/nan)
      im_arr[j] = im_val
      
      im_flat.Add, im_val
      lat_vert.Add, lat_v
      lon_vert.Add, lon_v
      

    endfor
  endfor
  

  im_flat = im_flat.ToArray()
  im_col = im_flat
  im_min = min(im_flat)
  im_max = max(im_flat)
  im_col[where(im_flat le im_min or finite(im_flat) ne 1)] = im_min
  im_col[where(im_flat ge im_max)] = im_max
  if clog eq 1 then im_col = alog10(im_col)
  
  im_col = bytscl(im_col,/nan)
  
  window, 2, xsize = 500, ysize = 500
  !p.multi  = [0,1,1]
  !x.omargin = [15,15]
  loadct,0
  xyouts, 0.5,0.85, ts, /normal, alignment=0.5, charsize=1.5
  map_set, 90, 0, 180, /azimuthal,/isotropic, limit=[lat_min,0,90,360],/noerase
  ; plot circles for each pixel
  loadct, 8
  
  for i=0L, im_flat.length-1 do begin
    polyfill, lon_vert[i], lat_vert[i], color = im_col[i]
  endfor
  
  stop
  return, 0
end


;main 
;testing
;

fn = 'D:\data\IMAGE_FUV\2001\WIC\016\wic20010160013.idl'
fn = "D:\data\IMAGE_FUV\2001\WIC\016\wic20010161609.idl"

a = image_bin(fn,clog=1)



end