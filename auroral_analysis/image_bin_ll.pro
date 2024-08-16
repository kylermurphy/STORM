function image_bin_ll, $
  fn, $ ; file to load
  lat_res = lat_res, $ ; km resolution in the latitude direction
  lon_res = lon_res, $ ; km resolution in the longitude direction
  lat_min = lat_min, $ ; minimum latitude for grid
  mag = mag, $ ; use geomagnetic coordinates
  geo = geo, $ ; use geographic coordinates
  mlt = mlt, $ ; use mlt and magnetic latitude
  hgt = hgt, $ ; height of the aurora
  im_min = im_min, $ ; min intensity to plot
  im_max = im_max, $ ; max intensity to plot
  im_plot = im_plot, $ ; plot the imate
  clog = clog, $ ; log before plotting
  save_png = save_png, $ ; output png
  out_dir = out_dir ; 
  
  
  if keyword_set(lat_res) then lat_res=lat_res else lat_res=1.
  if keyword_set(lon_res) then lon_res=lon_res else lon_res=5.
  if keyword_set(lat_min) then lat_min=lat_min else lat_min=45.
  if keyword_set(mag) then mag = 1 else mag = 0
  if keyword_set(geo) then geo = 1 else geo = 0
  if keyword_set(mlt) then mlt = 1 else mlt = 0
  if keyword_set(hgt) then hgt=hgt else hgt=110
  if keyword_set(im_plot) then im_plot=1 else im_plot=0 
  if keyword_set(clog) then clog=1 else clog=0
  if keyword_set(save_png) then save_png=1 else save_png=0
  if keyword_set(out_dir) then out_dir=out_dir else out_dir='D:\data\IMAGE_FUV\plots\'

  
  if keyword_set(wf) then wf=wf else wf=0.05
  if keyword_set(cw) then cw=cw else cw=0.03
  
  ;open file
  restore, fn
  td = time_double(imageinfo.epoch, /epoch)
  ts = time_string(td)
  
  im = imageinfo.image
  
  if mag eq 1 then begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon
  endif else if geo eq 1 then begin
    im_lat = imageinfo.glat
    im_lon = imageinfo.glon
  endif else if mag eq 1 then begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlt * 15
  endif else begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon    
  endelse
  
  nlat = (90.-lat_min)/lat_res
  lat_arr = findgen(nlat+1)*lat_res
  lat_arr = 90-max(lat_arr)+lat_arr

  nlon = 360/lon_res
  lon_arr = findgen(nlon+1)*lon_res

  im_arr = fltarr(nlon,nlat)
  lon_mask =  fltarr(im.dim)
  lat_mask = lon_mask
  
  for j=0L, nlon-1 do begin
      lon_vec = [lon_arr[j],lon_arr[j],lon_arr[j+1],lon_arr[j+1],lon_arr[j]]
      
      gd_lon = where(im_lon gt lon_arr[j] and im_lon le lon_arr[j+1], lon_c)
      if lon_c lt 1 then continue
      
      lon_mask[*] = !values.f_nan
      lon_mask[gd_lon] = 1
       
    for i=0L, nlat-1 do begin
      lat_vec = [lat_arr[i],lat_arr[i+1],lat_arr[i+1],lat_arr[i],lat_arr[i]]
      
      gd_lat = where(im_lat gt lat_arr[i] and im_lat le lat_arr[i+1], lat_c)
      if lat_c lt 1 then cotinue
      
      lat_mask[*] = !values.f_nan
      lat_mask[gd_lat] = 1
      
      im_arr[j,i] = total(lon_mask*lat_mask*im,/nan)
      
      ;stop
    endfor
  endfor

  stop
  
  stop
  return,0  
end


; main

fn = 'D:\data\IMAGE_FUV\2001\WIC\016\wic20010160013.idl'

im = image_bin_ll(fn)

end

