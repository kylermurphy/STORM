function image_fit_ll, $
  im, $ ; image [lon,lat]
  lon_arr, $ ; lon array (same or +1 elements image[*,0]) 
  lat_arr, $ ; lat arrat (same or +1 elements image[0,*])
  lon_avg=lon_avg, $ ; number of bins to average in the longitude direction
  lat_avg=lat_avg, $ ; number of bins to average in the latitude direction
  sun_rm=sun_rm, $ ; attempt to remove the sun
  lon_mid=lon_mid ; midnight longitude


  
  if keyword_set(lon_avg) then lon_avg=1 else lon_avg=lon_avg
  if keyword_set(lat_avg) then lat_avg=1 else lat_avg=lat_avg
  if keyword_set(sun_rm) then sun_rm=1 else sun_rm=0
  if keyword_set(lon_mid) then lon_mid=lon_mid else lon_mid=!values.f_nan
  

  if (im.dim)[0] ne lon_arr.length then $
    lon_fit = (lon_arr[0:-2]+lon_arr[1:-1])/2.0 else $
    lon_fit = lon_arr
  if (im.dim)[1] ne lat_arr.length then $
    lat_fit = (lat_arr[0:-2]+lat_arr[1:-1])/2.0 else $
    lat_fit = lat_arr
  
  if sun_rm eq 1 then begin
    if finite(lon_mid) eq 1 then begin
      lon_pos = min(abs(lon_arr-lon_mid),/nan)
      lon_pos = max(im[!C,*],/nan)
      lon_pos = lon_pos*(im.dim)[1]+!C
    endif else begin
      lon_pos = max(im,/nan)
      lon_pos = !C
      
      lon_pos = [lon_pos, lon_pos+1,lon_pos-1, $
        lon_pos-(im.dim)[0], lon_pos-(im.dim)[0]-1, lon_pos-(im.dim)[0]+1, $
        lon_pos+(im.dim)[0], lon_pos+(im.dim)[0]-1, lon_pos+(im.dim)[0]+1]
    endelse
    
    sun_pix = region_grow(im,lon_pos, /all_neighbors, /nan,stddev_multiplier=5)
    
    stop
    
  endif

  stop
  ; fit a gaussian first
  for i=0L, lon_fit.length-1 do begin
    
    
  endfor


  stop
  return,0
end


; Main
; 
; 

fn = 'D:\data\IMAGE_FUV\2001\WIC\016\wic20010161609.idl'
fn = "D:\data\IMAGE_FUV\2001\WIC\001\wic20010010351.idl"

im = image_bin_ll(fn)
imf = image_fit_ll(im.im, im.lon_arr, im.lat_arr, /sun_rm)

end