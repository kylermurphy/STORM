function image_fit_ll, $
  im_str, $
  lon_avg=lon_avg, $ ; number of bins to average in the longitude direction
  lat_avg=lat_avg, $ ; number of bins to average in the latitude direction
  sun_rm=sun_rm, $ ; attempt to remove the sun
  lon_mid=lon_mid ; midnight longitude



  ; get the tag names and make sure
  ; the data we need is there
  tags = tag_names(im_str)
  
  im_p = where(tags eq 'IM', im_c)
  lat_p = where(tags eq 'LAT_ARR')
  lon_p = where(tags eq 'LON_ARR')
  coo_p = where(tags eq 'COORD')
  
  if im_c eq 0 then return, 0
  if coo_p eq 0 then return, 0
  
  ;position of magnetic midnight
  if im_str.coord eq 'geomagnetic' then lon_mid = im_str.mlon_mid $
    else if im_str.coord eq 'geographic' then lon_mid = im_str.glon_mid $
    else lon_mid = im_str.mlt_mid*15
  
  im = im_str.(im_p)
  lon_arr = im_str.(lon_p)
  lat_arr = im_str.(lat_p)
  
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
    
    
    ;identify the roi using gauss_smooth and pick the pixels in the 95th percentile
    
    stop
  endif
  
  ; fit a gaussian to each longitude position
  ; start near midnight and work our way around
  ; through two loops
  ; 
  ; easier to fit around midnight
  ; can use previous fits to seed the next
  
  ; find the midnight position
  mn_min = min(abs(lon_fit-lon_mid),mn_pos)
  
  l_pts = lon_fit.length
  
  ; get points from midnight to ~ noon moving
  ; around dawn (cw)
  dawn_mn = indgen(l_pts/2.)+mn_pos
  bd = where(dawn_mn gt l_pts-1)
  dawn_mn[bd] = dawn_mn[bd]-l_pts
  
  ; get points from midnight to ~ noon moving
  ; around dusk (ccw)
  dusk_pts = l_pts-dawn_mn.length
  dusk_mn = indgen(dusk_pts)+dawn_mn[-1]+1
  
  ; check to make sure the number of fits we have
  ; matches the number of elements in the longitude array
  if total(dusk_mn)+total(dawn_mn) ne total(indgen(l_pts)) then begin
    print,'missing some longitude points for fitting'
    stop
  endif
  
  g_coeff = findgen(l_pts, 5)
  g_chi = findgen(l_pts)
  g_fit = im
  g_fit[*] = !values.f_nan
  
  ; first fit at midnight
  mn_fit = reform(im[dawn_mn[0],*])
  gd_fit = where(finite(mn_fit) eq 1)
  g_f = gaussfit(lat_fit[gd_fit],mn_fit[gd_fit], g_c, $
                    chisq=chisq, nterms=5)
  
  ; fill the arrays
  g_coeff[dawn_mn[0],*] = g_c
  g_fit[dawn_mn[0], gd_fit] = g_f
  g_chi[dawn_mn[0]] = chisq
  
  window
  loadct,39,/silent
  plot, lat_fit[gd_fit],mn_fit[gd_fit]
  oplot, lat_fit[gd_fit],g_f, color = 55
  
  ;repeat the above for cw toward noon
  for i=1L, dawn_mn.length-1 do begin
    stop
    mn_fit = reform(im[dawn_mn[i],*])
    gd_fit = where(finite(mn_fit) eq 1)
    g_f = gaussfit(lat_fit[gd_fit],mn_fit[gd_fit], g_c, $
      chisq=chisq, nterms=5, estimates=reform(g_coeff[dawn_mn[i-1],*]))
    
    g_coeff[dawn_mn[i],*] = g_c
    g_fit[dawn_mn[i], gd_fit] = g_f
    g_chi[dawn_mn[i]] = chisq
    
    loadct,39,/silent
    plot, lat_fit[gd_fit],mn_fit[gd_fit]
    oplot, lat_fit[gd_fit],g_f, color = 55
  endfor
  stop
  
  ;
  ; what about starting fitting around the night-side?

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
fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010151622.idl"
fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010151328.idl"

fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"

;identify the roi using gauss_smooth and pick the pixels in the 95th percentile
; sun needs to be remobed


im = image_bin_ll(fn, lon_res=5)
image_plot, im, xsize=900, ysize=900, win=2
imf = image_fit_ll(im)

end