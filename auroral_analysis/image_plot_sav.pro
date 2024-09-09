pro image_plot_sav, $
      fn, $
      lat_min=lat_min, $
      mag = mag, $ ; use geomagnetic coordinates
      geo = geo, $ ; use geographic coordinates
      mlt = mlt, $ ; use mlt and magnetic latitude
      im_max=im_max, $
      im_min=im_min, $
      ns_scl=ns_scl, $
      clog = clog, $
      ct=ct, $
      win=win, $
      _EXTRA = ex
      
      
   
  if keyword_set(lat_min) then lat_min=lat_min else lat_min=45.
  if keyword_set(mag) then mag = 1 else mag = 0
  if keyword_set(geo) then geo = 1 else geo = 0
  if keyword_set(mlt) then mlt = 1 else mlt = 0
  if keyword_set(clog) then clog=1 else clog=0
  if keyword_set(ct) then ct=ct else ct=56
  if keyword_set(win) then win=win else win=0
  if keyword_set(wf) then wf=wf else wf=0.02
  if keyword_set(cw) then cw=cw else cw=0.03
   
  restore, fn
  im = imageinfo.image
  td = time_double(imageinfo.epoch, /epoch)
  ts = time_string(td)
   
  if mag eq 1 then begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon
    coord = 'geomagnetic'
  endif else if geo eq 1 then begin
    im_lat = imageinfo.glat
    im_lon = imageinfo.glon
    coord = 'geographic'
  endif else if mag eq 1 then begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlt * 15
    coord = 'geomagnetic/mlt (mlt converted to 0-360 degrees mlt*15'
  endif else begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon
    coord = 'geomagnetic'
  endelse
   
  ; convert longitude array to 0-360
  bd = where(im_lon lt -1000, bd_c)
  if bd_c gt 1 then im_lon[bd] = !values.f_nan
  bd = where(im_lon ge -180 and im_lon lt 0, bd_c)
  if bd_c gt 1 then im_lon[bd] = 360+im_lon[bd]
   
  ; get plotting values
  if keyword_set(im_min) then im_min = im_min else im_min = min(im,/nan)
  if keyword_set(im_max) then im_max = im_max else im_max = max(im,/nan)
   
  if keyword_set(ns_scl) then begin
    ns_dat = where(imageinfo.mlt gt 18 or imageinfo.mlt lt 6, g_gs)
    
    ns_int = imageinfo.image[ns_dat]
    ns_int = ns_int[sort(ns_int,/l64)]
    
    im_max = ns_int[ns_int.length*0.9]
    im_min = ns_int[ns_int.length*0.1]
  endif
   
  if keyword_set(clog) then begin
    im_col = bytscl(alog10(im),/nan, min=alog10(im_min+1),max=alog10(im_max))
    im_min = im_min+1
  endif else begin
    im_col = bytscl(im,/nan,min=im_min,max=im_max)
  endelse
  
  ; minimum to plot
  r_min = 90 - lat_min
  p_arr = [-1*r_min,r_min]
  
  ; setup the plot area
  window, win, _EXTRA=ex
  !x.margin=[5,15]
  !y.margin=[5,5]
  loadct, 0, /silent
  plot, p_arr, p_arr, /nodata, /isotropic, $
    xtickf='no_ticks', ytickf='no_ticks', $
    xticks=1, yticks=1, xminor=1, yminor=1
  loadct, ct, /silent     
  
  x = (90-im_lat)*cos(im_lon*!dtor)
  y = (90-im_lat)*sin(im_lon*!dtor)
  
  plots, x, y, color=im_col, psym=sym(1), noclip=0, symsize=0.25
  
  stop    
end


; main
; testing

fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"

image_plot_sav,fn, ns_scl=1

end


