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
  bd_im = where(im le 1 and im gt 10000, im_c)
  if im_c gt 0 then im[bd_im]=!values.f_nan
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
    
    day_px = where(im_str.mlt_arr gt 6 and im_str.mlt_arr lt 18, dc, complement=ns_pix)
    
    sun_arr = im
    sun_min = min(sun_arr[day_px])
    sun_arr[ns_pix] = sun_min
    
    ; find the brightest 5% pixels
    ; 5% b/c we've already cut the array
    ; in half with dayside
    sun_sort = sort(sun_arr, /l64)
    sun_peak = sun_sort[sun_sort.length*0.90:-1]
    
    sun_pix = region_grow(im,sun_peak, /all_neighbors, /nan,stddev_multiplier=1.5)
    
    sun2 = im
    sun2[sun_pix] = max(sun_arr)
    tvscale, sun2,/nointerpolation
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
  
  mlt_fit = fltarr(l_pts)
  mlt_fit[*] = !values.f_nan
  g_coeff = fltarr(l_pts, 5)
  g_coeff[*] = !values.f_nan
  g_chi = fltarr(l_pts)
  g_chi[*] = !values.f_nan
  g_fit = im
  g_fit[*] = !values.f_nan
  
  g_sig = g_coeff
  
  ; first fit at midnight
  mn_fit = reform(im[dawn_mn[0],*])
  gd_fit = where(finite(mn_fit) eq 1)
  g_f = gaussfit(lat_fit[gd_fit],mn_fit[gd_fit], g_c, $
                    chisq=chisq, nterms=5, sigma=ss)
  
  ; fill the arrays
  g_coeff[dawn_mn[0],*] = g_c
  g_fit[dawn_mn[0], gd_fit] = g_f
  g_chi[dawn_mn[0]] = chisq
  g_sig[dawn_mn[0],*] = ss
  
  mlt_fit[dawn_mn[0]] = median(reform(im_str.mlt_arr[dawn_mn[0],*]))
  
  ;repeat the above for cw toward noon
  g_old=g_c
  for i=1L, dawn_mn.length-1 do begin
    mn_fit = reform(im[dawn_mn[i],*])
    mlt_fit[dawn_mn[i]] = median(reform(im_str.mlt_arr[dawn_mn[i],*]))

    if mlt_fit[dawn_mn[i]] gt 6 then begin
      b_dat = where(mn_fit gt 1.1*pk or mn_fit lt 0.75*mn, bc)
      if bc gt 0 then mn_fit[b_dat] = !values.f_nan
    endif

    gd_fit = where(finite(mn_fit) eq 1)
    if max(lat_fit[gd_fit],min=mm,/nan)-mm lt 20 then continue
    g_f = gaussfit(lat_fit[gd_fit],mn_fit[gd_fit], g_c, $
      chisq=chisq, nterms=5, estimates=g_old, status=stat, sigma=ss)

    if stat ne 0 then continue
    if g_c[0] lt 0 then continue
    if g_c[2] gt 10 then continue

    g_coeff[dawn_mn[i],*] = g_c
    g_fit[dawn_mn[i], gd_fit] = g_f
    g_chi[dawn_mn[i]] = chisq
    g_sig[dawn_mn[i],*] = ss
    
;    loadct,39,/silent
;    plot, lat_fit[gd_fit],mn_fit[gd_fit]
;    oplot, lat_fit[gd_fit], g_f, color = 44
;    print, g_c

    gd_pk = where(lat_fit gt g_c[1]-2*g_c[2] and lat_fit lt g_c[1]+2*g_c[2], pk_c)
    if pk_c gt 3 then pk = max(mn_fit[gd_pk],/nan) else pk=max(mn_fit,/nan) 
    if pk_c gt 3 then mn = min(mn_fit[gd_pk],/nan) else min=min(mn_fit,/nan)
    
    g_old = g_c

  endfor
  
  ;repeat the above for ccw toward noon
  pk=-1
  mn=-1
  dk_pts = dusk_mn.length
  g_old = g_coeff[dawn_mn[0],*]
  
  for j=0L, dk_pts-1 do begin
    i = dk_pts-j-1
    mn_fit = reform(im[dusk_mn[i],*])
    mlt_fit[dusk_mn[i]] = median(reform(im_str.mlt_arr[dusk_mn[i],*]))
    
    if mlt_fit[dusk_mn[i]] lt 20 then begin
      b_dat = where(mn_fit gt 1.1*pk or mn_fit lt mn*0.75, bc)
      if bc gt 0 then mn_fit[b_dat] = !values.f_nan
    endif

    gd_fit = where(finite(mn_fit) eq 1, ll)
    if ll le 6 then continue
    if max(lat_fit[gd_fit],min=mm,/nan)-mm lt 20 then continue
    
    g_f = gaussfit(lat_fit[gd_fit],mn_fit[gd_fit], g_c, $
      chisq=chisq, nterms=5, estimates=g_old, status=stat, sigma=ss)
    
    if stat ne 0 then continue
    if g_c[0] lt 0 then continue
    
    g_coeff[dusk_mn[i],*] = g_c
    g_fit[dusk_mn[i], gd_fit] = g_f
    g_chi[dusk_mn[i]] = chisq
    g_sig[dusk_mn[i],* ] = ss
    
;    loadct,39,/silent
;    plot, lat_fit[gd_fit],mn_fit[gd_fit]
;    oplot, lat_fit[gd_fit], g_f, color = 44
;    print, ss
;    stop
    
    gd_pk = where(lat_fit gt g_c[1]-2*g_c[2] and lat_fit lt g_c[1]+2*g_c[2], pk_c)
    if pk_c gt 3 then pk = max(mn_fit[gd_pk],/nan) else pk=max(mn_fit,/nan) 
    if pk_c gt 3 then mn = min(mn_fit[gd_pk],/nan) else min=min(mn_fit,/nan)
    
    g_old = g_c
    
  endfor



  lon_plot = lon_fit
  lat_plot = g_coeff[*,1]
    
  r_str = create_struct('lon_fit',lon_fit, 'lat_fit',lat_fit, $
           'g_coeff',g_coeff, 'g_fit',g_fit, 'g_chi',g_chi, name='Fit Oval')
            
  rot_t = 90
  r = 90-lat_plot
  theta = lon_plot-im_str.mlon_mid
  
  x = r*cos((theta-rot_t)*!dtor)
  y = r*sin((theta-rot_t)*!dtor) 

  gd = where(finite(x) eq 1)

  e_fit = mpfitellipse(x[gd],y[gd], /quiet, /tilt, status= e_stat)

  phi = findgen(180)*2*!dtor

  xm = e_fit[2] + e_fit[0]*cos(phi)*cos(e_fit[4]) + e_fit[1]*sin(phi)*sin(e_fit[4])
  ym = e_fit[3] - e_fit[0]*cos(phi)*sin(e_fit[4]) + e_fit[1]*sin(phi)*cos(e_fit[4])
  
  im_str = create_struct(im_str, 'oval_fit', r_str, name='Image Data')
  
  return, im_str
end

; Main
; 
; 

fn = 'D:\data\IMAGE_FUV\2001\WIC\016\wic20010161609.idl'
fn = "D:\data\IMAGE_FUV\2001\WIC\001\wic20010010449.idl"
;fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010151622.idl"
;fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010151328.idl"

fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"

;identify the roi using gauss_smooth and pick the pixels in the 95th percentile
; sun needs to be remobed

sr = 0
im = image_bin_ll(fn, lon_res=5)
im_fit = image_fit_ll(im, sun_rm=sr)
image_plot, im_fit, xsize=900, ysize=900, win=2

; fit an ellipse and plot on top of the image
rot_t = 90
r = 90-im_fit.oval_fit. g_coeff[*,1]
theta = im_fit.oval_fit.lon_fit-im_fit.mlon_mid

x = r*cos((theta-rot_t)*!dtor)
y = r*sin((theta-rot_t)*!dtor)

gd = where(finite(x) eq 1)

e_fit = mpfitellipse(x[gd],y[gd], /quiet, /tilt, status= e_stat)

phi = findgen(180)*2*!dtor

xm = e_fit[2] + e_fit[0]*cos(phi)*cos(e_fit[4]) + e_fit[1]*sin(phi)*sin(e_fit[4])
ym = e_fit[3] - e_fit[0]*cos(phi)*sin(e_fit[4]) + e_fit[1]*sin(phi)*cos(e_fit[4])

loadct,1,/silent
plots, x,y, psym=sym(1), color=175
oplot, xm,ym, color=175, thick=1.5

end