;pass a grid, bin on that grid

function image_bin_grid, $
  fn, $ ; file to load
  grid, $ ; grid to bin image into
  sun_rot = sun_rot, $ ; rotate so sun is at the top
  im_rot = im_rot, $ ; rotate the image by an abitrary angle
  mag = mag, $ ; use geomagnetic coordinates
  geo = geo, $ ; use geographic coordinates
  mlt = mlt ; use mlt and magnetic latitude
  
  if keyword_set(mag) then mag = 1 else mag = 0
  if keyword_set(geo) then geo = 1 else geo = 0
  if keyword_set(mlt) then mlt = 1 else mlt = 0
  
  ;open file
  restore, fn
  td = time_double(imageinfo.epoch, /epoch)
  ts = time_string(td)
  
  im = imageinfo.image

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
    sun_rot = 0
  endif else begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon
    coord = 'geomagnetic'
  endelse
  
  bd = where(im_lat lt -90, bd_c)
  if bd_c gt 0 then im_lat[bd] = !values.f_nan
  
  ; convert longitude array to 0-360
  bd = where(im_lon lt -1000, bd_c)
  if bd_c gt 1 then im_lon[bd] = !values.f_nan
  bd = where(im_lon ge -180 and im_lon lt 0, bd_c)
  if bd_c gt 1 then im_lon[bd] = 360+im_lon[bd]

  mlt_arr = imageinfo.mlt
  bd = where(mlt_arr lt 0,c)
  if c gt 0 then mlt_arr[bd] = !values.f_nan
  
  ; get location of magnetic midnight
  mlt_mid = min(abs(imageinfo.mlt))
  mlon_mid = imageinfo.mlon[!C]
  if mlon_mid lt 0 then mlon_mid = 360+mlon_mid
  glon_mid = imageinfo.glon[!C]
  if glon_mid lt 0 then glon_mid = 360+glon_mid
  
  
  ; rotate so that sun is aligned with
  ; the positve y-axis
  ; in my plotting code the +ve x-axis is zero degrees
  ; the +ve y-axis is 90 degrees
  ; when plotted it is then rotated so that zero longitude
  ; is along the -ve y-axis
  
  ; find the location of the sun in longitude
;  gsm=fltarr(1,3)
;  gsm[0,0] = 6738.14
;  dprint,getdebug=gd
;  dprint,setdebug=-10
;  cotrans, gsm, gse, td, /GSM2GSE
;  cotrans, gse, gei, td, /GSE2GEI
;  cotrans, gei, geo, td, /GEI2GEO
;  ;restore previous verbosity
;  dprint,setdebug=gd
;
;  sm_lon = !radeg * atan(geo[*,1],geo[*,0])
;  sm_lat = !radeg * atan(geo[*,2],sqrt(geo[*,0]^2+geo[*,1]^2))
;
;  if coord eq 'geomagnetic' then begin
;    ;convert geo to aacgm
;    aacgmidl_v2
;    t_str = time_struct(td)
;    ret = AACGM_v2_SetDateTime(t_str.YEAR,t_str.MONTH,t_str.DATE, $
;                               t_str.HOUR,t_str.MIN,t_str.SEC)
;    sm_aacgm = cnvcoord_v2(geo[0],geo[1],110)
;    
;    sm_lon = reform(sm_aacgm[1])
;  endif  
;  
;  sm_lon = sm_lon[0]
;  if sm_lon lt 0 then sm_lon = 360+sm_lon
   
  ; place the sun longitude at 90 degrees
  if keyword_set(sun_rot) then begin  
    im_lon = im_lon-mlon_mid-90 
    bd_lon = where(im_lon lt 0, bd_c)
    if bd_c gt 0 then im_lon[bd_lon] = 360+im_lon[bd_lon]
  endif
  
  
  ; rotate the image if needed
  if keyword_set(im_rot) then begin
    im_lon = im_lon+im_rot
    bd_lon = where(im_lon lt 0, bd_c)
    if bd_c gt 0 then im_lon[bd_lon] = 360+im_lon[bd_lon]    
  endif
  
  ; start binning the image
  g_sz = size(grid.lat_up)
  
  im_bin = fltarr(g_sz[1],g_sz[2]-1)
  im_bin[*] = !values.f_nan
  im_mlt = im_bin
  im_cx = im_bin
  im_cy = im_bin
  
  im_flat = list()
  mlt_flat = list()
  lat_vert = list()
  lon_vert = list()
  pix_num = list()
  
  scan_pix = list()
  scan_pxy = list()
  scan_pll = list()
  scan_pol = list()
  scan_lon = list()
  scan_lat = list()
  scan_mlt = list()

  
  ;flatten the image and lat/lon arrays
  ; this will make the array slightly
  ; smaller when looking for what pixels
  ; are within are polygons
  
  gd_im = where(im gt 0 and finite(im_lon) eq 1 and finite(im_lat) eq 1)
  
  f_im = im[gd_im]
  f_lat = im_lat[gd_im]
  f_lon = im_lon[gd_im]
  f_mlt = mlt_arr[gd_im]
  
  f_x = (90-f_lat)*cos(f_lon*!dtor)
  f_y = (90-f_lat)*sin(f_lon*!dtor)
  
  ns_min = min(f_im[where(f_mlt gt 18 or f_mlt lt 6)],max=ns_max)
  
;  window, xsize = 900, ysize = 900, 9
;  fixplot
;  plot, [-45,45],[-45,45],/nodata, /isotropic
;  loadct,25,/silent
  
  pxpc = 0L
  for i=0L, g_sz[1]-1 do begin
    x_u = reform((90-grid.lat_up[i,*])*cos(grid.lon_up[i,*]*!dtor))
    y_u = reform((90-grid.lat_up[i,*])*sin(grid.lon_up[i,*]*!dtor))

    x_d = reform((90-grid.lat_dn[i,*])*cos(grid.lon_dn[i,*]*!dtor))
    y_d = reform((90-grid.lat_dn[i,*])*sin(grid.lon_dn[i,*]*!dtor))
    
    
    scan_x = [x_u,reverse(x_d)]
    scan_y = [y_u,reverse(y_d)]
    
    bd_scan = where(f_x lt min(scan_x) or f_x gt max(scan_x) $
                 or f_y lt min(scan_y) or f_y gt max(scan_y), complement=gd_scan)
        
    scan_pts = pts_inside(f_x[gd_scan], f_y[gd_scan], scan_x, scan_y)
    scan_ind = gd_scan[where(scan_pts eq 1)]
    
    ;plots, [scan_x, scan_x[0]], [scan_y,scan_y[0]], color=143
    ;plots, f_x[scan_ind], f_y[scan_ind], color=250, psym=sym(3), symsize=0.2
    
    scan_pol.Add, transpose([[scan_x, scan_x[0]],[scan_y, scan_y[0]]])
    scan_pix.Add, f_im[scan_ind]
    scan_pxy.Add, transpose([[f_x[scan_ind]],[f_y[scan_ind]]])
    scan_lon.Add, f_lon[scan_ind]
    scan_lat.Add, f_lat[scan_ind]
    scan_mlt.Add, f_mlt[scan_ind]
    
    ;shrink the arrays as this speeds 
    ; things up
    s_im = f_im[scan_ind]
    s_fx = f_x[scan_ind]
    s_fy = f_y[scan_ind]
    s_mlt = f_mlt[scan_ind]
    
    for j=0L, g_sz[2]-2 do begin
      ; define the polygon of the pixel
      ; in cartesian
      x_vert = [x_u[j], x_u[j+1], x_d[j+1], x_d[j], x_u[j]]
      y_vert = [y_u[j], y_u[j+1], y_d[j+1], y_d[j], y_u[j]]
      
      ; in lat/lon
      lat_vec = [grid.lat_up[i,j],grid.lat_up[i,j+1], grid.lat_dn[i,j+1], grid.lat_dn[i,j], grid.lat_up[i,j]]
      lon_vec = [grid.lon_up[i,j],grid.lon_up[i,j+1], grid.lon_dn[i,j+1], grid.lon_dn[i,j], grid.lon_up[i,j]]

      ; calculate which points of the image pixels
      ; are within the polygon
      o_st = where(s_fx lt min(x_vert) or s_fx gt max(x_vert) $
                or s_fy lt min(y_vert) or s_fy gt max(y_vert), complment=i_st)
      
      in_pts = pts_inside(s_fx[i_st], s_fy[i_st], x_vert[0:-2], y_vert[0:-2], index=pts_ind) 
      gd_pts = i_st[where(in_pts eq 1, pix_c)]
      
      ; calculate the center of polygon
      vert_c = poly_centroid(x_vert[0:-2], y_vert[0:-2])
      
      im_cx[i,j] = vert_c.cx
      im_cy[i,j] = vert_c.cy
      
      ;plots,x_vert,y_vert,color=44
      ;plots,vert_c.cx,vert_c.cy, psym=sym(1), symsize=0.25

      
      pix_num.Add, pix_c
      if pix_c lt 1 then continue

      im_val = mean(s_im[gd_pts],/nan)
      ;im_val = min(f_im[gd_pts])*pix_c
      mlt_val = median(s_mlt[gd_pts])

      ;polyfill, x_vert, y_vert, color=bytscl(alog10(im_val),min=alog10(ns_min),max=alog10(ns_max))
      
      im_bin[i,j] = im_val
      im_mlt[i,j] = mlt_val
      
     
      
      im_flat.Add, im_val
      lat_vert.Add, lat_vec
      lon_vert.Add, lon_vec
      mlt_flat.Add, mlt_val
      
      pxpc++
    endfor
  endfor
  
  im_flat = im_flat.ToArray()
  mlt_flat = mlt_flat.ToArray()
  lat_vert = lat_vert.ToArray()
  lon_vert = lon_vert.ToArray()
  pix_num = pix_num.ToArray()
  pxpc = pxpc/float(n_elements(im_bin))
  
  im_sort = im_flat[sort(im_flat,/l64)]
  im_sort = im_sort[where(im_sort gt 0)]
  im_min=im_sort[im_sort.length*0.1]
  im_max=im_sort[im_sort.length*0.85]
  
  ; get the image intesnity from the nightside
  ns_dat = where((mlt_flat gt 18 or mlt_flat lt 6) $
    and finite(im_flat) eq 1, ns_c)
  im_ns = im_flat[ns_dat]
  im_ns = im_ns[sort(im_ns,/l64)]

  ns_min = im_ns[im_ns.length*0.1]
  ns_max = max(im_ns,/nan)
  
  ; convert the0.11 x-y centroid to lat/lon
  cx_lat = 90 - sqrt(im_cx^2. + im_cy^2.)
  cx_lon = atan(im_cx,im_cy)/!dtor
  bd = where(cx_lon lt 0, bc)
  if bc gt 0 then cx_lon[bd] = 360+cx_lon[bd]
  
  
  r_str = {name:'Gridded Image', coordinate:coord, colat_min:grid.colat_min, grid:grid, $
          im:im_bin, mlt_arr:im_mlt, lat_arr:cx_lat, lon_arr:cx_lon, pxpc:pxpc, $
          im_flat:im_flat, lat_vert:lat_vert, lon_vert:lon_vert, mlt_flat:mlt_flat, $
          t:ts, pix_num:pix_num, im_min:im_min, im_max:im_max, ns_min:ns_min, ns_max:ns_max, $
          scan_pol:scan_pol, scan_pix:scan_pix, scan_pxy:scan_pxy, scan_lon:scan_lon, $
          scan_lat:scan_lat, scan_mlt:scan_mlt, $
          mlt_mid:mlt_mid, mlon_mid:mlon_mid, glon_mid:glon_mid, coord:coord} 

  return, r_str
  
end



;main
;
fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"
cmin = 45
pix_sz = 0.26*1E-3

;image_plot_sav,fn, /ns_scl, /clog
guvi_grid = km_grid_guvi(colat_min=cmin, fov_a=pix_sz, pix_a=pix_sz)

;earth_grid = km_grid_earth(100, colat_min=c_min, translate=1)

s1 = systime(/seconds)
Profiler, /SYSTEM & Profiler
guvi_im = image_bin_grid(fn, guvi_grid,/sun_rot)
Profiler, /REPORT
s2 = systime(/seconds)

print,(s2-s1)/60.

image_plot, guvi_im, /no_rot, /clog, xsize=900, ysize=900, win=4
;image_plot_sav, fn, colat_min=cmin, /ns_scl, /clog
;image_plot, guvi_im, /clog, xsize=900, ysize=900, win=1

end


; pix size need to be bigger
; make array to hold number of pixels
