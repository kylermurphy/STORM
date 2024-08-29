function image_bin_km, $ 
  fn, $ ; file to load
  lat_res = lat_res, $ ; km resolution in the latitude direction
  lon_res = lon_res, $ ; km resolution in the longitude direction
  lat_min = lat_min, $ ; minimum latitude for grid
  hgt = hgt, $ ; height of the aurora
  im_min = im_min, $ ; min intensity to plot
  im_max = im_max, $ ; max intensity to plot
  ns_scl = ns_scl, $ ; scale data to the night side
  im_plot = im_plot, $ ; plot the image
  clog = clog, $ ; log before plotting
  save_png = save_png, $ ; output png
  out_dir = out_dir ; 
  
  
  if keyword_set(lat_res) then lat_res=lat_res else lat_res=100.
  if keyword_set(lon_res) then lon_res=lon_res else lon_res=100.
  if keyword_set(lat_min) then lat_min=lat_min else lat_min=45.
  if keyword_set(hgt) then hgt=hgt else hgt=110
  if keyword_set(im_plot) then im_plot=1 else im_plot=0 
  if keyword_set(clog) then clog=1 else clog=0
  if keyword_set(save_png) then save_png=1 else save_png=0
  if keyword_set(out_dir) then out_dir=out_dir else out_dir='D:\data\IMAGE_FUV\plots\'

  
  if keyword_set(wf) then wf=wf else wf=0.05
  if keyword_set(cw) then cw=cw else cw=0.03
  
  
  fixplot
  ct = 8
  ct = 56
  ; radius of earth
  re = 6378.14
  h_map = re + hgt
  
  ;open file
  restore, fn
  td = time_double(imageinfo.epoch, /epoch)
  ts = time_string(td)
  
  mlt_mid = min(abs(imageinfo.mlt))
  mlon_mid = imageinfo.mlon[!C]
  glon_mid = imageinfo.glon[!C]
  
  mlt_arr = imageinfo.mlt
  bd = where(mlt_arr lt 0,c)
  if c gt 0 then mlt_arr[bd] = !values.f_nan
  
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
  ; convert longitude array to 0-360
  ; and rotate so sun is top
  bd = where(im_lon lt -1000, bd_c)
  if bd_c gt 1 then im_lon[bd] = !values.f_nan
  bd = where(im_lon ge -180 and im_lon lt 0, bd_c)
  if bd_c gt 1 then im_lon[bd] = 360+im_lon[bd]
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
  mlt_pix = list()
  
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
      
      if lon_c lt 1 then im_val = !values.f_nan else im_val = total(lon_mask*lat_mask*im,/nan)
      im_arr[j] = im_val
      
      im_flat.Add, im_val
      lat_vert.Add, lat_v
      lon_vert.Add, lon_v
      mlt_pix.Add, mean(lon_mask*lat_mask*mlt_arr,/nan)
    endfor
  endfor
  
  
  im_flat = im_flat.ToArray()
  lon_vert = lon_vert.ToArray()
  lat_vert = lat_vert.ToArray()
  mlt_pix = mlt_pix.ToArray()
  
  if size(im_flat,/type) eq 0 then return,0
  if max(im_flat) eq 0 then return, 0
  if finite(max(im_flat, min=f_min)) eq 0 or finite(f_min) eq 0 then return, 0
  
  im_sort = im_flat[sort(im_flat,/l64)]
  im_sort = im_sort[where(im_sort gt 0)]
  im_col = im_flat
  
  if keyword_set(im_min) then im_min=im_min else begin
    im_min=im_sort[im_sort.length*0.1]
  endelse
  if keyword_set(im_max) then im_max=im_max else begin
    im_max=im_sort[im_sort.length*0.85]
  endelse

    ; get the image intesnity from the nightside
  ns_dat = where(mlt_pix gt 16 or mlt_pix lt 6, ns_c)
  if ns_c lt 1 then begin
    im_ns = im_flat[ns_dat]
    im_ns = im_ns[sort(im_ns,/l64)]

    ns_min = im_ns[im_ns.length*0.1]
    ns_max = im_ns[im_ns.length*0.9]
  endif else begin
    ns_min = im_min
    ns_max = im_max
  endelse
  
  
  if keyword_set(ns_scl) then begin
    plot_min=ns_min
    plot_max=ns_max
  endif else begin
    plot_min=im_min
    plot_max=im_max
  endelse
  
  im_pc = where(im_flat gt 0 and finite(im_flat) eq 1, im_c)
  im_pc = 1.0*im_c/n_elements(im_flat)
  
  if keyword_set(clog) then $
    im_col = bytscl(alog10(im_col),/nan, min=alog10(plot_min+1), max=alog10(plot_max)) else $
    im_col = bytscl(im_col,/nan, min=plot_min, max=plot_max)

  if im_plot eq 1 then begin
    window, 2, xsize = 900, ysize = 900
    !p.multi  = [0,1,1]
    !x.omargin = [15,15]
    loadct,0
    xyouts, 0.5,0.85, ts, /normal, alignment=0.5, charsize=1.5
    map_set, 90, 0, 180, /azimuthal,/isotropic, limit=[lat_min,0,90,360],/noerase
    ; plot circles for each pixel
    loadct, ct
    
    im_fin = finite(im_flat)
    for i=0L, im_flat.length-1 do begin
      if im_fin[i] eq 0 then continue
      polyfill, lon_vert[i,*], lat_vert[i,*], color = im_col[i]
    endfor
    
    loadct,25,/silent
    map_grid, londel=15, latdel=10, label=1, thick=2, color=42, charsize=1.5, charthick=2.
    if keyword_set(rev_ct) then rev_ct = 1
    ;plot color bar
    p0 = convert_coord(!p.clip[0],!p.clip[1], /device, /to_normal)
    p1 = convert_coord(!p.clip[2],!p.clip[3], /device, /to_normal)
  
    width = (p1[0]-p0[0])*wf
    cwidth = (p1[0]-p0[0])*cw
  
    position = [p1[0]+width,p0[1],p1[0]+width+cwidth,p1[1]]
    colorbar_krm,position,plot_min,plot_max,ct, /vertical, /right, c_title = 'Raylieghs', log=clog, tl = -0.3, rev_ct = rev_ct, ct_file=ct_file
    
    if save_png eq 1 then begin
      makepng,out_dir+time_string(ts, tformat='YYYYMMDD_hhmmss')
    endif
  endif
  
  return, {name:'binned image in km pixels', lat_min:lat_min, $
           lat_res:lat_res, lon_res:lon_res,  $ 
           im:im_flat, lon_ver:lon_vert, lat_vert:lat_vert, mlt_pix:mlt_pix, t:ts, $
           im_max:im_max, im_min:im_min, im_pc:im_pc, ns_min:ns_min, ns_max:ns_max, $ $
           mlt_mid:mlt_mid, mlon_mid:mlon_mid, glon_mid:glon_mid}
end


;main 
;testing
;

fn = 'D:\data\IMAGE_FUV\2001\WIC\016\wic20010160013.idl'
fn = "D:\data\IMAGE_FUV\2001\WIC\016\wic20010161609.idl"
fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"

im = image_bin_km(fn,/im_plot, /ns_scl)
stop


out_f = 'D:\data\IMAGE_FUV\out_dat.txt'
fn = file_search('D:\data\IMAGE_FUV\2001\WIC\*\*.idl')
pc = fltarr(fn.length)
openw,outlun,out_f,/get_lun,/append

for i=7593L, fn.length-1 do begin
  im = image_bin_km(fn[i],clog=1,save_png=1)
  if size(im,/type) ne 8 then continue
  gd = where(im.im gt 0,c)
  pc[i] = 1.0*c/n_elements(im.im)
  
  printf, outlun, fn[i]+', '+string(pc[i],format='(F0.2)')
endfor

close,outlun
free_lun,outlun

end