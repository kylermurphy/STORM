function image_bin_ll, $
  fn, $ ; file to load
  lat_res = lat_res, $ ; km resolution in the latitude direction
  lon_res = lon_res, $ ; km resolution in the longitude direction
  colat_min = colat_min, $ ; minimum latitude for grid
  mag = mag, $ ; use geomagnetic coordinates
  geo = geo, $ ; use geographic coordinates
  mlt = mlt, $ ; use mlt and magnetic latitude
  hgt = hgt, $ ; height of the aurora
  im_min = im_min, $ ; min intensity to plot
  im_max = im_max, $ ; max intensity to plot
  ns_scl = ns_scl, $ ; scale data to the night side
  im_plot = im_plot, $ ; plot the image
  clog = clog, $ ; log before plotting
  save_png = save_png, $ ; output png
  out_dir = out_dir ; 
  
  
  if keyword_set(lat_res) then lat_res=lat_res else lat_res=1.
  if keyword_set(lon_res) then lon_res=lon_res else lon_res=5.
  if keyword_set(colat_min) then lat_min=colat_min else lat_min=45.
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
  
  mlt_arr = imageinfo.mlt
  bd = where(mlt_arr lt 0,c)
  if c gt 0 then mlt_arr[bd] = !values.f_nan

  ; fix longitude
  
  mlt_mid = min(abs(imageinfo.mlt))
  mlon_mid = imageinfo.mlon[!C]
  if mlon_mid lt 0 then mlon_mid = 360+mlon_mid
  glon_mid = imageinfo.glon[!C]
  if glon_mid lt 0 then glon_mid = 360+glon_mid
  
  nlat = (lat_min)/lat_res
  lat_arr = findgen(nlat+1)*lat_res
  lat_arr = reverse(90-lat_arr)

  nlon = 360/lon_res
  lon_arr = findgen(nlon+1)*lon_res

  im_arr = fltarr(nlon,nlat)
  mlt_dat = im_arr
  lon_mask =  fltarr(im.dim)
  lat_mask = lon_mask
  
  im_flat = list()
  lat_vert = list()
  lon_vert = list()
  mlt_pix = list()
  
  
  for j=0L, nlon-1 do begin
      lon_vec = [lon_arr[j],lon_arr[j],lon_arr[j+1],lon_arr[j+1],lon_arr[j]]
      
      gd_lon = where(im_lon gt lon_arr[j] and im_lon le lon_arr[j+1], lon_c)
      if lon_c lt 1 then continue
      
      lon_mask[*] = !values.f_nan
      lon_mask[gd_lon] = 1
       
    for i=0L, nlat-1 do begin
      lat_vec = [lat_arr[i],lat_arr[i+1],lat_arr[i+1],lat_arr[i],lat_arr[i]]
      
      gd_lat = where(im_lat gt lat_arr[i] and im_lat le lat_arr[i+1], lat_c)
      if lat_c lt 1 then continue
      
      lat_mask[*] = !values.f_nan
      lat_mask[gd_lat] = 1
      
      im_val = total(lon_mask*lat_mask*im,/nan)
      im_val = mean(lon_mask*lat_mask*im,/nan)
      im_arr[j,i] = im_val
      mlt_dat[j,i] = median(lon_mask*lat_mask*mlt_arr)
      
      im_flat.Add, im_val
      lat_vert.Add, lat_vec
      lon_vert.Add, lon_vec
      mlt_pix.Add, median(lon_mask*lat_mask*mlt_arr)

    endfor
  endfor

  im_flat = im_flat.ToArray()
  mlt_pix = mlt_pix.ToArray()
  
  if size(im_flat,/type) eq 0 then return,0

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
  ns_dat = where((mlt_pix gt 16 or mlt_pix lt 6) $  
                 and finite(im_flat) eq 1, ns_c)
  if ns_c gt 1 then begin
    im_ns = im_flat[ns_dat]
    im_ns = im_ns[sort(im_ns,/l64)]

    ns_min = im_ns[im_ns.length*0.1]
    ns_max = im_ns[im_ns.length*0.95]
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

  return, {name:'lat/lon binned image', coordinate:coord, colat_min:lat_min, $
           lat_res:lat_res, lon_res:lon_res,  $ 
           im:im_arr, lat_arr:lat_arr, lon_arr:lon_arr, mlt_arr:mlt_dat, $
           im_flat:im_flat, lat_vert:lat_vert.ToArray(), $
           lon_vert:lon_vert.ToArray(), mlt_flat:mlt_pix, t:ts, $ 
           im_max:im_max, im_min:im_min, ns_min:ns_min, ns_max:ns_max, $
           mlt_mid:mlt_mid, mlon_mid:mlon_mid, glon_mid:glon_mid, coord:coord}
end


; main

fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"

im = image_bin_ll(fn, colat_min=30)

image_plot,im, xsize=900, ysize=900

end

