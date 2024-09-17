pro process_image, $
  fn, $
  r_max = r_max, $
  ns_min = ns_min, $
  lat_min = lat_min, $
  out_dir = out_dir, $
  _EXTRA=ex
  
  
  if keyword_set(r_max) then r_max=r_max else r_max=30
  if keyword_set(ns_min) then ns_min=ns_min else ns_min=300
  if keyword_set(lat_min) then lat_min=lat_min else lat_min=60
  if keyword_set(out_dir) then out_dir=out_dir else out_dir='D:\data\IMAGE_FUV\processed\'
  
  
  ; create a grid for a guvi like insturment
  guvi_grid = km_grid_guvi(_EXTRA=ex)
  
  t0 = systime(/seconds)
  ;loop through files and bin the images
  for i=1l, fn.length-1 do begin
    restore,fn[i]
    td = time_double(imageinfo.epoch, /epoch)
    tstr = time_struct(td)
    ns = where(imageinfo.mlat ge lat_min and (imageinfo.mlt ge 18 or imageinfo.mlt lt 6))
    gd = where(imageinfo.mlat ge 0 and imageinfo.mlon ge -180 and imageinfo.image gt 0, gc)
    
    x = (90-imageinfo.glat[gd])*cos(imageinfo.glon[gd]*!dtor)
    y = (90-imageinfo.glat[gd])*sin(imageinfo.glon[gd]*!dtor)
    
    xcm = float(total(x))/x.length
    ycm = float(total(y))/y.length
    rcm = sqrt(xcm^2.+ycm^2.)
    ipc = gc/256.^2.
    
    int_ns = min(imageinfo.image[ns],/nan)
    
    if rcm gt r_max then continue
    if int_ns lt ns_min then continue
    if ipc lt 0.5 then continue
    
    ; if we have a good file then start binning it
    guvi_im = image_bin_grid(fn[i], guvi_grid,/sun_rot)
    ll_im = image_bin_ll(fn[i], _EXTRA=ex)

    sav_dir = out_dir+string(tstr.year,format='(I04)')+'\'+string(tstr.month,format='(I02)')
    
    sav_src = file_search(sav_dir, count=sc)
    
    if sc eq 0 then FILE_MKDIR, sav_dir 
    
    out_file = strsplit(fn[i],'\',/extract)
    out_file = 'pro'+out_file[-1]
    save, guvi_im, ll_im, filename=sav_dir+'\'+out_file, /compress
    
    if i mod 100 eq 0 then begin
      tn = systime(/seconds)
      print, 'I:'+strtrim(i,2)+', ellapsed: '+string((tn-t0)/60.,format='(F0.2)')
    endif
  endfor
  
    
end

; MAIN

fn = file_search('D:\data\IMAGE_FUV\2001\WIC\*\*.idl', count=fc)

ft = 'D:\data\IMAGE_FUV\image_fill.txt'
fl = file_lines(ft)
fr = strarr(fl)
pc = fltarr(fl)
openr, inlun, ft, /get_lun

i=0L
line = ' '
while not eof(inlun) do begin
  readf,inlun,line
  dat = strsplit(line,',', /extract)

  fr[i] = dat[0]
  pc[i] = dat[1]
  i++
endwhile

close, inlun
free_lun, inlun

gd = where(pc gt 0.8)
fn = fr[gd]

cmin = 45
pix_sz = 0.26*1E-3
lat_res = 1.0
lon_res = 2.0
out_dir = 'C:\data\IMAGE_FUV\processed\'

process_image,fn, out_dir=out_dir, $
  colat_min=colat_min, fov_a=pix_sz, pix_a=pix_sz, lat_res=lat_res, lon_res=lon_res

end