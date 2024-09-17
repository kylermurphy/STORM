; a short script to investigate the typical number of
; pixels we have in our region of interest when we
; are able to generate a good image
; a good image is described by the percent of pixels
; that have an associated intensity and non a NaN. 
;
; These images where binned with image_bin_km.pro


pro image_pix_roi, $
  fn, $
  colat_min=colat_min
  
  if keyword_set(colat_min) then colat_min=colat_min else colat_min=30

  lat_min = 90-colat_min

  ; read in preprocessed file
  ; with filenames and percentages
  fl = file_lines(fn)  
  fr = strarr(fl)
  pc = fltarr(fl)
  pn = fltarr(fl)
  
  xcm = pn
  ycm = pn
  ipc = pn
  lat_ctr = pn
  lon_ctr = pn
  dec = pn
  ra = pn
  
  min_ns = pn
  max_ns = pn
  
  openr, inlun, fn, /get_lun
  
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
  
  ; loop through all the files
  ; and find the number of pixels
  ; in the region of interest
  for i=0L, fr.length-1 do begin
    restore,fr[i]
    gd = where(imageinfo.mlat ge lat_min and finite(imageinfo.image) eq 1, gc)
    pn[i] = gc  
    
    gd = where(imageinfo.mlat ge 0 and imageinfo.mlon ge -180 and imageinfo.image gt 0, gc)
    ns = where(imageinfo.mlat ge lat_min and (imageinfo.mlt ge 18 or imageinfo.mlt lt 6))
    
    x = (90-imageinfo.glat[gd])*cos(imageinfo.glon[gd]*!dtor)
    y = (90-imageinfo.glat[gd])*sin(imageinfo.glon[gd]*!dtor)
    
    ;gd = where(finite(x) eq 1 and finite(y) eq 1, gc)
    
    xcm[i] = float(total(x))/x.length
    ycm[i] = float(total(y))/y.length
    ipc[i] = float(gc)/n_elements(imageinfo.image)
    lat_ctr[i] = imageinfo.lat_ctr
    lon_ctr[i] = imageinfo.lon_ctr
    dec[i] = imageinfo.dec
    ra[i] = imageinfo.ra
    
    min_ns[i] = min(imageinfo.image[ns],/nan,max=mm)
    max_ns[i] = mm
    
;    image_plot_sav, fr[i], xsize=400, ysize=400
;    loadct,25,/silent
;    plots, xc[i], ycm[i], psym=sym(1), color=44 
  endfor
  
  r = sqrt(xcm^2.+ycm^2.)
  
  color = bytscl(pc, min=0.2, max=1.)
  
  fixplot
  window, xsize=700, ysize=800
  !p.multi = [0,1,3]
  !p.charsize = 1.5
  !x.omargin = [0,15]
  
  loadct,0,/silent
  plot, min_ns, r, psym=sym(1), symsize=0.2, xtitle='Min Int Night Side', ytitle='R_com'
  loadct,25,/silent
  plots, min_ns,r, color=color,psym=sym(1), symsize=0.2
  
  loadct,0,/silent
  plot, ipc, r, psym=sym(1), symsize=0.2, xtitle='Fraction of Good Pixels', ytitle='R_com'
  loadct,25,/silent
  plots, ipc,r, color=color,psym=sym(1), symsize=0.2
  
  p0 = convert_coord(!p.clip[0],!p.clip[1], /device, /to_normal)
  p1 = convert_coord(!p.clip[2],!p.clip[3], /device, /to_normal) 
  
  loadct,0,/silent
  plot, min_ns, ipc, psym=sym(1), symsize=0.2, xtitle='Min Int Night Side', ytitle='Fraction of Good Pixels'
  loadct,25,/silent
  plots, min_ns, ipc, color=color,psym=sym(1), symsize=0.2
  
  wf = 0.05
  cw = 0.05
  
  width = (p1[0]-p0[0])*wf
  cwidth = (p1[0]-p0[0])*0.01

  position = [p1[0]+width,p0[1],p1[0]+width+cwidth,p1[1]]
  colorbar_krm,position,0.2,1.0,25, $
    /vertical, /right, c_title = 'Fraction of simulated GUVI-HR image filled', $
    log=clog, tl = -0.3, rev_ct = rev_ct, ct_file=ct_file
  stop
  

end


; main

image_pix_roi,'D:\data\IMAGE_FUV\image_fill.txt'

end