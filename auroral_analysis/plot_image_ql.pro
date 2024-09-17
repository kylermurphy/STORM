;+
; :Author: krmurph1
;-
;+
; :Description:
;    Procedure to plot quick looks of IMAGE WIC and SI data. 
;
; :Params:
;    fn - filename of the idl savefile *.idl
;
; :Keywords:
;    ns_scl - scale the data based on the brightness of nightside aurora
;    l_qt - lower qauntile for scaling the data when using ns_scl, decimal number
;    u_qt - upper qauntile for scaling the data when using ns_scl, decimal number 
;    lat_min - minimum latitude to plot
;    lat_shift - shift the latitude of all pixels in the image
;    ct - color table to use
;    file_ct - color table file to use
;    rev_ct - reverse the color table
;    clog - plot the log intensity of the image
;    i_max - image intensity max
;    i_min - image intensity min
;    wf - shift for placement of color bar
;    cw - width of color bar
;    save_png - save the image as pn
;    ps - create a ps image
;    out_dir - directory to save the image in 
;    win - window for plottiing
;-
pro plot_image_ql, $
  fn, $ ; file
  ns_scl = ns_scl, $ ; scale color to night side data
  l_qt = l_qt, $ ; lower quantile for data scaling
  u_qt = u_qt, $ ; upper quantile for data scaling
  lat_min = lat_min, $ ; min lat to plot
  lat_shift = lat_shift, $ ; shift latitude of aurora
  ct = ct, $ ; auroral ct
  file_ct = file_ct, $ ; auroral ct file
  rev_ct = rev_ct, $ ; reverse the color table for plotting
  clog = clog, $ ; log color scale and image
  i_max = i_max, $ ; max intensity
  i_min = i_min, $ ; min intesnity
  wf = wf, $ ; shift for color bar
  cw = cw, $ ; width of color bar
  save_png = save_png, $ ; save image as png
  ps = ps, $ ; output ps
  out_dir = out_dir, $ ; output directory
  win = win ; window to plot

  if keyword_set(ns_scl) then ns_scl = ns_scl else ns_scl = 1
  if keyword_set(l_qt) then l_qt = l_qt else l_qt = 0.1
  if keyword_set(u_qt) then u_qt = u_qt else u_qt = 0.95
  if keyword_set(lat_min) then lat_min = lat_min else lat_min = 50
  if keyword_set(lat_shift) then lat_shift = lat_shift else lat_shift = 0
  if keyword_set(ct) then ct = ct else ct = 8
  if keyword_set(file_ct) then ct_file = file_ct else ct_file = 'E:\GoogleDrive\Work\idl\colortable\mycolorbars.tbl'
  if keyword_set(clog) then clog = 1 else clog = 0
  if keyword_set(wf) then wf = wf else wf = 0.05
  if keyword_set(cw) then cw = cw else cw = 0.03
  if keyword_set(save_png) then save_png = 1 else save_png = 0
  if keyword_set(ps) then ps = 1 else ps = 0
  if keyword_set(out_dir) then out_dir = out_dir else out_dir = 'C:\Users\krmurph1\Physics\output\IMAGE\'
  if keyword_set(win) then win = win else win = 1
  
  fixplot
  aacgmidl_v2
  ;restore file
  restore, fn
  
  ;get instrument
  inst = strmid(fn,17,3,/reverse_offset)
  
  ;get time
  yr  = float(strmid(fn,14,4,/reverse_offset))
  doy = float(strmid(fn,10,3,/reverse_offset))
  hh  = float(strmid(fn,7,2,/reverse_offset))
  mm  = float(strmid(fn,5,2,/reverse_offset))
  
  doy_to_month_date,yr,doy,mt,dt
  
  ;create title for figure
  t_str = string(yr,format='(I04)')
  t_str = t_str+'-'+string(mt,format='(I02)')
  t_str = t_str+'-'+string(dt,format='(I02)')
  t_str = t_str+'/'+string(hh,format='(I02)')
  t_str = t_str+':'+string(mm,format='(I02)')+':00'
  
  ;create output file name
  ofn = inst+'_'+time_string(time_double(t_str),format=2)
  
  ;get mlt of of zero longitude to rotate noon to
  ; top of figure
  ;im_mlt = MLTConvert_v2(yr,mt,dt,hh,mm,0,0)
  mlt_min = min(imageinfo.mlt, min_id)
  im_mlt = imageinfo.mlon[min_id]
  
  im  = imageinfo.image
  lat = abs(imageinfo.mlat + lat_shift)
  lon = imageinfo.mlon
  imlt= imageinfo.mlt
  
  if ns_scl eq 1 then begin
    gd = where((imlt ge 18 or imlt le 6) and lat ge lat_min and im gt 0)
  
    idat = im[gd]
    idat = idat[sort(idat,/l64)]
  
    i_min = idat[n_elements(idat)*l_qt]
    i_max = idat[n_elements(idat)*u_qt]
  
  endif else begin
    if keyword_set(i_max) then i_max = i_max else i_max = max(im,/nan)
    if keyword_set(i_min) then i_min = i_min else i_min = min(im[where(im gt 0)],/nan)
  endelse
  
  ;find values above/below thresholds
  bd = where(im le i_min)
  
  im[where(im le i_min)] = i_min
  im[where(im ge i_max)] = i_max
  
  if clog eq 1 then im = alog10(im)
  
  col_arr = bytscl(im)
  col_arr[bd] = !values.f_nan
  
  window, win, xsize = 850, ysize = 500
  !p.multi  = [0,2,1]
  !x.omargin = [15,15]
  
  if ps eq 1 then begin
    ps_s = PXPERFECT( )
    !p.font = 0
    !p.charsize = 1.5
    wdelete, win
    set_plot,'ps'
    device, _EXTRA=ps_s, $
      /color, $
      filename = out_dir+ofn+'.eps', $
      /ENCAPSULATED, SET_FONT='Courier', /TT_FONT
    pcol = 255
  endif
  
  xyouts, 0.5, 0.85,inst+' '+t_str, /normal, alignment=0.5, charsize=1.5
  
  ; setup the plotting area
  ; im_mlt rotates the map so that noon is at the top
  loadct,0
  map_set, 90, 0,-1*im_mlt, /azimuthal,/isotropic, limit=[lat_min,0,90,360],/noerase
  ; plot circles for each pixel
  loadct, ct, file = ct_file
  if keyword_set(rev_ct) then reverse_ct
  for i=0L, n_elements(im[0,*])-1 do begin
    for j=0L, n_elements(im[*,0])-1 do begin
      if finite(col_arr[i,j]) ne 1 then continue
  
      y = lat[i,j]
      x = lon[i,j]
  
      plots, x,y, psym=sym(1), color = col_arr[i,j],noclip=0
    endfor
  endfor
  ; add grid to map
  loadct,0,/silent
  map_grid, londel=15, latdel=10,color=28
  
  
  loadct,0,/silent
  map_set, 90,0,-1*im_mlt, /azimuthal,/isotropic, limit=[lat_min,0,90,360], /noerase
  loadct, ct, file = ct_file
  if keyword_set(rev_ct) then reverse_ct
  for i=0L, n_elements(im[0,*])-2 do begin
    for j=0L, n_elements(im[*,0])-2 do begin
  
      if finite(col_arr[i,j]) ne 1 then continue
  
      y = [lat[i,j],lat[i+1,j],lat[i+1,j+1],lat[i,j+1],lat[i,j]]
      x = [lon[i,j],lon[i+1,j],lon[i+1,j+1],lon[i,j+1],lon[i,j]]
  
      polyfill, x,y, color = col_arr[i,j], noclip=1
    endfor
  endfor
  
  loadct,0,/silent
  map_grid, londel=15, latdel=10,color=28
  
  ;plot color bar
  p0 = convert_coord(!p.clip[0],!p.clip[1], /device, /to_normal)
  p1 = convert_coord(!p.clip[2],!p.clip[3], /device, /to_normal)
  
  width = (p1[0]-p0[0])*wf
  cwidth = (p1[0]-p0[0])*cw
  
  position = [p1[0]+width,p0[1],p1[0]+width+cwidth,p1[1]]
  if keyword_set(rev_ct) then rev_ct = 1
  colorbar_krm,position,i_min,i_max,ct, /vertical, /right, c_title = 'Raylieghs', log=clog, tl = -0.3, rev_ct = rev_ct, ct_file=ct_file
  
  
  if ps eq 0 and save_png eq 1 then begin
    makepng,out_dir+ofn
  endif
  if ps eq 1 then begin
    device,/close
    set_plot,'win'
  endif

end

;main
fn = "D:\data\IMAGE_FUV\2001\WIC\016\wic20010161609.idl"
fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"
plot_image_ql, fn


end

